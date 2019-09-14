{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Network policies and configuration for OpenFlow controllers.

module Sdn.Policy.OpenFlow
    ( Policy (..)
    , Configuration
    , PolicyCoord

    , policyXid
    , policySwitchId
    , genPolicy

    , ConflictReport (..)
    , crAcceptedPercent
    , TimeLimitMillis
    , averageConflictReports
    , withAcceptancePercent
    , emulateConflicts
    , simpleTimestampsSeq
    ) where

import           Control.Lens                 (at, has, ix, makeLenses, non, to)
import           Data.Coerce                  (coerce)
import           Data.Default                 (Default (..))
import           Data.Hashable                (Hashable (..))
import           Data.Kind                    (Type)
import qualified Data.List                    as L
import qualified Data.Map                     as M
import           Data.MessagePack             (MessagePack (..))
import           Data.Reflection              (Reifies (..))
import qualified Data.Set                     as S
import qualified Data.Text.Buildable
import           Data.Time.Units              (Microsecond, Millisecond, convertUnit,
                                               toMicroseconds)
import           Formatting                   (bprint, build, sformat, shown, (%))
import qualified Network.Data.OpenFlow        as OF
import           Test.QuickCheck              (Gen, arbitrary, choose)
import           Universum

import           Sdn.Base
import           Sdn.Extra.Util               (type (%), binaryFromObject, binaryToObject,
                                               decompose, listF, pairF, presence)
import           Sdn.Policy.PseudoConflicting

instance Hashable OF.Action
instance Hashable OF.PseudoPort
instance Hashable Microsecond where
  hashWithSalt s = hashWithSalt s . toMicroseconds

type PolicyCoord = (OF.TransactionID, OF.SwitchID)

data Policy = Policy
    { policyCoord      :: PolicyCoord
    , policyAction     :: [OF.Action]
    , policyCreatorPid :: ProcessId ProposerTag
    , policyTimestamp  :: Microsecond
    } deriving (Eq, Ord, Show, Generic)

policyXid :: Policy -> OF.TransactionID
policyXid = fst . policyCoord

policySwitchId :: Policy -> OF.SwitchID
policySwitchId = snd . policyCoord

instance Hashable Policy

instance Buildable Policy where
    build = bprint shown

instance Conflict Policy Policy where
    conflictReason p1@(Policy coord1 action1 _ _) p2@(Policy coord2 action2 _ _) =
        let pack action = M.fromList $ map (\a -> (OF.actionToType a, a)) action
            [at1, at2] = map pack [action1, action2]
            actionsOfSameTypeAreSame = M.intersectionWith (==) at1 at2
        in  if coord1 == coord2 && and actionsOfSameTypeAreSame
            then Left $ sformat ("Openflow policies "%shown%" & "%shown%" conflict!")
                        p1 p2
            else Right ()

instance MessagePack Policy where
    toObject (Policy xid action pid time) = binaryToObject (xid, action, pid, time)
    fromObject o = do
        (xid, action, pid, time) <- binaryFromObject o
        return (Policy xid action pid time)


data Configuration = Configuration
    { _configEntry    :: M.Map PolicyCoord $ S.Set Policy
    , _configRejected :: S.Set Policy
    } deriving (Eq, Show, Generic)

makeLenses ''Configuration

instance Buildable Configuration where
    build Configuration{..} =
        bprint
            (  "Accepted: "%listF "\n  " (pairF ("for #"%pairF (build%"-"%build)%": "%listF ", " build))%
            "\n Rejected: "%listF ", " build)
          _configEntry _configRejected

instance Default Configuration where
    def = Configuration mempty mempty

instance MessagePack Configuration

instance Conflict (Acceptance Policy) Configuration where
    conflictReason (Rejected _) _ = Right ()
    conflictReason (Accepted policy) Configuration{..} =
        case M.lookup (policyCoord policy) _configEntry of
            Nothing          -> Right ()
            Just oldPolicies -> mapM_ (conflictReason policy) oldPolicies

instance Conflict Configuration (Acceptance Policy) where
    conflictReason = flip conflictReason

instance Conflict Configuration Configuration where
    conflictReason c1 c2 =
        mapM_ (conflictReason c1 . Accepted) (S.unions . toList $ _configEntry c2)

addCommandUnsafe :: Acceptance Policy -> Configuration -> Configuration
addCommandUnsafe (Rejected policy) = configRejected . at policy . presence .~ True
addCommandUnsafe (Accepted policy) =
    configEntry . at (policyCoord policy) . non mempty . at policy . presence .~ True

instance CStruct Configuration where
    type Cmd Configuration = Acceptance Policy
    addCommand = checkingAgreement addCommandUnsafe
    glb = checkingAgreement $ \c1 c2 -> Configuration
        { _configEntry = M.unionWith (S.union) (_configEntry c1) (_configEntry c2)
        , _configRejected = S.union (_configRejected c1) (_configRejected c2)
        }
    lub c1 c2 =
        Configuration
        { _configEntry = M.intersectionWith (S.intersection) (_configEntry c1) (_configEntry c2)
        , _configRejected = S.intersection (_configRejected c1) (_configRejected c2)
        }
    extends c1 c2 = and
        [ M.isSubmapOfBy (S.isSubsetOf) (_configEntry c2) (_configEntry c1)
        , S.isSubsetOf (_configRejected c2) (_configRejected c1)
        ]
    difference c1 c2 = mconcat
        [ map Accepted . toList $ fold (_configEntry c1) `S.difference` fold (_configEntry c2)
        , map Rejected . toList $ _configRejected c1 `S.difference` _configRejected c2
        ]


instance AtCmd Configuration where
    atCmd raw@Policy{..} = to getter
      where
        getter config =
            if | (configEntry . ix policyCoord . ix raw) `has` config -> Just AcceptedT
               | (configRejected . ix raw) `has` config -> Just RejectedT
               | otherwise -> Nothing

instance MayHaveProposerId Policy where
    cmdProposerId = Just . policyCreatorPid

instance PracticalCStruct Configuration

-- * Other instances

instance (f1 ~ (k % n), KnownNat k, KnownNat n) =>
         Conflict (PseudoConflicting (k % n) Policy) (PseudoConflicting f1 Policy) where
  conflictReason (PseudoConflicting a) (PseudoConflicting b) =
      -- Note: reflexivity and symmetry should still hold
      let diff = fromIntegral $ hash' (hash a) * hash b - hash' (hash b) * hash a
      in if and
          [ abs diff `mod` reflect (Proxy @n) > reflect (Proxy @k)
          -- , sort [policyCreatorPid a, policyCreatorPid b] ==
          --    [ProcessId 1, ProcessId 2]
          ]
          then Left $ sformat ("Conflicting policies (fake): "%build%" vs "%build) a b
          else pass
    where
      -- @hash @Int@ is the same as @id@, thus applying something a bit smarter
      hash' k = k * 237 + 4315431

-- | Period of time which allowed for conflicts to happen.
--
-- In real life controllers do not propose policies which conflict with already
-- installed ones, this datatype helps to simulate that.
data TimeLimitMillis :: Nat -> Type -> Type

instance ( f1 ~ TimeLimitMillis t f, KnownNat t
         , Conflict (PseudoConflicting f Policy) (PseudoConflicting f Policy)
         ) =>
         Conflict (PseudoConflicting (TimeLimitMillis t f) Policy) (PseudoConflicting f1 Policy) where
  conflictReason (PseudoConflicting a) (PseudoConflicting b) =
     let time = fromInteger @Millisecond $ natVal (Proxy @t)
     in if
           -- trace @Text (show (policyTimestamp a) <> " - " <> show (policyTimestamp b) <>
           --               " = " <> show (policyTimestamp a - policyTimestamp b) <>
           --               " vs " <> show time) $
           abs (policyTimestamp a - policyTimestamp b) < convertUnit time
          then (conflictReason `on` (PseudoConflicting @f)) a b
          else pass

instance ( Conflict (PseudoConflicting f Policy) (PseudoConflicting f Policy)
         , f ~ f1
         ) =>
         Conflict (PseudoConflicting f Policy)
                  (PseudoConflicting f1 Configuration) where
    conflictReason p (PseudoConflicting c2) =
        mapM_ (conflictReason p . Accepted)
              (fmap PseudoConflicting . toList . S.unions . toList $ _configEntry c2)

instance ( Conflict (PseudoConflicting f1 Policy) (PseudoConflicting f1 Policy)
         , f ~ f1
         ) =>
         Conflict (PseudoConflicting f Configuration)
                  (PseudoConflicting f1 Policy) where
    conflictReason = flip conflictReason

instance ( Conflict (PseudoConflicting f1 Policy) (PseudoConflicting f1 Policy)
         , f ~ f1
         ) =>
         Conflict (PseudoConflicting f Configuration)
                  (PseudoConflicting f1 Configuration) where
    conflictReason c1 (PseudoConflicting c2) =
        mapM_ (conflictReason c1 . Accepted)
              (fmap PseudoConflicting . toList . S.unions . toList $ _configEntry c2)

genPolicy :: ProcessId p -> Microsecond -> Gen Policy
genPolicy pid timestamp = do
    xid <- arbitrary
    sid <- choose (0, 100)
    return Policy
        { policyCoord = (xid, sid)
        , policyAction = OF.actionSequenceToList OF.flood
        , policyCreatorPid = coerce pid
        , policyTimestamp = timestamp
        }

-- * Stuff

data ConflictReport a = ConflictReport
  { crTotal    :: a
  , crAccepted :: a
  , crRejected :: a
  } deriving (Show, Functor)

crAcceptedPercent :: ConflictReport Int -> Double
crAcceptedPercent ConflictReport{..} =
  let frac = fromIntegral crAccepted / fromIntegral crTotal
  in fromIntegral (round @Double @Int $ frac * 10000) / 100

mergeConflictReports :: (a -> a -> a) -> ConflictReport a -> ConflictReport a -> ConflictReport a
mergeConflictReports f r1 r2 =
  ConflictReport
  { crTotal = f (crTotal r1) (crTotal r2)
  , crAccepted = f (crAccepted r1) (crAccepted r2)
  , crRejected = f (crRejected r1) (crRejected r2)
  }

averageConflictReports :: [ConflictReport Int] -> ConflictReport Int
averageConflictReports reports =
  fmap (`div` length reports) $
  foldr1 (mergeConflictReports (+)) reports

withAcceptancePercent :: ConflictReport Int -> (ConflictReport Int, Double)
withAcceptancePercent report = (report, crAcceptedPercent report)

emulateConflicts
  :: forall frac.
      (Conflict (PseudoConflicting frac Policy) (PseudoConflicting frac Policy))
  => Int -> [Millisecond] -> Gen (ConflictReport Int)
emulateConflicts policiesNum (map convertUnit -> timesSeq) = do
  let genPolicyConflicting time = PseudoConflicting <$> genPolicy (ProcessId 0) time
  policies <- forM (take policiesNum timesSeq) genPolicyConflicting
  let initConfig = def @(PseudoConflicting frac Configuration)
  let (accs, _) = acceptOrRejectCommands policies initConfig
  let (oks, rejected) = L.partition ((== AcceptedT) . fst . decompose) accs
  return . fmap length $ ConflictReport
    { crTotal = accs
    , crAccepted = oks
    , crRejected = rejected
    }

simpleTimestampsSeq :: Int -> Millisecond -> [Millisecond]
simpleTimestampsSeq controllersNum proposalDelay =
  [0, proposalDelay ..] <* replicate controllersNum ()

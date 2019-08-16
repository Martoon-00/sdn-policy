{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Network policies and configuration for OpenFlow controllers.

module Sdn.Policy.OpenFlow
    ( Policy (..)
    , Configuration
    , PolicyCoord

    , policyXid
    , policySwitchId
    ) where

import           Control.Lens          (at, has, ix, makeLenses, non, to)
import           Data.Default          (Default (..))
import           Data.Hashable         (Hashable (..))
import qualified Data.Map              as M
import           Data.MessagePack      (MessagePack (..))
import qualified Data.Set              as S
import qualified Data.Text.Buildable
import           Formatting            (bprint, build, sformat, shown, (%))
import qualified Network.Data.OpenFlow as OF
import           Universum

import           Sdn.Base
import           Sdn.Extra.Util        (binaryFromObject, binaryToObject, listF, pairF, presence)

instance Hashable OF.Action
instance Hashable OF.PseudoPort

type PolicyCoord = (OF.TransactionID, OF.SwitchID)

data Policy = Policy
    { policyCoord      :: PolicyCoord
    , policyAction     :: [OF.Action]
    , policyCreatorPid :: ProcessId ProposerTag
    } deriving (Eq, Ord, Show, Generic)

policyXid :: Policy -> OF.TransactionID
policyXid = fst . policyCoord

policySwitchId :: Policy -> OF.SwitchID
policySwitchId = snd . policyCoord

instance Hashable Policy

instance Buildable Policy where
    build = bprint shown

instance Conflict Policy Policy where
    conflictReason (Policy coord1 action1 _) (Policy coord2 action2 _) =
        let pack action = M.fromList $ map (\a -> (OF.actionToType a, a)) action
            [at1, at2] = map pack [action1, action2]
            actionsOfSameTypeAreSame = M.intersectionWith (==) at1 at2
        in  if coord1 == coord2 && and actionsOfSameTypeAreSame
            then Right ()
            else Left $ sformat ("Policies "%shown%" & "%shown%" conflict!")
                        action1 action2

instance MessagePack Policy where
    toObject (Policy xid action pid) = binaryToObject (xid, action, pid)
    fromObject o = do
        (xid, action, pid) <- binaryFromObject o
        return (Policy xid action pid)


data Configuration = Configuration
    { _configEntry    :: M.Map PolicyCoord $ S.Set Policy
    , _configRejected :: S.Set Policy
    } deriving (Eq, Show, Generic)

makeLenses ''Configuration

instance Hashable Configuration where
  hashWithSalt s (Configuration e r) =
    hashWithSalt s ( hashWithSalt s (second S.toList <$> M.toList e)
                   , hashWithSalt s (S.toList r)
                   )

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

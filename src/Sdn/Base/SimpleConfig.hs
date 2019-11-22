{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Sdn.Base.SimpleConfig
    ( SimpleConfig (..)
    , mkSimpleConfig
    ) where

import           Control.Lens        (AsEmpty (..), At (..), Index, Iso', IxValue, Ixed (..),
                                      Wrapped (..), iso, mapping, to, _Wrapped')
import           Data.Default        (Default (..))
import qualified Data.Map.Strict     as M
import           Data.MessagePack    (MessagePack (..))
import qualified Data.Set            as S
import qualified Data.Text.Buildable
import           Formatting          (bprint, build)
import qualified GHC.Exts            as Exts
import           Universum

import           Sdn.Base.CStruct
import           Sdn.Base.Quorum
import           Sdn.Base.Types
import           Sdn.Extra.Util

-- | How policies are included into CStruct.
type PolicyEntry p = Acceptance p

-- | Generic implementation of cstruct. Very inefficient.
newtype SimpleConfig p = SimpleConfig
    { unConfiguration :: S.Set (PolicyEntry p)
    } deriving (Eq, Show)

instance Wrapped (SimpleConfig p) where
    type Unwrapped (SimpleConfig p) = S.Set (PolicyEntry p)
    _Wrapped' = iso unConfiguration SimpleConfig

instance Ord p => Exts.IsList (SimpleConfig p) where
    type Item (SimpleConfig p) = Exts.Item (Unwrapped (SimpleConfig p))
    toList = Exts.toList . unConfiguration
    fromList = SimpleConfig . Exts.fromList

instance Default (SimpleConfig p) where
    def = SimpleConfig def

instance AsEmpty (SimpleConfig p) where
    _Empty = _Wrapped' . _Empty

mkSimpleConfig :: (Ord p, Conflict p p, Buildable p) => [PolicyEntry p] -> Maybe (SimpleConfig p)
mkSimpleConfig policies =
    let res = SimpleConfig $ S.fromList policies
    in  guard (consistent res) $> res

instance Buildable p => Buildable (SimpleConfig p) where
    build = bprint (listF ", " build) . toList . unConfiguration

instance (Ord p, MessagePack p) => MessagePack (SimpleConfig p) where
    toObject = toObject . S.toList . unConfiguration
    fromObject = fmap (SimpleConfig . S.fromList) . fromObject

instance Ord p => Ixed (SimpleConfig p) where
    ix i = _Wrapped' . ix i

instance Ord p => At (SimpleConfig p) where
    at i = _Wrapped' . at i

type instance Index (SimpleConfig p) = Index (Unwrapped (SimpleConfig p))
type instance IxValue (SimpleConfig p) = IxValue (Unwrapped (SimpleConfig p))

-- | Policy conflicts with cstruct if it conflicts with at least one of the
-- policies of cstruct.
instance (Eq p, Conflict p p, Buildable p) => Conflict (PolicyEntry p) (SimpleConfig p) where
    policy `conflictReason` SimpleConfig policiesHeap =
        mapM_ (conflictReason policy) policiesHeap

-- | Symmetric to instance above.
instance (Eq p, Conflict p p, Buildable p) => Conflict (SimpleConfig p) (PolicyEntry p) where
    conflictReason = flip conflictReason

-- | CStructs conflict if there are a couple of policies in them which
-- conflict.
instance (Eq p, Conflict p p, Buildable p) => Conflict (SimpleConfig p) (SimpleConfig p) where
    policies1 `conflictReason` SimpleConfig policies2 =
        mapM_ (conflictReason policies1) policies2

-- | Transpose votes about configurations.
perPolicy
    :: Ord p
    => Iso' (Votes qf (SimpleConfig p)) (M.Map (PolicyEntry p) $ Votes qf ())
perPolicy = _Wrapped' . mapping _Wrapped' . iso toPolicyMap fromPolicyMap
  where
    toPolicyMap
        :: Ord p
        => Map AcceptorId (Set $ PolicyEntry p)
        -> Map (PolicyEntry p) $ Votes qf ()
    toPolicyMap accMap =
        M.unionsWith mappend
        [ one (policy, Votes $ one (accId, ()))
        | (accId, policies) <- M.toList accMap
        , policy <- S.toList policies
        ]
    fromPolicyMap
        :: Ord p
        => Map (PolicyEntry p) $ Votes qf ()
        -> Map AcceptorId (Set $ PolicyEntry p)
    fromPolicyMap policyMap =
        M.unionsWith mappend
        [ one (accId, one policy)
        | (policy, Votes votes) <- M.toList policyMap
        , (accId, ()) <- M.toList votes
        ]


instance (Ord p, Conflict p p, Buildable p) => CStruct (SimpleConfig p) where
    type Cmd (SimpleConfig p) = PolicyEntry p
    glb = checkingAgreement $ underneath2 S.union
    lub = underneath2 S.intersection
    SimpleConfig c1 `extends` SimpleConfig c2 = c2 `S.isSubsetOf` c1
    difference = Exts.toList ... underneath2 S.difference

    addCommand p c = checkingAgreement (underneath . S.insert) p c

    -- for each policy check, whether there is a quorum containing
    -- its acceptance or rejection
    combination (votes :: Votes qf $ SimpleConfig p) =
        let votes' = either error identity $ mapM preSanityCheck votes
        in sanityCheck . SimpleConfig . M.keysSet $
          M.filter (isQuorum @qf) (votes' ^. perPolicy)
      where
        sanityCheck = first ("combination: " <> ) . checkingConsistency
        preSanityCheck = first ("combination arg validity: " <> ) . checkingConsistency

instance Ord p => AtCmd (SimpleConfig p) where
    atCmd raw = _Wrapped' . to getter
      where
        getter config =
            if | S.member (Accepted raw) config -> Just AcceptedT
               | S.member (Rejected raw) config -> Just RejectedT
               | otherwise -> Nothing

instance ( Ord p
         , Show p
         , Buildable p
         , Typeable p
         , MessagePack p
         , MayHaveProposerId p
         , Conflict p p
         ) =>
         PracticalCStruct (SimpleConfig p)

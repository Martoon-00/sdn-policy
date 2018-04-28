{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Fake policies with various conflicting behaviour.

module Sdn.Policy.Fake
    ( Policy (..)
    , PolicyName
    , policyName

    , Configuration (..)
    , mkConfig
    ) where

import           Control.Lens        (AsEmpty (..), At (..), Index, Iso', IxValue,
                                      Ixed (..), Wrapped (..), iso, mapping, to,
                                      _Wrapped')
import           Data.Default        (Default (..))
import qualified Data.Map.Strict     as M
import           Data.MessagePack    (MessagePack (..))
import qualified Data.Set            as S
import qualified Data.Text.Buildable
import           Formatting          (bprint, build, (%))
import qualified GHC.Exts            as Exts
import           Test.QuickCheck     (Arbitrary (..), getNonNegative, oneof, resize,
                                      suchThat)
import           Universum

import           Sdn.Base.CStruct
import           Sdn.Base.Quorum
import           Sdn.Base.Types
import           Sdn.Extra.Util

newtype PolicyName = PolicyName Int
    deriving (Eq, Ord, Show, Num, Buildable, MessagePack)

instance Arbitrary PolicyName where
    arbitrary = PolicyName <$> arbitrary `suchThat` (>= 0)

-- | Abstract SDN policy.
data Policy
    = GoodPolicy PolicyName       -- ^ Agrees with any other one
    | BadPolicy PolicyName        -- ^ Conflicts with any other one
    | MoodyPolicy Int PolicyName  -- ^ Conflicts if group ids are equal
    deriving (Eq, Ord, Show, Generic)

instance Buildable Policy where
    build = \case
        GoodPolicy name -> bprint ("Good \""%build%"\"") name
        BadPolicy name -> bprint ("Bad \""%build%"\"") name
        MoodyPolicy id name -> bprint ("Moody #"%build%" \""%build%"\"") id name

policyName :: Policy -> PolicyName
policyName = \case
    GoodPolicy name    -> name
    BadPolicy name     -> name
    MoodyPolicy _ name -> name

instance Conflict Policy Policy where
    agrees a b | a == b                            = True
    agrees GoodPolicy{} _                          = True
    agrees _ GoodPolicy{}                          = True
    agrees BadPolicy{} _                           = False
    agrees _ BadPolicy{}                           = False
    agrees (MoodyPolicy id1 _) (MoodyPolicy id2 _) = id1 /= id2

instance Arbitrary Policy where
    arbitrary =
        oneof
        [ pure GoodPolicy
        , pure BadPolicy
        , MoodyPolicy <$> resize 5 (getNonNegative <$> arbitrary)
        ]
        <*>
        resize 5 arbitrary

instance MessagePack Policy

-- | How policies are included into CStruct.
type PolicyEntry = Acceptance Policy

-- | For our simplified model with abstract policies, cstruct is just set of
-- policies.
newtype Configuration = Configuration
    { unConfiguration :: S.Set PolicyEntry
    } deriving (Eq, Show)

instance Wrapped Configuration where
    type Unwrapped Configuration = S.Set PolicyEntry
    _Wrapped' = iso unConfiguration Configuration

instance Exts.IsList Configuration where
    type Item Configuration = Exts.Item (Unwrapped Configuration)
    toList = Exts.toList . unConfiguration
    fromList = Configuration . Exts.fromList

type instance Element Configuration = Element (Unwrapped Configuration)

instance Container Configuration where
    null = null . unpack
    toList = toList . unpack

instance NontrivialContainer Configuration where
    foldr f s l = foldr f s (unpack l)
    foldl f s l = foldl f s (unpack l)
    foldl' f s l = foldl' f s (unpack l)
    length = length . unpack
    elem e l = elem e (unpack l)
    maximum = maximum . unpack
    minimum = minimum . unpack

instance Default Configuration where
    def = Configuration def

instance AsEmpty Configuration where
    _Empty = _Wrapped' . _Empty

mkConfig :: [PolicyEntry] -> Maybe Configuration
mkConfig policies =
    let res = Configuration $ S.fromList policies
    in  guard (consistent res) $> res

instance Buildable Configuration where
    build = bprint (listF ", " build) . toList . unConfiguration

instance MessagePack Configuration where
    toObject = toObject . S.toList . unConfiguration
    fromObject = fmap (Configuration . S.fromList) . fromObject

instance Ixed Configuration where
    ix i = _Wrapped' . ix i

instance At Configuration where
    at i = _Wrapped' . at i

type instance Index Configuration = Index (Unwrapped Configuration)
type instance IxValue Configuration = IxValue (Unwrapped Configuration)

-- | Policy conflicts with cstruct if it conflicts with at least one of the
-- policies of cstruct.
instance Conflict PolicyEntry Configuration where
    policy `conflicts` Configuration policiesHeap =
        any (conflicts policy) policiesHeap

-- | Symmetric to instance above.
instance Conflict Configuration PolicyEntry where
    conflicts = flip conflicts

-- | CStructs conflict if there are a couple of policies in them which
-- conflict.
instance Conflict Configuration Configuration where
    policies1 `conflicts` Configuration policies2 =
        any (conflicts policies1) policies2

-- | Transpose votes about configurations.
perPolicy :: Iso' (Votes qf Configuration) (M.Map PolicyEntry $ Votes qf ())
perPolicy = _Wrapped' . mapping _Wrapped' . iso toPolicyMap fromPolicyMap
  where
    toPolicyMap
        :: Map AcceptorId (Set PolicyEntry)
        -> Map PolicyEntry $ Votes qf ()
    toPolicyMap accMap =
        M.unionsWith mappend
        [ one (policy, Votes $ one (accId, ()))
        | (accId, policies) <- M.toList accMap
        , policy <- S.toList policies
        ]
    fromPolicyMap
        :: Map PolicyEntry $ Votes qf ()
        -> Map AcceptorId (Set PolicyEntry)
    fromPolicyMap policyMap =
        M.unionsWith mappend
        [ one (accId, one policy)
        | (policy, Votes votes) <- M.toList policyMap
        , (accId, ()) <- M.toList votes
        ]


pattern HappyPolicy :: PolicyName -> Acceptance Policy
pattern HappyPolicy name = Accepted (GoodPolicy name)

instance CStruct Configuration where
    type Cmd Configuration = PolicyEntry
    glb = checkingAgreement $ underneath2 S.union
    lub = underneath2 S.intersection
    Configuration c1 `extends` Configuration c2 = c2 `S.isSubsetOf` c1
    difference = Exts.toList ... underneath2 S.difference

    addCommand p c = case (p, S.lookupMin $ unConfiguration c) of
        (HappyPolicy _, Just (HappyPolicy _)) ->
            -- optimization for profiling
            pure $ Exts.coerce (S.insert p) c
        _ ->
            checkingAgreement (underneath . S.insert) p c

    -- for each policy check, whether there is a quorum containing
    -- its acceptance or rejection
    combination (votes :: Votes qf Configuration) =
        sanityCheck . Configuration . M.keysSet $
        M.filter (isQuorum @qf) (votes ^. perPolicy)
      where
        sanityCheck = first ("combination: " <> ) . checkingConsistency

instance AtCmd Configuration where
    atCmd raw = _Wrapped' . to getter
      where
        getter config =
            if | S.member (Accepted raw) config -> Just AcceptedT
               | S.member (Rejected raw) config -> Just RejectedT
               | otherwise -> Nothing

instance PracticalCStruct Configuration

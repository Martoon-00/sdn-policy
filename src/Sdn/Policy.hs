{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Policies arangement.

module Sdn.Policy where


import           Data.MessagePack    (MessagePack (..))
import qualified Data.Set            as S
import           Data.String         (IsString)
import qualified Data.Text.Buildable
import           Formatting          (bprint, build, sformat, (%))
import           Test.QuickCheck     (Arbitrary (..), getNonNegative, scale)
import           Universum

import           Sdn.CStruct         (Acceptance (..), Command (..), Conflict (..),
                                      checkingAgreement)
import           Sdn.Util

newtype PolicyName = PolicyName Text
    deriving (Eq, Ord, Show, Buildable, IsString, MessagePack)

instance Arbitrary PolicyName where
    arbitrary =
        scale (* 10) $
        PolicyName . sformat ("policy #"%build @Int) . getNonNegative
            <$> arbitrary

-- | Abstract SDN policy.
data Policy
    = GoodPolicy PolicyName       -- ^ Agrees with any other one
    | BadPolicy PolicyName        -- ^ Conflicts with any other one
    | MoodyPolicy Int PolicyName  -- ^ Conflicts if group ids are equal
    deriving (Eq, Ord, Generic)

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
    agrees GoodPolicy{} _                          = True
    agrees _ GoodPolicy{}                          = True
    agrees BadPolicy{} _                           = False
    agrees _ BadPolicy{}                           = False
    agrees (MoodyPolicy id1 _) (MoodyPolicy id2 _) = id1 /= id2

instance MessagePack Policy

-- | How policies are included into CStruct.
type PolicyEntry = Acceptance Policy

-- | For our simplified model with abstract policies, cstruct is just set of
-- policies.
type Configuration = S.Set PolicyEntry

instance Buildable Configuration where
    build = bprint buildList . toList

instance MessagePack Configuration where
    toObject = toObject . S.toList
    fromObject = fmap S.fromList . fromObject

-- | Policy conflicts with cstruct if it conflicts with at least one of the
-- policies of cstruct.
instance Conflict PolicyEntry Configuration where
    policy `conflicts` policiesHeap =
        any (conflicts policy) policiesHeap

-- | Symmetric to instance above.
instance Conflict Configuration PolicyEntry where
    conflicts = flip conflicts

-- | CStructs conflict if there are a couple of policies in them which
-- conflict.
instance Conflict Configuration Configuration where
    policies1 `conflicts` policies2 =
        any (conflicts policies1) policies2

instance Command PolicyEntry Configuration where
    addCommand = checkingAgreement S.insert
    glb = checkingAgreement S.union
    lub = S.intersection
    extends = flip S.isSubsetOf

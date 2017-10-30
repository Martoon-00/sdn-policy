-- | Policies arangement.

module Sdn.Policy where


import           Data.MessagePack (MessagePack (..))
import qualified Data.Set         as S
import           Universum

import           Sdn.CStruct      (Acceptance (..), Command (..), Conflict (..),
                                   checkingAgreement)

-- | Abstract SDN policy.
data Policy
    = GoodPolicy Text       -- ^ Agrees with any other one
    | BadPolicy Text        -- ^ Conflicts with any other one
    | MoodyPolicy Int Text  -- ^ Conflicts if ids are equal
    deriving (Eq, Ord, Generic)

policyName :: Policy -> Text
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

-- | Policies arangement.

module Sdn.Policy where


import qualified Data.Set    as S
import           Universum

import           Sdn.CStruct (Acceptance (..), Command (..), Conflict (..),
                              checkingAgreement)

-- | Abstract SDN policy.
data Policy
    = GoodPolicy Text       -- ^ Agrees with any other one
    | BadPolicy Text        -- ^ Conflicts with any other one
    | MoodyPolicy Int Text  -- ^ Conflicts if ids are equal
    deriving (Eq, Ord)

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

-- | How policies are included into CStruct.
type PolicyEntry = Acceptance Policy

-- | For our simplified model with abstract policies, cstruct is just set of
-- policies.
type PoliciesHeap = S.Set PolicyEntry

-- | Policy conflicts with cstruct if it conflicts with at least one of the
-- policies of cstruct.
instance Conflict PolicyEntry PoliciesHeap where
    policy `conflicts` policiesHeap =
        any (conflicts policy) policiesHeap

-- | Symmetric to instance above.
instance Conflict PoliciesHeap PolicyEntry where
    conflicts = flip conflicts

-- | CStructs conflict if there are a couple of policies in them which
-- conflict.
instance Conflict PoliciesHeap PoliciesHeap where
    policies1 `conflicts` policies2 =
        any (conflicts policies1) policies2

instance Command PolicyEntry PoliciesHeap where
    addCommand = checkingAgreement S.insert
    glb = checkingAgreement S.union
    lub = S.intersection

{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Policies arangement.

module Sdn.Base.Policy where


import           Control.Monad.Except (throwError)
import           Data.MessagePack     (MessagePack (..))
import qualified Data.Set             as S
import           Data.String          (IsString)
import qualified Data.Text.Buildable
import           Formatting           (bprint, build, sformat, (%))
import           Test.QuickCheck      (Arbitrary (..), getNonNegative, oneof, resize)
import           Universum

import           Sdn.Base.CStruct
import           Sdn.Base.Quorum
import           Sdn.Extra.Util

newtype PolicyName = PolicyName Text
    deriving (Eq, Ord, Show, Buildable, IsString, MessagePack)

instance Arbitrary PolicyName where
    arbitrary =
        PolicyName . sformat ("policy #"%build @Int) . getNonNegative
            <$> arbitrary

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
type Configuration = S.Set PolicyEntry

mkConfig :: [PolicyEntry] -> Maybe (S.Set PolicyEntry)
mkConfig policies =
    let res = S.fromList policies
    in  guard (consistent res) $> res

instance Buildable Configuration where
    build = bprint (buildList ", ") . toList

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

instance Command Configuration PolicyEntry where
    addCommand = checkingAgreement S.insert
    glb = checkingAgreement S.union
    lub = S.intersection
    extends = flip S.isSubsetOf

    -- for each policy check, whether there is a quorum containing
    -- its acceptance or rejection
    combination (votes :: Votes qf Configuration) =
        let allInvolvedPolicies =
                toList $ fold $ ((votes & traverse . listL . traverse %~ acceptanceCmd) )
            combPolicies = flip mapMaybe allInvolvedPolicies $ \policy ->
                    tryAcceptance Accepted policy
                <|> tryAcceptance Rejected policy
        in  sanityCheck $ S.fromList combPolicies
      where
         tryAcceptance acc policy =
             let containsPolicy = (`extends` liftCommand (acc policy))
                 containingVotes = filterVotes containsPolicy votes
             in  guard (isQuorum @qf containingVotes) $> acc policy
         sanityCheck x
             | contradictive x =
                  throwError $
                  sformat ("combination: got contradictive cstruct: "%build) x
             | otherwise = pure x

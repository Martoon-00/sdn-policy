{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Utilities for various phases of Paxos.

module Sdn.Protocol.Common.Phases
    ( MakeProposal
    , BatchingSettings (..)
    , LearningCallback (..)
    , PolicyTargets (..)
    , simpleProposal
    , batchingProposal
    , batchedOrSimpleProposals
    , groupPolicyTargets

    , MonadPhase
    , updateLearnedValue
    , onFixatedPolicies
    , learnCStruct
    , rememberVoteForPolicy
    , rememberUnstableCmdVote
    , confirmCommitted
    , isPolicyUnconfirmed
    ) where

import           Control.Exception            (assert)
import           Control.Lens                 (at, non, (%%=), (%=), (.=))
import           Control.TimeWarp.Rpc         (MonadRpc)
import           Control.TimeWarp.Timed       (MonadTimed (..))
import           Data.Default                 (Default (..))
import qualified Data.Map                     as M
import qualified Data.Semigroup               as Semi
import qualified Data.Set                     as S
import           Formatting                   (build, sformat, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra                    (MFunctored (..), MonadLog, MonadReporting,
                                               OldNew (..), PreparedAction (..),
                                               RpcOptions, decompose, foldlF', logError,
                                               logInfo, presence, throwOnFail, zoom,
                                               (<<<%=))
import           Sdn.Extra.Batching
import           Sdn.Protocol.Common.Context
import           Sdn.Protocol.Common.Messages
import           Sdn.Protocol.Processes
import           Sdn.Protocol.Versions

-- | Action which makes a proposal.
type MakeProposal m = PreparedAction (DeclaredRawCmd m) m

simpleProposal :: Applicative m => (DeclaredRawCmd m -> m ()) -> MakeProposal m
simpleProposal = PreparedAction . pure

batchingProposal
    :: (MonadIO m, MonadCatch m, MonadTimed m)
    => BatchingSettings -> (NonEmpty (DeclaredRawCmd m) -> m ()) -> MakeProposal m
batchingProposal = batchedAction

batchedOrSimpleProposals
    :: (MonadIO m, MonadCatch m, MonadTimed m)
    => Maybe BatchingSettings
    -> (NonEmpty (DeclaredRawCmd m) -> m ())
    -> MakeProposal m
batchedOrSimpleProposals mbs propose = case mbs of
    Nothing -> simpleProposal (propose . one)
    Just bs -> batchingProposal bs propose


-- | Executed when new pack of policies is applied.
-- Batched for optimization purposes.
newtype LearningCallback m = LearningCallback
    { runLearningCallback :: NonEmpty (DeclaredCmd m) -> m ()
    }

instance MFunctored LearningCallback where
    type MFunctoredConstr LearningCallback n m = DeclaredCmd n ~ DeclaredCmd m
    hoistItem modifyM = LearningCallback . fmap modifyM . runLearningCallback

-- | Specifies which learners should receive a policy at Fast version of
-- protocol.
-- Every acceptor will follow this rule, thus it's preferred not
-- to specify all learners in order to avoid square complexity.
data PolicyTargets cstruct = PolicyTargets
    { getPolicyTargets :: HasMembers => RawCmd cstruct -> [Learner]
    }

instance Default (PolicyTargets cstruct) where
    def = PolicyTargets (\_ -> toList takeAllProcesses)

-- | Distribute items in heaps, so that each heap contains values
-- addressed to same set of targets.
groupPolicyTargets
    :: HasMembers
    => PolicyTargets cstruct
    -> (a -> RawCmd cstruct)
    -> [a]
    -> [([Learner], NonEmpty a)]
groupPolicyTargets (PolicyTargets evalTargets) getPolicy values =
    let targetsNValues =
            [ (sort targets, one value)
            | value <- values, let targets = evalTargets (getPolicy value)
            ]
    in  M.toList $ M.fromListWith (Semi.<>) targetsNValues

-- | Common constraints for all phases.
type MonadPhase cstruct m =
    ( MonadIO m
    , MonadCatch m
    , MonadTimed m
    , MonadRpc RpcOptions m
    , MonadLog m
    , MonadReporting m
    , HasMembersInfo
    , cstruct ~ DeclaredCStruct m
    , PracticalCStruct cstruct
    )


-- * Learning

-- | Update learned value with all checks and cautions.
updateLearnedValue
    :: (CStruct cstruct, Eq cstruct, Buildable cstruct)
    => cstruct
    -> TransactionM (ProcessState Learner pv cstruct) [Cmd cstruct]
updateLearnedValue newLearned = do
    prevLearned <- use learnerLearned

    -- sanity check
    if newLearned `extends` prevLearned
    then learnerLearned .= newLearned
    else do
        if prevLearned `extends` newLearned
        then logInfo "Previously learnt cstruct extends new one, ignoring"
        else reportErrorBadLearnedCStruct prevLearned newLearned

    -- report if the interesting happened
    when (newLearned /= prevLearned) $
        reportNewLearnedCStruct newLearned

    return (newLearned `difference` prevLearned)
  where
    reportErrorBadLearnedCStruct prev new =
        logError $
        sformat ("Newly learned cstruct doesn't extend previous one:\n"%
                 "\t"%build%"\n\t->\n\t"%build)
                prev new
    reportNewLearnedCStruct new =
        logInfo $
        sformat ("New learned cstruct: "%build) new

instance Applicative m => Monoid (LearningCallback m) where
    mempty = LearningCallback $ \_ -> pure ()
    LearningCallback a `mappend` LearningCallback b = LearningCallback $ \p -> a p *> b p

-- | Set of actions, invoked when learner finds some new policies fixated.
onFixatedPolicies
    :: forall cstruct pv m.
       (MonadPhase cstruct m, HasContextOf Learner pv m)
    => LearningCallback m -> NonEmpty (Cmd cstruct) -> m ()
onFixatedPolicies (LearningCallback callback) policyAcceptances = do
    -- notify proposer that it can stop reproposing policy
    -- TODO: enable
    -- let policies = map acceptanceCmd policyAcceptances
    -- submit (processAddress Proposer) (CommittedMsg @cstruct policies)

    -- invoke callback
    callback policyAcceptances

-- | Learning phase of algorithm.
learnCStruct
    :: (MonadPhase cstruct m, HasContextOf Learner pv m)
    => LearningCallback m
    -> (Votes (VersionQuorum pv) cstruct -> Either Text cstruct)
    -> AcceptorId
    -> cstruct
    -> m ()
learnCStruct callback combinator accId (cstruct :: cstruct) = do
    newLearnedPolicies <- withProcessStateAtomically $ do
        -- rewrite cstruct kept for this acceptor
        zoom (learnerVotes . at accId . non def) $ do
            store <- get
            case extendCoreCStruct cstruct store of
                Left err ->
                    -- if new cstruct doesn't extend previous one,
                    -- it is not an error due to usage of non-FIFO channels
                    logInfo $ cstructIgnored err
                Right store' ->
                    put store'

        -- update total learned cstruct
        -- it should be safe (i.e. it does not induce commands losses)
        -- to learn votes even from fast round if recovery is going
        -- to happen then, because recovery deals with liveleness, correctness
        -- always holds.
        learnedDifference <-
            use learnerVotes
            >>= throwOnFail ProtocolError . combinator . fmap totalCStruct
            >>= updateLearnedValue

        return learnedDifference

    whenNotNull newLearnedPolicies $ onFixatedPolicies callback
  where
    cstructIgnored =
        sformat ("New cstruct doesn't extend the old one and so is ignored: "%build)

-- | Adds vote for a single policy to given 'PerCmdVotes'.
rememberVoteForPolicy
    :: forall cstruct qf s.
       PracticalCStruct cstruct
    => Lens' s (PerCmdVotes qf cstruct)
    -> AcceptorId
    -> Cmd cstruct
    -> TransactionM s (RawCmd cstruct, OldNew (Votes qf AcceptanceType))
rememberVoteForPolicy atVotesL accId policyAcceptance =
    let (acceptance, policy) = decompose policyAcceptance
    in  lift $ atVotesL . at policy . non mempty %%= \oldVotes ->
            let newVotes = oldVotes & at accId .~ Just acceptance
                allVotes = OldNew { getOld = oldVotes, getNew = newVotes }
            in assertNoRebind oldVotes acceptance $
               ((policy, allVotes), newVotes)
  where
    -- checks we are not changing existing vote for particular "acceptance" value
    assertNoRebind oldVotes newAcceptance =
        let oldAcceptance = oldVotes ^. at accId
        in  assert (oldAcceptance == Nothing || oldAcceptance == Just newAcceptance)

-- | Add vote for a single policy to 'CStructStore' related to a given acceptor.
rememberUnstableCmdVote
    :: PracticalCStruct cstruct
    => Lens' s (Votes qf $ CStructStore cstruct)
    -> AcceptorId
    -> Cmd cstruct
    -> TransactionM s (RawCmd cstruct, OldNew (Votes qf AcceptanceType))
rememberUnstableCmdVote atStore accId policyAcc = do
    change <- atStore <<<%= (at accId . non def %~ snd . addUnstableCmd policyAcc)
    let policyVotes = fmap @OldNew takeVotesForPolicy change
    return (acceptanceCmd policyAcc, policyVotes)
  where
    policy = acceptanceCmd policyAcc
    takeVotesForPolicy =
        catMaybesVotes . fmap @(Votes _) (view (atCmd policy) . totalCStruct)

-- * Confirmation to proposal

-- | Remember that given policies were committed.
confirmCommitted
    :: (MonadPhase cstruct m, HasContextOf Proposer pv m)
    => CommittedMsg cstruct -> m ()
confirmCommitted (CommittedMsg policies) = do
    withProcessStateAtomically $
        proposerUnconfirmedPolicies %= foldlF' S.delete policies

-- | Helper to check whether policy is in list of unconfirmed policies.
isPolicyUnconfirmed
    :: (MonadPhase cstruct m, HasContextOf Proposer pv m)
    => RawCmd cstruct -> m Bool
isPolicyUnconfirmed policy = do
    withProcessStateAtomically $
        use $ proposerUnconfirmedPolicies . at policy . presence

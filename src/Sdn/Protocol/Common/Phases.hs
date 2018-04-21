{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Utilities for various phases of Paxos.

module Sdn.Protocol.Common.Phases
    ( MakeProposal
    , simpleProposal
    , BatchingSettings (..)
    , batchingProposal
    , batchedOrSimpleProposals
    , LearningCallback (..)

    , MonadPhase
    , updateLearnedValue
    , warnOnPartialCStructApply
    , warnOnPartialCommandsApply
    , onFixatedPolicies
    , learnCStruct
    , confirmCommitted
    , isPolicyUnconfirmed
    ) where

import           Control.Lens                 (at, non, (%=), (.=), (<%=))
import           Control.TimeWarp.Rpc         (MonadRpc)
import           Control.TimeWarp.Timed       (MonadTimed (..))
import           Data.Default                 (Default (..))
import qualified Data.Set                     as S
import           Formatting                   (build, sformat, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra                    (MFunctored (..), MonadLog, MonadReporting,
                                               PreparedAction (..), RpcOptions, foldlF',
                                               listF, logError, logInfo, presence, submit,
                                               throwOnFail)
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

-- * Common

-- | Drop a warning if we are going to merge received cstruct, but
-- dropping some of its policies in the process.
warnOnPartialCStructApply
    :: (MonadLog m, CStruct cstruct)
    => cstruct -> cstruct -> m ()
warnOnPartialCStructApply incoming updated = do
    unless (updated `extends` incoming) $
        logInfo $
        sformat ("Some policies were dropped while applying incoming cstruct:\
                 \\n  incoming:  "%build%
                 "\n  new value: "%build)
              incoming updated

-- | Drop a warning if some of commands were not applied.
warnOnPartialCommandsApply
    :: forall cstruct m.
       (MonadLog m, PracticalCStruct cstruct)
    => [Cmd cstruct] -> [Cmd cstruct] -> m ()
warnOnPartialCommandsApply incoming update = do
    unless (S.fromList update /= S.fromList incoming) $
        logInfo $
        sformat ("Some policies were dropped while applying incoming cstruct:\
                 \\n  incoming:  "%listF ", " build%
                 "\n  new value: "%listF ", " build)
              incoming update

-- * Learning

-- | Update learned value with all checks and cautions.
updateLearnedValue
    :: (CStruct cstruct, Eq cstruct)
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
    let policies = map acceptanceCmd policyAcceptances
    submit (processAddress Proposer) (CommittedMsg @cstruct policies)

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

        -- we should check here that new cstruct extends previous one.
        -- but the contrary is not an error, because of not-FIFO channels
        let replaceOrRemainOld = maxOrSecond
        updated <- learnerVotes . at accId . non def <%= replaceOrRemainOld cstruct
        warnOnPartialCStructApply cstruct updated

        -- update total learned cstruct
        -- it should be safe (i.e. it does not induce commands losses)
        -- to learn votes even from fast round if recovery is going
        -- to happen then, because recovery deals with liveleness, correctness
        -- always holds.
        learnedDifference <-
            use learnerVotes
            >>= throwOnFail ProtocolError . combinator
            >>= updateLearnedValue

        return learnedDifference

    whenNotNull newLearnedPolicies $ onFixatedPolicies callback

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

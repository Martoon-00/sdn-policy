-- | Phases of algorithm

module Sdn.Protocol.Phases where

import           Control.Lens           (at, non, (%=), (.=), (<%=), (<<.=), (<>=))
import           Control.TimeWarp.Rpc   (MonadRpc)
import           Control.TimeWarp.Timed (MonadTimed (..))
import           Formatting             (build, sformat, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra              (MonadLog, MonadReporting, buildList, logInfo,
                                         submit, throwOnFail)
import           Sdn.Protocol.Context
import           Sdn.Protocol.Messages
import           Sdn.Protocol.Processes
import           Sdn.Protocol.Versions

-- * Commons

-- | Common constraints for all phases.
type MonadPhase m =
    ( MonadIO m
    , MonadCatch m
    , MonadTimed m
    , MonadRpc m
    , MonadLog m
    , MonadReporting m
    , HasMembers
    )

-- * Phases
-- ** Proposal

propose
    :: (MonadPhase m, HasContextOf Proposer Classic m)
    => Policy -> m ()
propose policy = do
    logInfo $ sformat ("Proposing policy: "%build) policy
    -- remember policy (for testing purposes)
    withProcessState $
        proposerProposedPolicies <>= one policy
    -- and send it to leader
    submit (processAddress Leader) (ProposalMsg policy)

proposeFast
    :: (MonadPhase m, HasContextOf Proposer Fast m)
    => Policy -> m ()
proposeFast policy = do
    logInfo $ sformat ("Proposing policy (fast): "%build) policy
    withProcessState $
        proposerProposedPolicies <>= one policy
    broadcastTo (processAddresses Leader <> processesAddresses Acceptor)
                (ProposalFastMsg policy)

-- ** Remembering proposals

rememberProposal
    :: (MonadPhase m, HasContextOf Leader pv m)
    => ProposalMsg -> m ()
rememberProposal (ProposalMsg policy) = do
    -- atomically modify process'es state
    withProcessState $ do
        -- add policy to pending policiesToApply for next ballot
        bal <- nextFreshBallotId <$> use leaderBallotId
        leaderPendingPolicies . at bal . non mempty %= (policy :)

acceptorRememberFastProposal
    :: (MonadPhase m, HasContextOf Acceptor Fast m)
    => ProposalFastMsg -> m ()
acceptorRememberFastProposal (ProposalFastMsg policy) =
    withProcessState $ do
        ballotId <- nextFreshBallotId <$> use acceptorBallotId
        acceptorPendingPolicies . at ballotId . non mempty <>= one policy

-- TODO: very bad \^/
leaderRememberFastProposal
    :: (MonadPhase m, HasContextOf Leader Fast m)
    => ProposalFastMsg -> m ()
leaderRememberFastProposal (ProposalFastMsg policy) =
    withProcessState $ do
        ballotId <- nextFreshBallotId <$> use leaderBallotId
        leaderPendingPoliciesFast . at ballotId . non mempty <>= one policy

-- ** Phase 1

phase1a
    :: forall pv m.
       (MonadPhase m, HasContextOf Leader pv m)
    => m ()
phase1a = do
    logInfo "Starting new ballot"

    msg <- withProcessState $ do
        -- increment ballot id
        leaderBallotId %= nextFreshBallotId
        -- make up an "1a" message
        Phase1aMsg @pv <$> use leaderBallotId

    broadcastTo (processesAddresses Acceptor) msg

phase1b
    :: forall pv m.
       (MonadPhase m, HasContextOf Acceptor pv m)
    => Phase1aMsg pv -> m ()
phase1b (Phase1aMsg ballotId) = do
    msg <- withProcessState $ do
        -- promise not to accept messages of lesser ballot numbers
        -- make stored ballot id not lesser than @ballotId@
        acceptorBallotId %= max ballotId
        Phase1bMsg @pv
            <$> use acceptorId
            <*> use acceptorBallotId
            <*> use acceptorCStruct

    submit (processAddress Leader) msg

initFastBallot
    :: (MonadPhase m, HasContextOf Leader Fast m)
    => m ()
initFastBallot = do
    logInfo "Starting new fast ballot"

    msg <- withProcessState $ do
        leaderBallotId %= nextFreshBallotId
        InitFastBallotMsg <$> use leaderBallotId

    broadcastTo (processesAddresses Acceptor) msg

-- ** Phase 2

phase2a
    :: forall pv m.
       (MonadPhase m, HasContextOf Leader pv m)
    => Phase1bMsg pv -> m ()
phase2a (Phase1bMsg accId ballotId cstruct) = do
    maybeMsg <- withProcessState $ do
        -- add received vote to set of votes stored locally for this ballot,
        -- initializing this set if doesn't exist yet
        newVotes <-
            leaderVotes . at ballotId . non mempty <%= addVote accId cstruct

        -- if some quorums appeared, recalculate Gamma and apply pending policiesToApply
        if isQuorum newVotes
        then do
            when (isMinQuorum newVotes) $
                logInfo $ "Just got 1b from quorum of acceptors at " <> pretty ballotId

            combined <- throwOnFail ProtocolError $ combination newVotes
            policiesToApply <- use $ leaderPendingPolicies . at ballotId . non mempty
            let cstructWithNewPolicies = foldr acceptOrRejectCommand combined policiesToApply

            logInfo "Broadcasting cstruct"
            pure $ Just (Phase2aMsg @pv ballotId cstructWithNewPolicies)
        else pure Nothing

    -- when got a message to submit - broadcast it
    whenJust maybeMsg $
        broadcastTo (processesAddresses Acceptor)

phase2b
    :: (MonadPhase m, HasContextOf Acceptor pv m)
    => Phase2aMsg pv -> m ()
phase2b (Phase2aMsg ballotId cstruct) = do
    maybeMsg <- withProcessState $ do
        localBallotId <- use acceptorBallotId
        localCstruct <- use acceptorCStruct

        -- check whether did we promise to ignore this message
        if localBallotId == ballotId && (cstruct `extends` localCstruct)
            || localBallotId < ballotId
        then do
           -- if ok, remember received info
           acceptorBallotId .= ballotId
           acceptorCStruct .= cstruct

           -- form message
           accId <- use acceptorId
           pure $ Just (Phase2bMsg accId cstruct)
        else do
           logInfo "Received cstruct was rejected"
           pure Nothing

    whenJust maybeMsg $
        broadcastTo (processesAddresses Learner)

phase2bFast
    :: (MonadPhase m, HasContextOf Acceptor Fast m)
    => InitFastBallotMsg -> m ()
phase2bFast (InitFastBallotMsg ballotId) = do
    msg <- withProcessState $ do
        cstruct <- use acceptorCStruct
        policiesToApply <- use $ acceptorPendingPolicies . at ballotId . non mempty
        logInfo $ sformat ("List of fast pending policies:\n    "%buildList ", ")
                  policiesToApply
        let cstruct' = foldr acceptOrRejectCommand cstruct policiesToApply

        accId <- use acceptorId
        pure $ Phase2bFastMsg accId cstruct'

    broadcastTo (processAddresses Leader <> processesAddresses Learner) msg

-- ** Learning

-- | Update learned value with all checks and cautions.
updateLearnedValue :: Configuration -> TransactionM (ProcessState Learner pv) ()
updateLearnedValue newLearned = do
    prevLearned <- learnerLearned <<.= newLearned

    -- sanity check
    unless (newLearned `extends` prevLearned) $
        errorBadLearnedCStruct prevLearned newLearned

    -- report if the interesting happened
    when (newLearned /= prevLearned) $
        reportNewLearnedCStruct newLearned
  where

    errorBadLearnedCStruct prev new =
        throwM . ProtocolError $
        sformat ("Newly learned cstruct doesn't extend previous one:\n"%
                 "\t"%build%"\n\t->\n\t"%build)
                prev new
    reportNewLearnedCStruct new =
        logInfo $
        sformat ("New learned cstruct: "%build) new

learn
    :: (MonadPhase m, HasContextOf Learner pv m)
    => Phase2bMsg -> m ()
learn (Phase2bMsg accId cstruct) = do
    withProcessState $ do
        -- rewrite cstruct kept for this acceptor

        -- we should check here that new cstruct extends previous one.
        -- but the contrary is not an error, because of not-FIFO channels
        learnerVotes . at accId . non mempty %= maxOrSecond cstruct

        -- update total learned cstruct
        use learnerVotes
            >>= throwOnFail ProtocolError . combination
            >>= updateLearnedValue

learnFast
    :: (MonadPhase m, HasContextOf Learner pv m)
    => Phase2bFastMsg -> m ()
learnFast (Phase2bFastMsg accId cstruct) = do
    withProcessState $ do
        learnerFastVotes . at accId . non mempty %= maxOrSecond cstruct

        -- it's safe to learn votes from fast round even if recovery is going happen,
        -- because recovery deals with liveleness, correctness always holds.
        use learnerFastVotes
             >>= throwOnFail ProtocolError . intersectingCombination
             >>= updateLearnedValue


-- ** Recovery detection and initialition

detectConflicts
    :: (MonadPhase m, HasContextOf Leader pv m)
    => Phase2bFastMsg -> m ()
detectConflicts (Phase2bFastMsg accId cstruct) = do
    return ()




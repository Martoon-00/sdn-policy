-- | Phases of algorithm

module Sdn.Protocol.Phases where

import           Control.Lens           (at, non, (%=), (+=), (.=), (<%=), (<+=), (<>=),
                                         (?=))
import           Control.TimeWarp.Rpc   (MonadRpc)
import           Control.TimeWarp.Timed (Microsecond, MonadTimed (..), after, schedule)
import           Data.Default           (def)
import qualified Data.Map               as M
import qualified Data.Set               as S
import           Formatting             (build, sformat, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra              (MonadLog, MonadReporting, as, listF, logError,
                                         logInfo, submit, throwOnFail)
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
    broadcastTo (processAddresses Leader) (ProposalMsg policy)

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
        -- We store @BallotId 'SomeRound@, 'as' allows to make it for specific round
        -- In most cases round type may be ommited though
        bal <- use leaderBallotId
        leaderPendingPolicies . forClassic . at (bal + 1) . non mempty %= (policy :)

acceptorRememberFastProposal
    :: (MonadPhase m, HasContextOf Acceptor Fast m)
    => ProposalFastMsg -> m ()
acceptorRememberFastProposal (ProposalFastMsg policy) =
    withProcessState $ do
        bal <- fmap (+1) . use $ acceptorLastKnownBallotId . as
        logInfo $ sformat (build%" to apply at "%build) policy bal
        acceptorFastPendingPolicies . at bal . non mempty <>= one policy

-- ** Phase 1

phase1a
    :: forall pv m.
       (MonadPhase m, HasContextOf Leader pv m)
    => m ()
phase1a = do
    logInfo "Starting new ballot"

    msg <- withProcessState $ do
        -- increment ballot id
        leaderBallotId += 1
        -- make up an "1a" message
        Phase1aMsg <$> use (leaderBallotId . as)

    broadcastTo (processesAddresses Acceptor) msg

phase1b
    :: forall pv m.
       (MonadPhase m, HasContextOf Acceptor pv m)
    => Phase1aMsg -> m ()
phase1b (Phase1aMsg bal) = do
    msg <- withProcessState $ do
        -- promise not to accept messages of lesser ballot numbers
        -- make stored ballot id not lesser than @bal@
        acceptorLastKnownBallotId . as %= max bal  -- TODO: remove 'as'
        Phase1bMsg
            <$> use acceptorId
            <*> use (acceptorLastKnownBallotId . as)
            <*> use acceptorCStruct

    submit (processAddress Leader) msg

initFastBallot
    :: (MonadPhase m, HasContextOf Leader Fast m)
    => Microsecond -> m ()
initFastBallot recoveryDelay = do
    logInfo "Starting new fast ballot"

    bal <- withProcessState $ do
        newBallotId <- leaderBallotId . as <+= 1
        pure newBallotId

    schedule (after recoveryDelay) $
        startRecoveryIfNecessary bal

    broadcastTo (processesAddresses Acceptor) (InitFastBallotMsg bal)

-- ** Phase 2

phase2a
    :: forall pv m.
       (MonadPhase m, HasContextOf Leader pv m)
    => Phase1bMsg -> m ()
phase2a (Phase1bMsg accId bal cstruct) = do
    maybeMsg <- withProcessState $ do
        -- add received vote to set of votes stored locally for this ballot,
        -- initializing this set if doesn't exist yet
        newVotes <-
            leaderVotes . at bal . non mempty <%= addVote accId cstruct

        -- if some quorums appeared, recalculate Gamma and apply pending policiesToApply
        if isQuorum newVotes
        then do  -- TODO: don't hurry
            when (isMinQuorum newVotes) $
                logInfo $ "Just got 1b from quorum of acceptors at " <> pretty bal

            combined <- throwOnFail ProtocolError $ combination newVotes
            policiesToApply <- use $ leaderPendingPolicies . forClassic . at bal . non mempty
            let cstructWithNewPolicies = foldr acceptOrRejectCommand combined policiesToApply

            logInfo $ "Broadcasting cstruct: " <> show cstructWithNewPolicies
            pure $ Just (Phase2aMsg bal cstructWithNewPolicies)
        else pure Nothing

    -- when got a message to submit - broadcast it
    whenJust maybeMsg $
        broadcastTo (processesAddresses Acceptor)

phase2b
    :: (MonadPhase m, HasContextOf Acceptor pv m)
    => Phase2aMsg -> m ()
phase2b (Phase2aMsg bal cstruct) = do
    maybeMsg <- withProcessState $ do  -- TODO: add "atomically" word
        localBallotId <- use $ acceptorLastKnownBallotId . as
        localCstruct <- use acceptorCStruct

        -- check whether did we promise to ignore this message
        if localBallotId == bal && (cstruct `extends` localCstruct)
            || localBallotId < bal
        then do
           -- if ok, remember received info
           acceptorLastKnownBallotId . as .= bal
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
phase2bFast (InitFastBallotMsg bal) = do
    msg <- withProcessState $ do
        acceptorLastKnownBallotId . as %= max bal

        cstruct <- use acceptorCStruct
        policiesToApply <- use $ acceptorFastPendingPolicies . at bal . non mempty
        logInfo $ sformat ("List of fast pending policies at "%build
                          %":\n    "%listF ", " build)
                  bal policiesToApply
        let cstruct' = foldr acceptOrRejectCommand cstruct policiesToApply
        acceptorCStruct .= cstruct'

        accId <- use acceptorId
        pure $ Phase2bFastMsg bal accId cstruct'

    broadcastTo (processAddresses Leader <> processesAddresses Learner) msg

-- ** Learning

-- | Update learned value with all checks and cautions.
updateLearnedValue :: Configuration -> TransactionM (ProcessState Learner pv) ()
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
  where
    reportErrorBadLearnedCStruct prev new =
        logError $
        sformat ("Newly learned cstruct doesn't extend previous one:\n"%
                 "\t"%build%"\n\t->\n\t"%build)
                prev new
    reportNewLearnedCStruct new =
        logInfo $
        sformat ("New learned cstruct: "%build) new

warnOnPartUpdate :: MonadLog m => Configuration -> Configuration -> m ()
warnOnPartUpdate incoming updated = do  -- TODO: normal name
    unless (updated `extends` incoming) $  -- TODO: and normal comment below
        logInfo $ sformat ("Incoming cstruct was (partly) dropped:\
                        \\n  incoming:  "%build%
                        "\n  new value: "%build)
              incoming updated

learn
    :: (MonadPhase m, HasContextOf Learner pv m)
    => Phase2bMsg -> m ()
learn (Phase2bMsg accId cstruct) = do
    withProcessState $ do
        -- rewrite cstruct kept for this acceptor

        -- we should check here that new cstruct extends previous one.
        -- but the contrary is not an error, because of not-FIFO channels
        updated <- learnerVotes . at accId . non mempty <%= maxOrSecond cstruct
        warnOnPartUpdate cstruct updated

        -- update total learned cstruct
        use learnerVotes
            >>= throwOnFail ProtocolError . combination
            >>= updateLearnedValue

learnFast
    :: (MonadPhase m, HasContextOf Learner Fast m)
    => Phase2bFastMsg -> m ()
learnFast (Phase2bFastMsg _ accId cstruct) = do
    withProcessState $ do
        updated <- learnerVotes . at accId . non mempty <%= maxOrSecond cstruct
        warnOnPartUpdate cstruct updated

        -- it's safe to learn votes from fast round even if recovery is going happen,
        -- because recovery deals with liveleness, correctness always holds.
        use learnerVotes
             >>= throwOnFail ProtocolError . intersectingCombination
             >>= updateLearnedValue

-- ** Recovery detection and initialition

delegateToRecovery
    :: (MonadPhase m, HasContextOf Leader Fast m)
    => AcceptorId -> BallotId -> Configuration -> m ()
delegateToRecovery accId bal cstruct = do
    let recoveryBallotId = bal
    phase2a (Phase1bMsg accId recoveryBallotId cstruct)

startRecoveryIfNecessary
    :: (MonadPhase m, HasContextOf Leader Fast m)
    => BallotId -> m ()
startRecoveryIfNecessary bal = do
    (needRecovery, unconfirmedPolicies) <- withProcessState $ do
        fastBallotStatus <- use $ leaderFastBallotStatus . at bal . non def

        case fastBallotStatus of
            FastBallotInProgress -> do
                votes <- use $ leaderFastVotes . at bal . non mempty
                combined <- throwOnFail ProtocolError $ intersectingCombination votes
                let mentionedPolicyEntries = fold votes
                    needRecovery = combined /= mentionedPolicyEntries
                    unconfirmedPolicies = mentionedPolicyEntries `S.difference` combined

                leaderFastBallotStatus . at bal . non def .=
                    if needRecovery then FastBallotInRecovery else FastBallotSucceeded
                return (needRecovery, unconfirmedPolicies)
            _ -> pure (False, mempty)

    when needRecovery $ do
        logInfo $ sformat ("Recovery in "%build%" initiated!") bal
        logInfo $ sformat ("Unconfirmed policies: "%build)
                  unconfirmedPolicies

        Votes votes <- withProcessState $ do
            leaderRecoveryUsed . at bal ?= ()
            use $ leaderFastVotes . at bal . non mempty

        -- artificially execute next available phase of recovery (classic) Paxos
        forM_ (M.toList votes) $ \(accId, cstruct) ->
             delegateToRecovery accId bal cstruct

detectConflicts
    :: (MonadPhase m, HasContextOf Leader Fast m)
    => Phase2bFastMsg -> m ()
detectConflicts (Phase2bFastMsg bal accId cstruct) = do
    newVotes <- withProcessState $ do
        leaderFastVotes . at bal . non mempty <%= addVote accId cstruct

    if void newVotes == maxBound
        -- all possible votes collected - ready to check whether recovery required
        then startRecoveryIfNecessary bal
        -- if recovery already occured - delegate to phase2a of classic paxos
        else do
            fastBallotStatus <-
                withProcessState . use $ leaderFastBallotStatus . at bal . non def
            case fastBallotStatus of
                FastBallotInProgress -> pass
                FastBallotSucceeded  -> pass
                FastBallotInRecovery -> delegateToRecovery accId bal cstruct




-- | Phases of Fast Paxos.

module Sdn.Protocol.Fast.Phases
    ( propose
    , acceptorRememberProposal
    , initBallot
    , phase2b
    , learn
    , detectConflicts
    ) where

import           Control.Lens                  (at, has, ix, non, (.=), (<%=), (<+=),
                                                (<>=))
import           Control.TimeWarp.Timed        (Microsecond, after, schedule)
import           Data.Default                  (def)
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Formatting                    (build, sformat, stext, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra                     (exists, exit, listF, logInfo, presence,
                                                rightSpaced, throwOnFail, zoom)
import qualified Sdn.Protocol.Classic.Messages as Classic
import qualified Sdn.Protocol.Classic.Phases   as Classic
import           Sdn.Protocol.Common.Phases
import           Sdn.Protocol.Context
import qualified Sdn.Protocol.Fast.Messages    as Fast
import           Sdn.Protocol.Processes
import           Sdn.Protocol.Versions

-- * Proposal

propose
    :: (MonadPhase m, HasContextOf Proposer Fast m)
    => Policy -> m ()
propose policy = do
    logInfo $ sformat ("Proposing policy (fast): "%build) policy
    withProcessStateAtomically $ do
        proposerProposedPolicies <>= one policy
        proposerUnconfirmedPolicies <>= one policy
    broadcastTo (processesAddresses Acceptor)
                (Fast.ProposalMsg policy)

-- * Remembering proposals

acceptorRememberProposal
    :: (MonadPhase m, HasContextOf Acceptor Fast m)
    => Fast.ProposalMsg -> m ()
acceptorRememberProposal (Fast.ProposalMsg policy) =
    withProcessStateAtomically $ do
        committed <- exists $ acceptorCStruct . forFast . (ix (Accepted policy) <> ix (Rejected policy))

        -- proposer may be insistent,
        -- this check allows to avoid performing extra actions
        if committed
        then
            logInfo $ sformat ("Policy "%build%" has already been committed") policy
        else do
            bal <- fmap (+1) . use $ acceptorLastKnownBallotId
            logInfo $ sformat (build%" to apply at "%build) policy bal
            acceptorFastProposedPolicies . pendingProposedCommands <>= one policy

-- * Ballot initiation

initBallot
    :: (MonadPhase m, HasContextOf Leader Fast m)
    => Microsecond -> m ()
initBallot recoveryDelay = do
    logInfo "Starting new fast ballot"

    newBalId <- withProcessStateAtomically $ do
        leaderBallotId <+= 1

    schedule (after recoveryDelay) $
        startRecoveryIfNecessary newBalId

    broadcastTo (processesAddresses Acceptor) (Fast.InitBallotMsg newBalId)

-- * Phase 2

phase2b
    :: (MonadPhase m, HasContextOf Acceptor Fast m)
    => Fast.InitBallotMsg -> m ()
phase2b (Fast.InitBallotMsg bal) = do
    msg <- withProcessStateAtomically . runMaybeT $ do
        lastKnownBal <- use acceptorLastKnownBallotId
        when (bal <= lastKnownBal) $ do
             logInfo "Already heard about this ballot, ignoring"
             exit

        acceptorLastKnownBallotId .= bal

        policiesToApply <-
            zoom acceptorFastProposedPolicies $
            dumpProposedCommands bal
        appliedPolicies <-
            zoom (acceptorCStruct . forFast) $
            mapM acceptOrRejectCommandS policiesToApply

        logInfo $
            sformat ("List of fast applied policies at "%build
                    %":\n    "%listF ", " build)
                bal appliedPolicies

        accId <- use acceptorId
        newCStruct <- use $ acceptorCStruct . forFast
        pure $ Fast.Phase2bMsg bal accId newCStruct

    whenJust msg $
        broadcastTo (processAddresses Leader <> processesAddresses Learner)

-- * Learning

learn
    :: (MonadPhase m, HasContextOf Learner Fast m)
    => Fast.Phase2bMsg -> m ()
learn (Fast.Phase2bMsg _ accId cstruct) =
    learnCStruct intersectingCombination accId cstruct

-- * Recovery detection and initialition

-- | In fast round, if recovery has been initiated, leader on fast 2b messages
-- acts like if it received classic 1b messsage.
delegateToRecovery
    :: (MonadPhase m, HasContextOf Leader Fast m)
    => AcceptorId -> BallotId -> Configuration -> m ()
delegateToRecovery accId bal cstruct = do
    let recoveryBallotId = bal
    Classic.phase2a (Classic.Phase1bMsg accId recoveryBallotId cstruct)

startRecoveryIfNecessary
    :: (MonadPhase m, HasContextOf Leader Fast m)
    => BallotId -> m ()
startRecoveryIfNecessary bal = do
    mVotesSoFar <- withProcessStateAtomically . runMaybeT $ do
        -- skip check if ballot status is ok or recovery already happened
        fastBallotStatus <- use $ leaderFastBallotStatus . at bal . non def
        unless (_FastBallotInProgress `has` fastBallotStatus) $ do
            exit

        -- check need in recovery
        votes <- use $ leaderFastVotes . at bal . non mempty
        unconfirmedPolicies <- evalUnconfirmedPolicies votes
        let needRecovery = not $ null unconfirmedPolicies

        when needRecovery $ do
            logInfo $ sformat ("Need in recovery just was checked (for ballot "%build%")") bal
            logInfo $ sformat ("Unconfirmed policies: "%build)
                    unconfirmedPolicies
            logInfo $ sformat ("Recovery is "%rightSpaced stext%" needed")
                      (if needRecovery then "" else "not")

        -- update state
        leaderFastBallotStatus . at bal . non def .=
            if needRecovery then FastBallotInRecovery else FastBallotSucceeded
        when needRecovery $
            leaderRecoveryUsed . at bal . presence .= True

        use $ leaderFastVotes . at bal . non mempty

    whenJust mVotesSoFar $ \(Votes votes) ->
        -- artificially execute next available phase of recovery (classic) round
        forM_ (M.toList votes) $ \(accId, cstruct) ->
             delegateToRecovery accId bal cstruct
  where
    -- find policies which haven't gathered a quorum of votes
    evalUnconfirmedPolicies votes = do
        combined <- throwOnFail ProtocolError $ intersectingCombination votes
        let mentionedPolicyEntries = fold votes
            unconfirmedPolicies = mentionedPolicyEntries `S.difference` combined
        return unconfirmedPolicies


detectConflicts
    :: (MonadPhase m, HasContextOf Leader Fast m)
    => Fast.Phase2bMsg -> m ()
detectConflicts (Fast.Phase2bMsg bal accId cstruct) = do
    newVotes <- withProcessStateAtomically $ do
        leaderFastVotes . at bal . non mempty <%= addVote accId cstruct

    if void newVotes == maxBound
        -- all possible votes collected - ready to check whether recovery required
        then startRecoveryIfNecessary bal
        -- if recovery already occured - delegate to phase2a of classic paxos
        else do
            fastBallotStatus <-
                withProcessStateAtomically . use $ leaderFastBallotStatus . at bal . non def
            case fastBallotStatus of
                FastBallotInProgress -> pass
                FastBallotSucceeded  -> pass
                FastBallotInRecovery -> delegateToRecovery accId bal cstruct

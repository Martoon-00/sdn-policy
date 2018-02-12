-- | Phases of Fast Paxos.

module Sdn.Protocol.Fast.Phases
    ( propose
    , acceptorRememberProposal
    , initBallot
    , phase2b
    , learn
    , detectConflicts
    ) where

import           Control.Lens                  (at, non, (%=), (.=), (<%=), (<+=), (<>=),
                                                (?=))
import           Control.TimeWarp.Timed        (Microsecond, after, schedule)
import           Data.Default                  (def)
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Formatting                    (build, sformat, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra                     (as, listF, logInfo, throwOnFail)
import           Sdn.Protocol.Classic.Messages
import           Sdn.Protocol.Classic.Phases   (phase2a)
import           Sdn.Protocol.Common.Phases
import           Sdn.Protocol.Context
import           Sdn.Protocol.Fast.Messages
import           Sdn.Protocol.Processes
import           Sdn.Protocol.Versions

-- Some Phases of Classic Paxos are also used, but not mentioned here.

-- * Proposal

propose
    :: (MonadPhase m, HasContextOf Proposer Fast m)
    => Policy -> m ()
propose policy = do
    logInfo $ sformat ("Proposing policy (fast): "%build) policy
    withProcessState $
        proposerProposedPolicies <>= one policy
    broadcastTo (processAddresses Leader <> processesAddresses Acceptor)
                (ProposalFastMsg policy)

-- * Remembering proposals

acceptorRememberProposal
    :: (MonadPhase m, HasContextOf Acceptor Fast m)
    => ProposalFastMsg -> m ()
acceptorRememberProposal (ProposalFastMsg policy) =
    withProcessState $ do
        bal <- fmap (+1) . use $ acceptorLastKnownBallotId . as
        logInfo $ sformat (build%" to apply at "%build) policy bal
        acceptorFastPendingPolicies . at bal . non mempty <>= one policy

-- * Ballot initiation

initBallot
    :: (MonadPhase m, HasContextOf Leader Fast m)
    => Microsecond -> m ()
initBallot recoveryDelay = do
    logInfo "Starting new fast ballot"

    bal <- withProcessState $ do
        newBallotId <- leaderBallotId . as <+= 1
        pure newBallotId

    schedule (after recoveryDelay) $
        startRecoveryIfNecessary bal

    broadcastTo (processesAddresses Acceptor) (InitFastBallotMsg bal)

-- * Phase 2

phase2b
    :: (MonadPhase m, HasContextOf Acceptor Fast m)
    => InitFastBallotMsg -> m ()
phase2b (InitFastBallotMsg bal) = do
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

-- * Learning

learn
    :: (MonadPhase m, HasContextOf Learner Fast m)
    => Phase2bFastMsg -> m ()
learn (Phase2bFastMsg _ accId cstruct) = do
    withProcessState $ do
        updated <- learnerVotes . at accId . non mempty <%= maxOrSecond cstruct
        warnOnPartUpdate cstruct updated

        -- it's safe to learn votes from fast round even if recovery is going happen,
        -- because recovery deals with liveleness, correctness always holds.
        use learnerVotes
             >>= throwOnFail ProtocolError . intersectingCombination
             >>= updateLearnedValue

-- * Recovery detection and initialition

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





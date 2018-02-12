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
import           Sdn.Extra                     (listF, logInfo, throwOnFail)
import qualified Sdn.Protocol.Classic.Messages as Classic
import qualified Sdn.Protocol.Classic.Phases   as Classic
import           Sdn.Protocol.Common.Phases
import           Sdn.Protocol.Context
import qualified Sdn.Protocol.Fast.Messages    as Fast
import           Sdn.Protocol.Processes
import           Sdn.Protocol.Versions

-- Some Phases of Classic Paxos are also used, but not mentioned here.

-- * Proposal

propose
    :: (MonadPhase m, HasContextOf Proposer Fast m)
    => Policy -> m ()
propose policy = do
    logInfo $ sformat ("Proposing policy (fast): "%build) policy
    withProcessStateAtomically $
        proposerProposedPolicies <>= one policy
    broadcastTo (processAddresses Leader <> processesAddresses Acceptor)
                (Fast.ProposalMsg policy)

-- * Remembering proposals

acceptorRememberProposal
    :: (MonadPhase m, HasContextOf Acceptor Fast m)
    => Fast.ProposalMsg -> m ()
acceptorRememberProposal (Fast.ProposalMsg policy) =
    withProcessStateAtomically $ do
        bal <- fmap (+1) . use $ acceptorLastKnownBallotId
        logInfo $ sformat (build%" to apply at "%build) policy bal
        acceptorFastPendingPolicies . at bal . non mempty <>= one policy

-- * Ballot initiation

initBallot
    :: (MonadPhase m, HasContextOf Leader Fast m)
    => Microsecond -> m ()
initBallot recoveryDelay = do
    logInfo "Starting new fast ballot"

    bal <- withProcessStateAtomically $ do
        newBallotId <- leaderBallotId <+= 1
        pure newBallotId

    schedule (after recoveryDelay) $
        startRecoveryIfNecessary bal

    broadcastTo (processesAddresses Acceptor) (Fast.InitBallotMsg bal)

-- * Phase 2

phase2b
    :: (MonadPhase m, HasContextOf Acceptor Fast m)
    => Fast.InitBallotMsg -> m ()
phase2b (Fast.InitBallotMsg bal) = do
    msg <- withProcessStateAtomically $ do
        acceptorLastKnownBallotId %= max bal

        cstruct <- use acceptorCStruct
        policiesToApply <- use $ acceptorFastPendingPolicies . at bal . non mempty
        logInfo $ sformat ("List of fast pending policies at "%build
                          %":\n    "%listF ", " build)
                  bal policiesToApply
        let cstruct' = foldr acceptOrRejectCommand cstruct policiesToApply
        acceptorCStruct .= cstruct'

        accId <- use acceptorId
        pure $ Fast.Phase2bMsg bal accId cstruct'

    broadcastTo (processAddresses Leader <> processesAddresses Learner) msg

-- * Learning

learn
    :: (MonadPhase m, HasContextOf Learner Fast m)
    => Fast.Phase2bMsg -> m ()
learn (Fast.Phase2bMsg _ accId cstruct) = do
    withProcessStateAtomically $ do
        updated <- learnerVotes . at accId . non mempty <%= maxOrSecond cstruct
        warnOnPartialApply cstruct updated

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
    Classic.phase2a (Classic.Phase1bMsg accId recoveryBallotId cstruct)

startRecoveryIfNecessary
    :: (MonadPhase m, HasContextOf Leader Fast m)
    => BallotId -> m ()
startRecoveryIfNecessary bal = do
    (needRecovery, unconfirmedPolicies) <- withProcessStateAtomically $ do
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

        Votes votes <- withProcessStateAtomically $ do
            leaderRecoveryUsed . at bal ?= ()
            use $ leaderFastVotes . at bal . non mempty

        -- artificially execute next available phase of recovery (classic) Paxos
        forM_ (M.toList votes) $ \(accId, cstruct) ->
             delegateToRecovery accId bal cstruct

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





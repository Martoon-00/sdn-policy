-- | Phases of Classic Paxos.

module Sdn.Protocol.Classic.Phases
    ( propose
    , rememberProposal
    , phase1a
    , phase1b
    , phase2a
    , phase2b
    , learn
    ) where

import           Control.Lens                  (at, non, (%=), (.=), (<%=), (<+=), (<>=))
import           Formatting                    (build, sformat, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra                     (exit, listF, logInfo, submit, throwOnFail,
                                                zoom)
import           Sdn.Protocol.Classic.Messages
import           Sdn.Protocol.Common.Phases
import           Sdn.Protocol.Context
import           Sdn.Protocol.Processes
import           Sdn.Protocol.Versions


-- * Proposal

propose
    :: (MonadPhase m, HasContextOf Proposer Classic m)
    => Policy -> m ()
propose policy = do
    logInfo $ sformat ("Proposing policy: "%build) policy
    -- remember policy (for testing purposes)
    withProcessStateAtomically $ do
        proposerProposedPolicies <>= one policy
        proposerUnconfirmedPolicies <>= one policy
    -- and send it to leader
    broadcastTo (processAddresses Leader) (ProposalMsg policy)

-- * Remembering proposals

rememberProposal
    :: (MonadPhase m, HasContextOf Leader pv m)
    => ProposalMsg -> m ()
rememberProposal (ProposalMsg policy) = do
    -- atomically modify process'es state
    withProcessStateAtomically $ do
        leaderProposedPolicies . forClassic . pendingProposedCommands %= (policy :)

-- * Phase 1

phase1a
    :: (MonadPhase m, HasContextOf Leader pv m)
    => m ()
phase1a = do
    logInfo "Starting new ballot"

    msg <- withProcessStateAtomically $ do
        -- increment ballot id
        newBallotId <- leaderBallotId <+= 1
        -- fixate pending policies as attached to newly started ballot
        _ <- zoom (leaderProposedPolicies . forClassic) $
             dumpProposedCommands newBallotId
        -- make up an "1a" message
        Phase1aMsg <$> use leaderBallotId

    broadcastTo (processesAddresses Acceptor) msg

phase1b
    :: (MonadPhase m, HasContextOf Acceptor pv m)
    => Phase1aMsg -> m ()
phase1b (Phase1aMsg bal) = do
    msg <- withProcessStateAtomically $ do
        -- promise not to accept messages of lesser ballot numbers
        -- make stored ballot id not lesser than @bal@
        acceptorLastKnownBallotId %= max bal
        Phase1bMsg
            <$> use acceptorId
            <*> use acceptorLastKnownBallotId
            <*> use (acceptorCStruct . forClassic)

    submit (processAddress Leader) msg

-- * Phase 2

phase2a
    :: (MonadPhase m, HasContextOf Leader pv m)
    => Phase1bMsg -> m ()
phase2a (Phase1bMsg accId bal cstruct) = do
    maybeMsg <- withProcessStateAtomically $ runMaybeT $ do
        -- add received vote to set of votes stored locally for this ballot,
        -- initializing this set if doesn't exist yet
        (oldVotes, newVotes) <- zoom (leaderVotes . at bal . non mempty) $ do
            oldVotes <- get
            newVotes <- identity <%= addVote accId cstruct
            return (oldVotes, newVotes)

        when (isMinQuorum newVotes) $
            logInfo $ "Just got 1b from quorum of acceptors at " <> pretty bal

        -- evaluate old and new Gamma
        oldCombined <- throwOnFail ProtocolError $ combination oldVotes
        newCombined <- throwOnFail ProtocolError $ combination newVotes

        -- if there is something new, recalculate Gamma and apply pending policies
        if isMinQuorum newVotes || oldCombined /= newCombined
        then do
            policiesToApply <-
                    use $ leaderProposedPolicies . forClassic . at bal . non mempty
            let (appliedPolicies, cstructWithNewPolicies) =
                    usingState newCombined $
                    mapM acceptOrRejectCommandS policiesToApply

            logInfo $ sformat ("Applied policies at "%build%": "%listF "," build)
                      bal appliedPolicies
            logInfo $ "Broadcasting new cstruct: " <> pretty cstructWithNewPolicies

            pure $ Phase2aMsg bal cstructWithNewPolicies
        else exit

    -- when got a message to submit - broadcast it
    whenJust maybeMsg $
        broadcastTo (processesAddresses Acceptor)

phase2b
    :: (MonadPhase m, HasContextOf Acceptor pv m)
    => Phase2aMsg -> m ()
phase2b (Phase2aMsg bal cstruct) = do
    maybeMsg <- withProcessStateAtomically $ runMaybeT $ do
        localBallotId <- use $ acceptorLastKnownBallotId
        localCstruct <- use $ acceptorCStruct . forClassic

        -- check whether did we promise to ignore this message
        let meetsPromise =
                localBallotId < bal ||
                localBallotId == bal && (cstruct `extends` localCstruct)
        unless meetsPromise $ do
           logInfo "Received cstruct was rejected"
           exit

        -- if ok, remember received info
        acceptorLastKnownBallotId .= bal
        acceptorCStruct . forClassic .= cstruct

        -- form message
        accId <- use acceptorId
        pure $ Phase2bMsg accId cstruct

    whenJust maybeMsg $
        broadcastTo (processesAddresses Learner)

-- * Learning

learn
    :: (MonadPhase m, HasContextOf Learner pv m)
    => LearningCallback m -> Phase2bMsg -> m ()
learn callback (Phase2bMsg accId cstruct) = learnCStruct callback combination accId cstruct

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
import           Sdn.Extra                     (logInfo, submit, throwOnFail, zoom)
import           Sdn.Protocol.Classic.Messages
import           Sdn.Protocol.Common.Messages
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
    :: forall pv m.
       (MonadPhase m, HasContextOf Leader pv m)
    => m ()
phase1a = do
    logInfo "Starting new ballot"

    msg <- withProcessStateAtomically $ do
        -- increment ballot id
        newBallotId <- leaderBallotId <+= 1
        -- fixate pending policies as attached to just started ballot
        _ <- zoom (leaderProposedPolicies . forClassic) $
             dumpProposedCommands newBallotId
        -- make up an "1a" message
        Phase1aMsg <$> use leaderBallotId

    broadcastTo (processesAddresses Acceptor) msg

phase1b
    :: forall pv m.
       (MonadPhase m, HasContextOf Acceptor pv m)
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
    :: forall pv m.
       (MonadPhase m, HasContextOf Leader pv m)
    => Phase1bMsg -> m ()
phase2a (Phase1bMsg accId bal cstruct) = do
    maybeMsg <- withProcessStateAtomically $ do
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

        -- if there is something new, recalculate Gamma and apply pending policiesToApply
        if isMinQuorum newVotes || oldCombined /= newCombined
        then do
            policiesToApply <- use $ leaderProposedPolicies . forClassic . at bal . non mempty
            let cstructWithNewPolicies = foldr acceptOrRejectCommand newCombined policiesToApply

            logInfo $ "Broadcasting new cstruct: " <> show cstructWithNewPolicies
            pure $ Just (Phase2aMsg bal cstructWithNewPolicies)
        else pure Nothing

    -- when got a message to submit - broadcast it
    whenJust maybeMsg $
        broadcastTo (processesAddresses Acceptor)

phase2b
    :: (MonadPhase m, HasContextOf Acceptor pv m)
    => Phase2aMsg -> m ()
phase2b (Phase2aMsg bal cstruct) = do
    maybeMsg <- withProcessStateAtomically $ do
        localBallotId <- use $ acceptorLastKnownBallotId
        localCstruct <- use $ acceptorCStruct . forClassic

        -- check whether did we promise to ignore this message
        if localBallotId == bal && (cstruct `extends` localCstruct)
            || localBallotId < bal
        then do
           -- if ok, remember received info
           acceptorLastKnownBallotId .= bal
           acceptorCStruct . forClassic .= cstruct

           -- form message
           accId <- use acceptorId
           pure $ Just (Phase2bMsg accId cstruct)
        else do
           logInfo "Received cstruct was rejected"
           pure Nothing

    whenJust maybeMsg $
        broadcastTo (processesAddresses Learner)

-- * Learning

learn
    :: (MonadPhase m, HasContextOf Learner pv m)
    => Phase2bMsg -> m ()
learn (Phase2bMsg accId cstruct) = do
    newLearnedPolicies <- withProcessStateAtomically $ do
        -- rewrite cstruct kept for this acceptor

        -- we should check here that new cstruct extends previous one.
        -- but the contrary is not an error, because of not-FIFO channels
        updated <- learnerVotes . at accId . non mempty <%= maxOrSecond cstruct
        warnOnPartialApply cstruct updated

        -- update total learned cstruct
        learnedDifference <-
            use learnerVotes
            >>= throwOnFail ProtocolError . combination
            >>= updateLearnedValue

        return $ map acceptanceCmd learnedDifference

    whenNotNull newLearnedPolicies $ \policies ->
        submit (processAddress Proposer) (CommittedMsg policies)


{-# LANGUAGE TypeFamilies #-}

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

import           Control.Lens                  (at, non, (%=), (.=), (<+=))
import qualified Data.Set                      as S
import           Formatting                    (build, sformat, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra                     (OldNew (..), broadcastTo, exit, listF,
                                                logInfo, submit, throwOnFail, wasChanged,
                                                zoom, (<<<%=))
import           Sdn.Protocol.Classic.Messages
import           Sdn.Protocol.Common.Context
import           Sdn.Protocol.Common.Phases
import           Sdn.Protocol.Processes
import           Sdn.Protocol.Versions


-- * Proposal

propose
    :: forall cstruct m.
       (MonadPhase cstruct m, HasContextOf Proposer Classic m)
    => NonEmpty (RawCmd cstruct) -> m ()
propose policies = do
    logInfo $ sformat ("Proposing policies: "%listF "," build) policies
    -- remember policy (for testing purposes)
    withProcessStateAtomically $ do
        proposerProposedPolicies %= (toList policies <>)
        proposerUnconfirmedPolicies %= \s -> foldl (flip S.insert) s policies
    -- and send it to leader
    broadcastTo (processAddresses Leader) (ProposalMsg @cstruct policies)

-- * Remembering proposals

rememberProposal
    :: (MonadPhase cstruct m, HasContextOf Leader pv m)
    => ProposalMsg cstruct -> m ()
rememberProposal (ProposalMsg policies) = do
    -- atomically modify process'es state
    withProcessStateAtomically $ do
        leaderProposedPolicies . forClassic . pendingProposedCommands %= (toList policies <>)

-- * Phase 1

phase1a
    :: (MonadPhase cstruct m, HasContextOf Leader pv m)
    => m ()
phase1a = do
    logInfo "Starting new ballot"

    mmsg <- withProcessStateAtomically $ runMaybeT $ do
        -- increment ballot id
        newBallotId <- leaderBallotId <+= 1
        -- fixate pending policies as attached to newly started ballot
        curBallotProposals <-
             zoom (leaderProposedPolicies . forClassic) $
             dumpProposedCommands newBallotId
        -- don't publicly start ballot if there is no proposals
        when (null curBallotProposals) $ do
            logInfo $
                sformat ("Skipping "%build%" (no proposals)")
                newBallotId
            exit
        -- make up an "prepare" message
        PrepareMsg <$> use leaderBallotId

    -- if decided to initiate ballot, notify acceptors about its start
    whenJust mmsg $
        broadcastTo (processesAddresses Acceptor)

phase1b
    :: (MonadPhase cstruct m, HasContextOf Acceptor pv m)
    => PrepareMsg -> m ()
phase1b (PrepareMsg bal) = do
    msg <- withProcessStateAtomically $ do
        -- promise not to accept messages of lesser ballot numbers
        -- make stored ballot id not lesser than @bal@
        acceptorLastKnownBallotId %= max bal
        PromiseMsg
            <$> use acceptorId
            <*> use acceptorLastKnownBallotId
            <*> use (acceptorCStruct . forClassic)

    submit (processAddress Leader) msg

-- * Phase 2

phase2a
    :: (MonadPhase cstruct m, HasContextOf Leader pv m)
    => PromiseMsg cstruct -> m ()
phase2a (PromiseMsg accId bal cstruct) = do
    maybeMsg <- withProcessStateAtomically $ runMaybeT $ do
        -- add received vote to set of votes stored locally for this ballot,
        -- initializing this set if doesn't exist yet
        votes <- leaderVotes . at bal . non mempty <<<%= addVote accId cstruct

        when (isMinQuorum $ getNew votes) $
            logInfo $ "Just got 1b from quorum of acceptors at " <> pretty bal

        -- evaluate old and new Gamma
        combined <- forM votes $ throwOnFail ProtocolError . combination

        -- if there is something new, recalculate Gamma and apply pending policies
        if isMinQuorum (getNew votes) || wasChanged combined
        then do
            policiesToApply <-
                    use $ leaderProposedPolicies . forClassic . at bal . non mempty
            let (appliedPolicies, cstructWithNewPolicies) =
                    usingState (getNew combined) $
                    mapM acceptOrRejectCommandS policiesToApply

            logInfo $ sformat ("Applied policies at "%build%": "%listF "," build)
                      bal appliedPolicies
            logInfo $ "Broadcasting new cstruct: " <> pretty cstructWithNewPolicies

            pure $ AcceptRequestMsg bal cstructWithNewPolicies
        else exit

    -- when got a message to submit - broadcast it
    whenJust maybeMsg $
        broadcastTo (processesAddresses Acceptor)

phase2b
    :: (MonadPhase cstruct m, HasContextOf Acceptor pv m)
    => AcceptRequestMsg cstruct -> m ()
phase2b (AcceptRequestMsg bal cstruct) = do
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
        pure $ AcceptedMsg accId cstruct

    whenJust maybeMsg $
        broadcastTo (processesAddresses Learner)

-- * Learning

learn
    :: (MonadPhase cstruct m, HasContextOf Learner pv m)
    => LearningCallback m -> AcceptedMsg cstruct -> m ()
learn callback (AcceptedMsg accId cstruct) = learnCStruct callback combination accId cstruct

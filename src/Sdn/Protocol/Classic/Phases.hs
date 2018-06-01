{-# LANGUAGE Rank2Types   #-}
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

import           Control.Lens                  (at, non, to, (%=), (.=), (<+=))
import qualified Data.Set                      as S
import           Formatting                    (build, sformat, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra                     (OldNew (..), broadcastTo, exit, foldlF',
                                                listF, logInfo, pairF, submit,
                                                throwOnFail, wasChanged, zoom, (<<<%=))
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
        proposerUnconfirmedPolicies %= foldlF' S.insert policies
    -- and send it to leader
    broadcastTo (processAddresses Leader) (ProposalMsg @cstruct policies)

-- * Remembering proposals

rememberProposal
    :: (MonadPhase cstruct m, HasContextOf Leader pv m)
    => ProposalMsg cstruct -> m ()
rememberProposal (ProposalMsg policies) = do
    -- atomically modify process'es state
    let policiesSet = S.fromList $ toList policies
    withProcessStateAtomically $ do
        leaderProposedPolicies . pendingProposedCommands %= (policiesSet <>)

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
             zoom leaderProposedPolicies $
             dumpProposedCommands newBallotId
        -- get policies advised at fast ballots
        hintPolicies <- use leaderHintPolicies
        -- don't publicly start ballot if there is no proposals
        when (null curBallotProposals && null hintPolicies) $ do
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
            <*> use (acceptorCStruct . to totalCStruct)

    submit (processAddress Leader) msg

-- * Phase 2

-- | Take hint policies and pending policies and apply them to given cstruct.
applyWaitingPolicies
    :: PracticalCStruct cstruct
    => BallotId -> cstruct -> TransactionM (LeaderState pv cstruct) cstruct
applyWaitingPolicies bal cstruct0 = do
    hintPolicies <- use leaderHintPolicies
    let (unduePolicies, cstruct1) = applyHintCommands (toList hintPolicies) cstruct0
    leaderHintPolicies %= foldlF' S.delete (map snd unduePolicies)
    logInfo $ logUndueHints unduePolicies

    pendingPolicies <- use $ leaderProposedPolicies . at bal . non mempty
    let (appliedPolicies, cstruct2) = acceptOrRejectCommands pendingPolicies cstruct1
    logInfo $ logAppliedPending bal appliedPolicies

    return cstruct2
  where
    logAppliedPending =
        sformat ("Applied policies at "%build%": "%listF "," build)
    logUndueHints =
        sformat ("Undue hints on policies: "%listF ", " (pairF (build%" ("%build%")")))

phase2a
    :: (MonadPhase cstruct m, HasContextOf Leader pv m)
    => PromiseMsg cstruct -> m ()
phase2a (PromiseMsg accId bal cstruct) = do
    maybeMsg <- withProcessStateAtomically $ runMaybeT $ do
        -- add received vote to set of votes stored locally for this ballot,
        -- initializing this set if doesn't exist yet
        oldnewVotes <- leaderVotes . at bal . non mempty <<<%= addVote accId cstruct

        when (isMinQuorum $ getNew oldnewVotes) $
            logInfo $ "Just got 1b from quorum of acceptors at " <> pretty bal

        -- evaluate old and new Gamma
        oldnewCombined <- forM oldnewVotes $ throwOnFail ProtocolError . combination

        -- if there is something new, recalculate Gamma and apply pending policies
        if isMinQuorum (getNew oldnewVotes) || wasChanged oldnewCombined
        then do
            cstructWithNewPolicies <- lift $ applyWaitingPolicies bal (getNew oldnewCombined)

            logInfo $ "Broadcasting new cstruct: " <> pretty cstructWithNewPolicies
            pure $ AcceptRequestMsg bal cstructWithNewPolicies
        else exit

    -- when got a message to submit - broadcast it
    whenJust maybeMsg $
        broadcastTo (processesAddresses Acceptor)
  where

phase2b
    :: (MonadPhase cstruct m, HasContextOf Acceptor pv m)
    => AcceptRequestMsg cstruct -> m ()
phase2b (AcceptRequestMsg bal cstruct) = do
    maybeMsg <- withProcessStateAtomically $ runMaybeT $ do
        localBallotId <- use $ acceptorLastKnownBallotId
        coreCstruct <- use $ acceptorCStruct . to coreCStruct

        -- Check whether did we promise to ignore this message.
        -- Case when we get message from this ballot is also checked,
        -- because leader can submit updates during ballot, although
        -- they are not oblidged to receive in FIFO.
        let meetsPromise =
                localBallotId < bal ||
                localBallotId == bal && (cstruct `extends` coreCstruct)
        unless meetsPromise $ do
           logInfo "Received cstruct was rejected"
           exit

        -- if ok, remember received info
        acceptorLastKnownBallotId .= bal

        use acceptorCStruct
            >>= throwOnFail ProtocolError . extendCoreCStruct cstruct
            >>= (acceptorCStruct .=)

        -- form message
        AcceptedMsg
            <$> use acceptorId
            <*> use (acceptorCStruct . to totalCStruct)

    whenJust maybeMsg $
        broadcastTo (processesAddresses Learner)

-- * Learning

learn
    :: (MonadPhase cstruct m, HasContextOf Learner pv m)
    => LearningCallback m -> AcceptedMsg cstruct -> m ()
learn callback (AcceptedMsg accId cstruct) = learnCStruct callback combination accId cstruct

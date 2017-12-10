-- | Phases of algorithm

module Sdn.Protocol.Phases where

import           Control.Lens           (at, non, (%=), (+=), (.=), (<%=), (<<.=), (<>=))
import           Control.TimeWarp.Rpc   (MonadRpc)
import           Control.TimeWarp.Timed (MonadTimed (..))
import           Formatting             (build, sformat, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra
import           Sdn.Protocol.Context
import           Sdn.Protocol.Messages
import           Sdn.Protocol.Processes

-- * Commons

-- | Common constraints for all phrases.
type MonadPhase m =
    ( MonadIO m
    , MonadCatch m
    , MonadTimed m
    , MonadRpc m
    , MonadLog m
    , MonadReporting m
    , HasMembers
    )

-- | Evaluate cstruct with all those policies, which are present
-- in votes from all acceptors of some quorum.
--
-- NOTE: This is _very_ inneficient for now, because it treats policies
-- in abstract way and literally sorts out all the minimal-on-inclusion quorums
-- to evaluate cstruct.
combinateOrThrow
    :: ( MonadThrow m
       , Command cstruct policy
       , Buildable cstruct
       , Show cstruct
       , HasMembers
       , QuorumFamily f
       )
    => Votes f cstruct -> m cstruct
combinateOrThrow votes =
    either (throwM . ProtocolError) pure $ combination votes

-- * Phases

propose
    :: (MonadPhase m, HasContextOf Proposer m)
    => Policy -> m ()
propose policy = do
    logInfo $ sformat ("Proposing policy: "%build) policy
    -- remember policy (for testing purposes)
    withProcessState $
        proposerProposedPolicies <>= one policy
    -- and send it policies to leader
    submit (processAddress Leader) (ProposalMsg policy)

rememberProposal
    :: (MonadPhase m, HasContextOf Leader m)
    => ProposalMsg -> m ()
rememberProposal (ProposalMsg policy) = do
    -- atomically modify process'es state
    withProcessState $ do
        -- add policy to pending policies for next ballot
        bal <- (+1) <$> use leaderBallotId
        leaderPendingPolicies . at bal . non mempty %= (policy :)

phrase1a
    :: (MonadPhase m, HasContextOf Leader m)
    => m ()
phrase1a = do
    logInfo "Starting new ballot"

    msg <- withProcessState $ do
        -- increment ballot id
        leaderBallotId += 1
        -- make up an "1a" message
        Phase1aMsg <$> use leaderBallotId

    broadcastTo (processesAddresses Acceptor) msg

phase1b
    :: (MonadPhase m, HasContextOf Acceptor m)
    => Phase1aMsg -> m ()
phase1b (Phase1aMsg ballotId) = do
    msg <- withProcessState $ do
        -- promise not to accept messages of lesser ballot numbers
        -- make stored ballot id not lesser than @ballotId@
        acceptorBallotId %= max ballotId
        Phase1bMsg
            <$> use acceptorId
            <*> use acceptorBallotId
            <*> use acceptorCStruct

    submit (processAddress Leader) msg

phase2a
    :: (MonadPhase m, HasContextOf Leader m)
    => Phase1bMsg -> m ()
phase2a (Phase1bMsg accId ballotId cstruct) = do
    maybeMsg <- withProcessState $ do
        -- add received vote to set of votes stored locally for this ballot,
        -- initializing this set if doesn't exist yet
        newVotes <-
            leaderVotes . at ballotId . non mempty <%= addVote accId cstruct

        -- if some quorums appeared, recalculate Gamma and apply pending policies
        if isQuorum newVotes
        then do
            when (isMinQuorum newVotes) $
                logInfo $ "Got 1b from quorum of acceptors at " <> pretty ballotId

            -- recalculate Gamma and its combination
            combined <- combinateOrThrow newVotes
            -- pull and reset pending policies
            policies <- use $ leaderPendingPolicies . at ballotId . non mempty
            -- apply policies
            let combined' = foldr acceptOrRejectCommand combined policies
            pure $ Just (Phase2aMsg ballotId combined')
        else pure Nothing

    -- when got a message to submit - broadcast it
    whenJust maybeMsg $
        broadcastTo (processesAddresses Acceptor)

phase2b
    :: (MonadPhase m, HasContextOf Acceptor m)
    => Phase2aMsg -> m ()
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

learn
    :: (MonadPhase m, HasContextOf Learner m)
    => Phase2bMsg -> m ()
learn (Phase2bMsg accId cstruct) = do
    learned <- withProcessState $ do
        do  -- rewrite cstruct kept for this acceptor

            -- we should check here that new cstruct extends previous one.
            -- but the contrary is not an error, because of not-FIFO channels
            learnerVotes . at accId . non mempty %= replaceExtending cstruct

            -- update total learned cstruct
            votes <- use learnerVotes
            newLearned <- combinateOrThrow votes
            prevLearned <- learnerLearned <<.= newLearned

            -- sanity check
            unless (newLearned `extends` prevLearned) $
                errorBadLearnedCStruct prevLearned newLearned

            -- report if the interesting happened
            pure $ guard (newLearned /= prevLearned) $> newLearned

    whenJust learned reportNewLearnedCStruct
  where
    replaceExtending cur new
        | new `extends` cur = new
        | otherwise         = cur

    errorBadLearnedCStruct prev new =
        throwM . ProtocolError $
        sformat ("Newly learned cstruct doesn't extend previous one:\n"%
                 "\t"%build%"\n\t->\n\t"%build)
                prev new
    reportNewLearnedCStruct new =
        logInfo $
        sformat ("New learned cstruct: "%build) new

-- | Phases of algorithm

module Sdn.Protocol.Phases where

import           Control.Lens           (at, non, (%=), (+=), (.=), (<%=), (<<.=), (<>=))
import           Control.TimeWarp.Rpc   (MonadRpc)
import           Control.TimeWarp.Timed (MonadTimed (..))
import           Data.Default           (def)
import           Formatting             (build, sformat, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra
import           Sdn.Protocol.Context
import           Sdn.Protocol.Messages
import           Sdn.Protocol.Processes
import           Sdn.Schedule

-- * Commons

-- | Common constraints for all phrases.
type MonadPhase m =
    ( MonadIO m
    , MonadCatch m
    , MonadTimed m
    , MonadRpc m
    , MonadLog m
    )

-- | Evaluate cstruct with all those policies, which are present
-- in votes from all acceptors of some quorum.
--
-- NOTE: This is _very_ inneficient for now, because it treats policies
-- in abstract way and literally sorts out all the minimal-on-inclusion quorums
-- to evaluate cstruct.
gatherCStructFromAllQuorums
    :: (MonadThrow m, Command policy cstruct, Buildable cstruct, Show cstruct)
    => Members -> Votes cstruct -> m cstruct
gatherCStructFromAllQuorums members votes =
    let quorumsVotes = allMinQuorums members votes
        gamma = map (foldr1 lub . toList) quorumsVotes
        combined = foldrM glb def gamma
    in  maybe (errorContradictory gamma) pure combined
  where
    errorContradictory gamma =
        throwM . ProtocolError $
        sformat ("Got contradictory Gamma: "%buildList) gamma

-- * Phases

propose
    :: (MonadPhase m, HasContext ProposerState m)
    => GenSeed -> Schedule m Policy -> m ()
propose seed schedule = do
    -- start producing policies according to given proposal schedule
    runSchedule seed schedule $ \policy -> do
        logInfo $ sformat ("Proposing policy: "%build) policy
        -- remember them (for testing purposes)
        withProcessState $
            proposerProposedPolicies <>= one policy
        -- and send those policies to leader
        submit (processAddress Leader) (ProposalMsg policy)

rememberProposal
    :: (MonadPhase m, HasContext LeaderState m)
    => ProposalMsg -> m ()
rememberProposal (ProposalMsg policy) = do
    -- atomically modify process'es state
    withProcessState $ do
        -- add policy to pending policies
        leaderPendingPolicies %= (policy :)

phrase1a
    :: (MonadPhase m, HasContext LeaderState m)
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
    :: (MonadPhase m, HasContext AcceptorState m)
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
    :: (MonadPhase m, HasContext LeaderState m)
    => Phase1bMsg -> m ()
phase2a (Phase1bMsg accId ballotId cstruct) = do
    members <- ctxMembers

    maybeMsg <- withProcessState $ do
        -- add received vote to set of votes stored locally for this ballot,
        -- initializing this set if doesn't exist yet
        newVotes <-
            leaderVotes . at ballotId . non mempty <%= addVote accId cstruct

        -- if some quorums appeared, recalculate Gamma and apply pending policies
        if isQuorum members newVotes
        then do
            -- recalculate Gamma and its combination
            combined <- gatherCStructFromAllQuorums members newVotes
            -- pull and reset pending policies
            policies <- leaderPendingPolicies <<.= []
            -- apply policies
            let combined' = foldr acceptOrRejectCommand combined policies
            pure $ Just (Phase2aMsg ballotId combined')
        else pure Nothing

    -- when got a message to submit - broadcast it
    whenJust maybeMsg $
        broadcastTo (processesAddresses Acceptor)

phase2b
    :: (MonadPhase m, HasContext AcceptorState m)
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
        else
           pure Nothing

    whenJust maybeMsg $
        broadcastTo (processesAddresses Learner)

learn
    :: (MonadPhase m, HasContext LearnerState m)
    => Phase2bMsg -> m ()
learn (Phase2bMsg accId cstruct) = do
    members <- ctxMembers

    learned <- withProcessState $ do
        do  -- rewrite cstruct kept for this acceptor
            prevCStruct <- learnerVotes . at accId . non mempty <<.= cstruct

            -- sanity check
            unless (cstruct `extends` prevCStruct) $
                errorBadCStruct prevCStruct cstruct

        do  -- update total learned cstruct
            votes <- use learnerVotes
            newLearned <- gatherCStructFromAllQuorums members votes
            prevLearned <- learnerLearned <<.= newLearned

            -- sanity check
            unless (newLearned `extends` prevLearned) $
                errorBadLearnedCStruct prevLearned prevLearned

            -- report if the interesting happened
            pure $ guard (newLearned /= prevLearned) $> newLearned

    whenJust learned reportNewLearnedCStruct
  where
    errorBadCStruct prev new =
        throwM . ProtocolError $
        sformat ("New cstruct doesn't extend current one:\n"%
                 "\t"%build%"\n\t->\n\t"%build)
                prev new
    errorBadLearnedCStruct prev new =
        throwM . ProtocolError $
        sformat ("Newly learned cstruct doesn't extend previous one:\n"%
                 "\t"%build%"\n\t->\n\t"%build)
                prev new
    reportNewLearnedCStruct new =
        logInfo $
        sformat ("New learned cstruct: "%build) new

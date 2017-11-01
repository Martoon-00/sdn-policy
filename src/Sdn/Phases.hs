-- | Phases of algorithm

module Sdn.Phases where

import           Control.Lens           (at, non, zoom, (%=), (+=), (.=), (<<.=))
import           Control.TimeWarp.Rpc   (MonadRpc)
import           Control.TimeWarp.Timed (MonadTimed (..))
import           Data.Default           (def)
import           Formatting             (build, sformat, (%))
import           System.Wlog            (WithLogger, logDebug, logInfo)
import           Universum

import           Sdn.Context
import           Sdn.CStruct
import           Sdn.Error
import           Sdn.Messages
import           Sdn.Policy
import           Sdn.Processes
import           Sdn.ProposalStrategy
import           Sdn.Quorum
import           Sdn.Types
import           Sdn.Util

-- * Commons

type MonadPhase m =
    ( MonadIO m
    , MonadThrow m
    , MonadTimed m
    , MonadRpc m
    , WithLogger m
    )

-- | Evaluate cstruct with all those policies, which are present
-- in votes from all acceptors of some quorum.
gatherFromAllQuorums
    :: (MonadThrow m, Command policy cstruct, Buildable cstruct)
    => Members -> Votes cstruct -> m cstruct
gatherFromAllQuorums members votes =
    let quorumsVotes = allMinQuorums members votes
        gamma = map (foldr lub def . toList) quorumsVotes
    in  case foldrM glb def gamma of
            Nothing -> throwM . ProtocolError $
                       sformat ("Got contradictory Gamma: "%buildList) gamma
            Just x -> pure x

-- * Phases

propose
    :: MonadPhase m
    => GenSeed -> ProposalStrategy Policy -> m ()
propose seed strategy =
    execStrategy seed strategy $ \policy -> do
        logInfo $ sformat ("Proposed policy: "%build) policy
        submit (processAddress Leader) (ProposalMsg policy)

rememberProposal
    :: (MonadPhase m, HasContext LeaderState m)
    => ProposalMsg -> m ()
rememberProposal (ProposalMsg policy) = do
    withProcessState $ do
        leaderPendingPolicies %= (policy :)

phrase1a
    :: (MonadPhase m, HasContext LeaderState m)
    => m ()
phrase1a = do
    logDebug "Starting new ballot"

    msg <- withProcessState $ do
        leaderBallotId += 1
        Phase1aMsg <$> use leaderBallotId

    broadcastTo (processesAddresses Acceptor) msg

phase1b
    :: (MonadPhase m, HasContext AcceptorState m)
    => Phase1aMsg -> m ()
phase1b (Phase1aMsg ballotId) = do
    furtherMsg <- withProcessState $ do
        acceptorBallotId %= max ballotId
        Phase1bMsg
            <$> use acceptorId
            <*> use acceptorBallotId
            <*> use acceptorCStruct

    submit (processAddress Leader) furtherMsg

phase2a
    :: (MonadPhase m, HasContext LeaderState m)
    => Phase1bMsg -> m ()
phase2a (Phase1bMsg accId ballotId cstruct) = do
    members <- ctxMembers

    maybeMsg <- withProcessState $ do
        maybeCombined <- zoom (leaderVotes . at ballotId . non mempty) $ do
            oldQuorums <- get
            identity %= addVote accId cstruct
            newQuorums <- get

            -- if new quorums appeared, recalculate Gamma and send combined
            -- cstruct further
            if oldQuorums == newQuorums
            then pure Nothing
            else Just <$> gatherFromAllQuorums members newQuorums

        case maybeCombined of
            Nothing -> pure Nothing
            Just combined -> do
                policies <- use leaderPendingPolicies
                leaderPendingPolicies .= []
                let combined' = foldr acceptOrRejectCommand combined policies
                pure $ Just (Phase2aMsg ballotId combined')

    whenJust maybeMsg $
        broadcastTo (processesAddresses Acceptor)

phase2b
    :: (MonadPhase m, HasContext AcceptorState m)
    => Phase2aMsg -> m ()
phase2b (Phase2aMsg ballotId cstruct) = do
    maybeMsg <- withProcessState $ do
        localBallotId <- use acceptorBallotId
        localCstruct <- use acceptorCStruct

        if localBallotId == ballotId && (cstruct `extends` localCstruct)
            || localBallotId < ballotId
        then do
           acceptorBallotId .= ballotId
           acceptorCStruct .= cstruct
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

    withProcessState $ do
        prevCStruct <- learnerVotes . at accId . non mempty <<.= cstruct
        unless (cstruct `extends` prevCStruct) $
            errorBadCStruct prevCStruct cstruct

        votes <- use learnerVotes
        learned <- use learnerLearned
        newLearned <- gatherFromAllQuorums members votes
        if newLearned `extends` learned
        then do
            learnerLearned .= newLearned
            when (learned /= newLearned) $
                reportNewLearnedCStruct newLearned
        else
            errorBadLearnedCStruct learned learned
  where
    errorBadCStruct prev new =
        throwM . ProtocolError $
        sformat ("New cstruct doesn't extend current one:\n"%
                 "\t"%build%"\n\t->\n\t"%build)
                prev new
    errorBadLearnedCStruct prev new =
        throwM . ProtocolError $
        sformat ("New learned cstruct doesn't extend current one:\n"%
                 "\t"%build%"\n\t->\n\t"%build)
                prev new
    reportNewLearnedCStruct new =
        logInfo $
        sformat ("New learned cstruct: "%build) new



-- | Phases of algorithm

module Sdn.Phases where

import           Control.Lens           (at, non, zoom, (%=), (+=), (.=), (<<.=))
import           Control.TimeWarp.Rpc   (MonadRpc)
import           Control.TimeWarp.Timed (MonadTimed (..))
import           Data.Default           (def)
import           Formatting             (build, sformat, (%))
import           System.Wlog            (WithLogger, logDebug, logError, logInfo)
import           Universum

import           Sdn.Context
import           Sdn.CStruct
import           Sdn.Messages
import           Sdn.Policy
import           Sdn.Processes
import           Sdn.ProposalStrategy
import           Sdn.Quorum
import           Sdn.Util

type MonadPhase m =
    ( MonadIO m
    , MonadTimed m
    , MonadRpc m
    , WithLogger m
    )

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
    msg <- withProcessState $ do
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
        zoom (leaderVotes . at ballotId . non mempty) $ do
            oldQuorums <- get
            identity %= addVote accId cstruct
            newQuorums <- get

            -- if new quorums appeared, recalculate Gamma and send combined
            -- cstruct further
            if oldQuorums == newQuorums
            then pure Nothing
            else do
                let quorumsVotes = allMinQuorums members newQuorums
                    gamma = map (foldr lub def . toList) quorumsVotes
                    maybeRes = foldrM glb def gamma
                case maybeRes of
                     Nothing  -> reportBadGamma gamma $> Nothing
                     Just res -> pure $ Just (Phase2aMsg ballotId res)

    whenJust maybeMsg $
        broadcastTo (processesAddresses Acceptor)
  where
    reportBadGamma gamma =
        logError $
        sformat ("Got contradictory Gamma: "%buildList) gamma

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
    withProcessState $ do
        prevCStruct <- learnerVotes . at accId . non mempty <<.= cstruct
        unless (cstruct `extends` prevCStruct) $
            reportBadCStruct prevCStruct cstruct

        allCStructs <- toList <$> use learnerVotes
        let learned = foldr lub def allCStructs
        prevLearned <- learnerLearned <<.= learned
        unless (learned `extends` prevLearned) $
            reportBadLearnedCStruct prevLearned learned
        unless (learned /= prevLearned) $
            reportNewLearnedCStruct learned
  where
    reportBadCStruct prev new =
        logError $
        sformat ("New cstruct doesn't extend current one:\n"%
                 "\t"%build%"\n\t->\n\t"%build)
                prev new
    reportBadLearnedCStruct prev new =
        logError $
        sformat ("New learned cstruct doesn't extend current one:\n"%
                 "\t"%build%"\n\t->\n\t"%build)
                prev new
    reportNewLearnedCStruct new =
        logInfo $
        sformat ("New learned cstruct: "%build) new



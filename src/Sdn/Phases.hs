-- | Phases of algorithm

module Sdn.Phases where

import           Control.Lens           (at, non, zoom, (%=), (+=), (.=), (<<.=))
import           Control.TimeWarp.Rpc   (MonadRpc)
import           Control.TimeWarp.Timed (MonadTimed (..))
import           Data.Default           (def)
import           Universum

import           Sdn.Context
import           Sdn.CStruct
import           Sdn.Messages
import           Sdn.Policy
import           Sdn.ProposalStrategy
import           Sdn.Quorum
import           Sdn.Roles
import           Sdn.Util

type MonadPhase m =
    ( MonadIO m
    , MonadTimed m
    , MonadRpc m
    )

propose
    :: MonadPhase m
    => GenSeed -> ProposalStrategy Policy -> m ()
propose seed strategy =
    execStrategy seed strategy $ \policy ->
        submit leaderAddress (ProposalMsg policy)

rememberProposals
    :: (MonadPhase m, HasContext LeaderState m)
    => ProposalMsg -> m ()
rememberProposals (ProposalMsg policy) = do
    withProcessState $
        leaderPendingPolicies %= (policy :)

phrase1a
    :: (MonadPhase m, HasContext LeaderState m)
    => m ()
phrase1a = do
    msg <- withProcessState $ do
        leaderBallotId += 1
        Phase1aMsg <$> use leaderBallotId

    broadcastTo acceptorsAddresses msg

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

    submit leaderAddress msg

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
                    mres  = foldrM glb def gamma
                res <- maybe undefined pure mres
                pure $ Just (Phase2aMsg ballotId res)

    whenJust maybeMsg $
        broadcastTo acceptorsAddresses

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
        broadcastTo learnersAddresses

learn
    :: (MonadPhase m, HasContext LearnerState m)
    => Phase2bMsg -> m ()
learn (Phase2bMsg accId cstruct) = do
    withProcessState $ do
        wasCStruct <- learnerVotes . at accId . non mempty <<.= cstruct
        unless (cstruct `extends` wasCStruct) $
            undefined

        allCStructs <- toList <$> use learnerVotes
        let learned = foldr lub def allCStructs
        wasLearned <- learnerLearned <<.= learned
        unless (learned `extends` wasLearned) $
            undefined
        unless (learned /= wasLearned) $
            undefined



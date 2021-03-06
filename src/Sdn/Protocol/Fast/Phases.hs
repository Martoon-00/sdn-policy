{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Phases of Fast Paxos.

module Sdn.Protocol.Fast.Phases
    ( propose
    , phase2b
    , learn
    , detectConflicts
    ) where

import           Control.Lens                  (at, makePrisms, (%=), (.=))
import           Control.Monad.Trans.Cont      (ContT (..), evalContT)
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Formatting                    (build, sformat, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra                     (OldNew (..), broadcastTo, compose,
                                                foldlF', listF, logInfo, panicOnFail,
                                                presence, takeNoMoreThanOne, throwOnFail,
                                                wasChanged, whenJust', zoom)
import qualified Sdn.Protocol.Classic.Messages as Classic
import qualified Sdn.Protocol.Classic.Phases   as Classic
import           Sdn.Protocol.Common.Context
import           Sdn.Protocol.Common.Phases
import qualified Sdn.Protocol.Fast.Messages    as Fast
import           Sdn.Protocol.Processes
import           Sdn.Protocol.Versions

-- * Helpers

-- | For one who receives a bunch of decisions from acceptors,
-- this type represents a decision on policy acceptance based on received votes.
data PolicyChoiceStatus
      -- | We still didn't get enough votes to make any decisions.
    = TooFewVotes
      -- | Some value gained so many votes, that no any quorum can accept
      -- another value.
    | OnlyPossible AcceptanceType
      -- | Policy gained a quorum of votes.
    | PolicyFixated AcceptanceType
      -- | We suggest that votes are too contradictory and round is failed.
      -- Some value can still be fixated in soonest future, but we don't rely
      -- on that and going to start a recovery (i.e. leader is going).
    | Undecidable
    deriving (Eq, Show)

makePrisms ''PolicyChoiceStatus

-- | Based on votes for policy, build a decision on its opportunity of
-- being fixated.
decideOnPolicyStatus
    :: (HasMembers, QuorumFamily qf)
    => Votes qf AcceptanceType -> Either Text PolicyChoiceStatus
decideOnPolicyStatus votesForPolicy = evalContT $ do
    let perValueVotes = transposeVotes votesForPolicy

    -- TODO: another quorum can go here
    do
        -- optimization
        let heardFromSome = excludesOtherQuorum votesForPolicy
        unless heardFromSome $
            finishWith TooFewVotes

    do
        mValueFixated <-
            lift . takeNoMoreThanOne "fixated value" $
            M.keys $ M.filter isQuorum perValueVotes
        whenJust mValueFixated $ \acceptance ->
            finishWith $ PolicyFixated acceptance

    do
        mValueCouldBeChosen <-
            lift . takeNoMoreThanOne "possibly chosen value" $
            M.keys $ M.filter excludesOtherQuorum perValueVotes
        whenJust mValueCouldBeChosen $ \acceptance ->
            finishWith $ OnlyPossible acceptance

    -- TODO: another quorum can go here
    do
        let heardFromQuorum = isQuorum votesForPolicy
        unless heardFromQuorum $
            finishWith TooFewVotes

    finishWith Undecidable
  where
    finishWith x = ContT $ \_ -> return x

-- * Proposal

propose
    :: forall cstruct m.
       (MonadPhase cstruct m, HasContextOf Proposer Fast m)
    => NonEmpty (RawCmd cstruct) -> m ()
propose policies = do
    logInfo $ sformat ("Proposing policy (fast): "%listF "," build) policies
    withProcessStateAtomically $ do
        proposerProposedPolicies %= (toList policies <>)
        proposerUnconfirmedPolicies %= foldlF' S.insert policies

    broadcastTo (processesAddresses Acceptor)
                (Fast.ProposalMsg @cstruct policies)

-- * Phase 2

phase2b
    :: forall cstruct m.
       (MonadPhase cstruct m, HasContextOf Acceptor Fast m)
    => PolicyTargets cstruct -> Fast.ProposalMsg cstruct -> m ()
phase2b policyTargets (Fast.ProposalMsg policiesToApply) = do
    logInfo "Got proposal"
    (leaderMsg, learnersMsg) <- withProcessStateAtomically $ do
        appliedPolicies <-
            zoom (acceptorCStruct) $
            mapM acceptOrRejectIntoStoreS policiesToApply

        logInfo $ logFastApplied appliedPolicies

        accId <- use acceptorId
        let leaderMsg = Fast.AcceptedMsg @Leader @cstruct accId appliedPolicies
        let learnersMsg =
                [ (targets, Fast.AcceptedMsg @Learner @cstruct accId policies)
                | (targets, policies) <-
                    groupPolicyTargets policyTargets acceptanceCmd (toList appliedPolicies)
                ]
        pure $ (leaderMsg, learnersMsg)

    broadcastTo (processAddresses Leader) leaderMsg
    forM_ learnersMsg $ \(learners, msg) ->
        broadcastTo (processAddress <$> learners) msg
  where
    logFastApplied =
        sformat ("List of fast applied policies:"
                %"\n    "%listF ", " build)


-- * Learning

learn
    :: forall cstruct m.
       (MonadPhase cstruct m, HasContextOf Learner Fast m)
    => LearningCallback m -> Fast.AcceptedMsg Learner cstruct -> m ()
learn callback (Fast.AcceptedMsg accId (toList -> cstructDiff)) = do
    voteUpdates <- withProcessStateAtomically $
        forM cstructDiff $ rememberUnstableCmdVote learnerVotes accId

    fixatedValues <- fmap catMaybes . forM voteUpdates $ \(policy, votesForPolicy) -> do
        policyStatus <- forM votesForPolicy $
            throwOnFail ProtocolError . decideOnPolicyStatus

        if wasChanged policyStatus
        then do
            whenJust' (getNew policyStatus ^? _PolicyFixated) $ \acceptance -> do
                let policyAcceptance = compose (acceptance, policy)
                logInfo $ sformat ("Policy "%build%" has been fixated")
                        policyAcceptance

                return policyAcceptance
        else return Nothing

    withProcessStateAtomically $
        learnerLearned %=
            let addOne cstruct value =
                    panicOnFail ProtocolError $ addCommand value cstruct
            in  \learned -> foldl addOne learned fixatedValues

    whenNotNull fixatedValues $ onFixatedPolicies callback

-- * Recovery detection and initialition

-- | In fast round, if recovery has been initiated, leader on fast 2b messages
-- acts like if it received proposal messsage.
delegateToRecovery
    :: (MonadPhase cstruct m, HasContextOf Leader Fast m)
    => NonEmpty (RawCmd cstruct) -> m ()
delegateToRecovery conflictingPolicies = do
    Classic.rememberProposal (Classic.ProposalMsg conflictingPolicies)

detectConflicts
    :: forall cstruct m.
       (MonadPhase cstruct m, HasContextOf Leader Fast m)
    => Fast.AcceptedMsg Leader cstruct -> m ()
detectConflicts (Fast.AcceptedMsg accId (toList -> cstructDiff)) = do
    voteUpdates <- forM cstructDiff $ \policyAcceptance ->
        withProcessStateAtomically $
            rememberVoteForPolicy @cstruct leaderFastVotes accId policyAcceptance

    forM_ voteUpdates $ \(policy, oldnewVotesForPolicy) -> do
        oldnewPolicyStatus <- forM oldnewVotesForPolicy $
            throwOnFail ProtocolError . decideOnPolicyStatus

        when (wasChanged oldnewPolicyStatus) $ do
          case getNew oldnewPolicyStatus of
            TooFewVotes -> return ()

            PolicyFixated acceptance -> do
                let policyAcceptance = compose (acceptance, policy)
                logInfo $ supposedlyLearnedLog policyAcceptance

                -- no need to do anything specific, since decision on policy
                -- will never change anymore

            OnlyPossible acceptance -> do
                let policyAcceptance = compose (acceptance, policy)
                logInfo $ onlyChosenLog policyAcceptance

                withProcessStateAtomically $
                    leaderHintPolicies . at policyAcceptance . presence .= True

            Undecidable -> do
                logInfo $ conflictLog policy

                delegateToRecovery (one policy)
                logInfo "Policy ^ proposed for next classic ballot"
  where
    supposedlyLearnedLog =
        sformat ("Policy "%build%" supposedly has been learned, \
                 \not tracking it further")
    onlyChosenLog =
        sformat ("Policy "%build%" is considered possibly chosen")
    conflictLog =
        sformat ("Heard about "%build%" by quorum, but no value \
                 \still has been even potentially chosen. \
                 \Declaring policy conflict!")

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Phases of Fast Paxos.

module Sdn.Protocol.Fast.Phases
    ( propose
    , acceptPolicies
    , phase2b
    , learn
    , detectConflicts
    ) where

import           Control.Lens                  (at, makePrisms, (%=), (.=))
import           Control.Monad.Trans.Cont      (ContT (..), evalContT)
import           Data.List                     (nubBy)
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Formatting                    (build, sformat, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra                     (OldNew (..), broadcastTo, compose, foldlF', listF,
                                                logInfo, panicOnFail, presence, takeNoMoreThanOne,
                                                throwOnFail, wasChanged, whenJust', zoom)
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
    = PolicyChoiceUnknown
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
            finishWith PolicyChoiceUnknown

    do
        mValueFixated <-
            lift . takeNoMoreThanOne "fixated value" $
            M.keys $ M.filter isQuorum perValueVotes
        whenJust mValueFixated $ \acceptance ->
            finishWith $ PolicyFixated acceptance

    do
        let valuesCouldBeChosen =
              M.keys $ M.filter excludesOtherQuorum perValueVotes
        case valuesCouldBeChosen of
            []           -> pass
            [acceptance] -> finishWith $ OnlyPossible acceptance
            _            -> finishWith Undecidable

    -- TODO: another quorum can go here
    do
        let heardFromQuorum = isQuorum votesForPolicy
        unless heardFromQuorum $
            finishWith PolicyChoiceUnknown

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

-- | Handle policy proposal, add it to unstable set in configuration.
acceptPolicies
    :: forall cstruct pv m.
       (MonadPhase cstruct m, HasContextOf Acceptor Fast m)
    => PolicyTargets cstruct
    -> NonEmpty (RawCmd cstruct)
    -> TransactionM (AcceptorState pv cstruct) (m ())
acceptPolicies policyTargets policiesToApply = do
    logInfo $ sformat ("Expected fast accepted: "%listF ", " build)
                      (nubBy conflicts . map Accepted $ toList policiesToApply)

    appliedPolicies <-
        zoom acceptorCStruct $
        mapM acceptOrRejectIntoStoreS policiesToApply

    logInfo $ logFastApplied appliedPolicies

    accId <- use acceptorId
    let leaderMsg = Fast.AcceptedMsg @Leader @cstruct accId appliedPolicies
    let learnersMsg =
            [ (targets, Fast.AcceptedMsg @Learner @cstruct accId policies)
            | (targets, policies) <-
                groupPolicyTargets policyTargets acceptanceCmd (toList appliedPolicies)
            ]
    return $ do
      broadcastTo (processAddresses Leader) leaderMsg
      forM_ learnersMsg $ \(learners, msg) ->
          broadcastTo (processAddress <$> learners) msg
  where
    logFastApplied =
        sformat ("List of fast applied policies:"
                %"\n    "%listF ", " build)

phase2b
    :: forall cstruct m.
       (MonadPhase cstruct m, HasContextOf Acceptor Fast m)
    => PolicyTargets cstruct -> Fast.ProposalMsg cstruct -> m ()
phase2b policyTargets (Fast.ProposalMsg policiesToApply) = do
    logInfo "Got proposal"

    join $ withProcessStateAtomically $ do
      inClassic <- acceptorIsInClassicRound
      if inClassic
        then do
          logInfo "Fast rounds blocked, will apply policy later"
          acceptorFastPending %= (<> toList policiesToApply)
          return pass
        else do
          acceptPolicies policyTargets policiesToApply

-- * Learning

learn
    :: forall cstruct m.
       (MonadPhase cstruct m, HasContextOf Learner Fast m)
    => LearningCallback m -> Fast.AcceptedMsg Learner cstruct -> m ()
learn callback (Fast.AcceptedMsg accId (toList -> cstructDiff)) = do
    voteUpdates <- withProcessStateAtomically $
        forM cstructDiff $
        rememberUnstableCmdVote learnerVotes accId

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
    => m () -> Fast.AcceptedMsg Leader cstruct -> m ()
detectConflicts onConflict (Fast.AcceptedMsg accId (toList -> cstructDiff)) = do
    voteUpdates <- forM cstructDiff $ \policyAcceptance ->
        withProcessStateAtomically $
            rememberVoteForPolicy @cstruct leaderFastVotes accId policyAcceptance

    forM_ voteUpdates $ \(policy, oldnewVotesForPolicy) -> do
        oldnewPolicyStatus <- forM oldnewVotesForPolicy $ \votes ->
            throwOnFail ProtocolError $ do
              status <- decideOnPolicyStatus votes
              return (isQuorum votes, status)

        when (wasChanged oldnewPolicyStatus) $ do
          -- Some optimization
          case getNew oldnewPolicyStatus of
            (_, OnlyPossible acceptance) -> do
                let policyAcceptance = compose (acceptance, policy)
                withProcessStateAtomically $
                    leaderHintPolicies . at policyAcceptance . presence .= True
            _ -> pass

          -- Decide whether the destiny of this policy is clear
          case getNew oldnewPolicyStatus of
            (_, PolicyFixated acceptance) -> do
                let policyAcceptance = compose (acceptance, policy)
                logInfo $ supposedlyLearnedLog policyAcceptance

            (_, Undecidable) -> do
                logInfo $ undecidableLog policy

                delegateToRecovery (one policy)
                logInfo "Policy ^ proposed for next classic ballot"

                onConflict

            (False, _) -> do
                -- Didn't get a quorum of messages yet, passing
                pass

            (True, PolicyChoiceUnknown) -> do
                logInfo (unknownStatusLog policy)
                onConflict

            (True, OnlyPossible acceptance) -> do
                let policyAcceptance = compose (acceptance, policy)
                logInfo $ onlyChosenLog policyAcceptance

                onConflict
  where
    supposedlyLearnedLog =
        sformat ("Policy "%build%" supposedly has been learned, \
                 \not tracking it further")
    unknownStatusLog =
        sformat ("Policy "%build%" has unknown status after getting \
                 \a quorum of messages")
    onlyChosenLog =
        sformat ("Policy "%build%" is considered possibly chosen")
    undecidableLog =
        sformat ("No quorum will be gathered on policy "%build%" \
                 \Declaring policy conflict!")

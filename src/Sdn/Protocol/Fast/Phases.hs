{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Phases of Fast Paxos.

module Sdn.Protocol.Fast.Phases
    ( propose
    , phase2b
    , learn
    , detectConflicts
    ) where

import           Control.Exception             (assert)
import           Control.Lens                  (at, non, (%%=), (%=), (.=))
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Formatting                    (build, sformat, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra                     (compose, decompose, listF, logInfo,
                                                panicOnFail, presence, takeNoMoreThanOne,
                                                throwOnFail, whenJust', zoom)
import qualified Sdn.Protocol.Classic.Messages as Classic
import qualified Sdn.Protocol.Classic.Phases   as Classic
import           Sdn.Protocol.Common.Phases
import           Sdn.Protocol.Context
import qualified Sdn.Protocol.Fast.Messages    as Fast
import           Sdn.Protocol.Processes
import           Sdn.Protocol.Versions

-- * Proposal

propose
    :: forall cstruct m.
       (MonadPhase cstruct m, HasContextOf Proposer Fast m)
    => NonEmpty (RawCmd cstruct) -> m ()
propose policies = do
    logInfo $ sformat ("Proposing policy (fast): "%listF "," build) policies
    withProcessStateAtomically $ do
        proposerProposedPolicies %= (toList policies <>)
        proposerUnconfirmedPolicies %= \s -> foldl (flip S.insert) s policies
    broadcastTo (processesAddresses Acceptor)
                (Fast.ProposalMsg @cstruct policies)

-- * Phase 2

phase2b
    :: forall cstruct m.
       (MonadPhase cstruct m, HasContextOf Acceptor Fast m)
    => Fast.ProposalMsg cstruct -> m ()
phase2b (Fast.ProposalMsg policiesToApply) = do
    msg <- withProcessStateAtomically $ do
        appliedPolicies <-
            zoom (acceptorCStruct . forFast) $
            mapM acceptOrRejectCommandS policiesToApply

        logInfo $
            sformat ("List of fast applied policies:"
                    %"\n    "%listF ", " build)
                appliedPolicies

        accId <- use acceptorId
        pure $ Fast.Phase2bMsg @cstruct accId appliedPolicies

    broadcastTo (processesAddresses Learner <> processAddresses Leader) msg

-- * Learning

learn
    :: forall cstruct m.
       (MonadPhase cstruct m, HasContextOf Learner Fast m)
    => LearningCallback m -> Fast.Phase2bMsg cstruct -> m ()
learn callback (Fast.Phase2bMsg accId (toList -> cstructDiff)) = do
    -- TODO perf: use foldlM
    -- let rememberPolicy policyAcceptance !acc = do
    --         mp <- withProcessStateAtomically $
    --             rememberVoteForPolicy @cstruct learnerFastVotes accId policyAcceptance
    --         return $ maybe identity (:) mp acc
    -- voteUpdates <- foldrM rememberPolicy [] cstructDiff

    voteUpdates <- withProcessStateAtomically $
        fmap catMaybes . forM cstructDiff $ \acceptancePolicy ->
        rememberVoteForPolicy @cstruct learnerFastVotes accId acceptancePolicy

    fixatedValues <- fmap catMaybes . forM voteUpdates $ \(policy, _, votesForPolicy) -> do
        let perValueVotes = transposeVotes votesForPolicy

        mValueFixated <-
            throwOnFail ProtocolError $
            takeNoMoreThanOne "fixated value" $
            M.keys $ M.filter isQuorum perValueVotes

        whenJust' mValueFixated $ \acceptance -> do
            let policyAcceptance = compose (acceptance, policy)
            logInfo $ sformat ("Policy "%build%" has been fixated")
                      policyAcceptance

            return policyAcceptance

    withProcessStateAtomically $
        learnerLearned %=
            let addOne cstruct value =
                    panicOnFail ProtocolError $ addCommand value cstruct
            in  \learned -> foldl addOne learned fixatedValues

    whenNotNull fixatedValues $
        runLearningCallback callback

-- * Recovery detection and initialition

-- | In fast round, if recovery has been initiated, leader on fast 2b messages
-- acts like if it received classic 1b messsage.
delegateToRecovery
    :: (MonadPhase cstruct m, HasContextOf Leader Fast m)
    => AcceptorId -> BallotId -> cstruct -> m ()
delegateToRecovery accId bal cstruct = do
    let recoveryBallotId = bal
    Classic.phase2a (Classic.Phase1bMsg accId recoveryBallotId cstruct)

rememberVoteForPolicy
    :: forall cstruct qf s.
       PracticalCStruct cstruct
    => Traversal' s (PerCmdVotes qf cstruct)
    -> AcceptorId
    -> Cmd cstruct
    -> TransactionM s (Maybe (RawCmd cstruct, Votes qf AcceptanceType, Votes qf AcceptanceType))
    -- TODO: distinguish old & new
rememberVoteForPolicy atVotesL accId policyAcceptance =
    let (acceptance, policy) = decompose policyAcceptance
    in  lift . fmap getAlt $ atVotesL . at policy . non mempty %%= \oldVotes ->
            let newVotes = oldVotes & at accId .~ Just acceptance
            in assertNoRebind oldVotes acceptance $
               (Alt $ Just (policy, oldVotes, newVotes), newVotes)
  where
    -- checks we are not changing existing vote for particular "acceptance" value
    assertNoRebind oldVotes newAcceptance =
        let oldAcceptance = oldVotes ^. at accId
        in  assert (oldAcceptance == Nothing || oldAcceptance == Just newAcceptance)

data PolicyChoiceStatus
    = TooFewVotes
    | OnlyPossible AcceptanceType
    | PolicyFixated AcceptanceType
    | Undecidable
    deriving (Eq, Show)

decideOnPolicyStatus
    :: (HasMembers, QuorumFamily qf)
    => Votes qf AcceptanceType -> Either Text PolicyChoiceStatus
decideOnPolicyStatus votesForPolicy = do
    let perValueVotes = transposeVotes votesForPolicy

    -- TODO: another quorum can go here
    let heardFromQuorum = isQuorum votesForPolicy

    mValueFixated <-
        takeNoMoreThanOne "fixated value" $
        M.keys $ M.filter isQuorum perValueVotes
    mValueCouldBeChosen <-
        takeNoMoreThanOne "possibly chosen value" $
        M.keys $ M.filter excludesOtherQuorum perValueVotes

    if | Just acceptance <- mValueFixated ->
        return $ PolicyFixated acceptance

       | Just acceptance <- mValueCouldBeChosen ->
        return $ OnlyPossible acceptance

       | heardFromQuorum ->
        return Undecidable

       | otherwise ->
        return TooFewVotes

detectConflicts
    :: forall cstruct m.
       (MonadPhase cstruct m, HasContextOf Leader Fast m)
    => Fast.Phase2bMsg cstruct -> m ()
detectConflicts (Fast.Phase2bMsg accId (toList -> cstructDiff)) = do
    voteUpdates <- fmap catMaybes . forM cstructDiff $ \policyAcceptance ->
        withProcessStateAtomically $
        rememberVoteForPolicy @cstruct leaderFastVotes accId policyAcceptance


    forM_ voteUpdates $ \(policy, oldVotesForPolicy, votesForPolicy) -> do
        oldPolicyStatus <- throwOnFail ProtocolError $ decideOnPolicyStatus oldVotesForPolicy
        policyStatus <- throwOnFail ProtocolError $ decideOnPolicyStatus votesForPolicy

        when (policyStatus /= oldPolicyStatus) $ do
          case policyStatus of
            TooFewVotes -> return ()

            PolicyFixated acceptance -> do
                let policyAcceptance = compose (acceptance, policy)
                logInfo $ sformat ("Policy "%build%" supposedly has been learned, \
                                   \not tracking it further")
                        policyAcceptance

                -- TODO: stop tracking

            OnlyPossible acceptance -> do
                let policyAcceptance = compose (acceptance, policy)
                logInfo $ sformat ("Policy "%build%" is considered possibly chosen")
                        policyAcceptance

                withProcessStateAtomically $
                    leaderFastPreferredPolicies . at policyAcceptance . presence .= True
                -- TODO: periodically send

            Undecidable -> do
                logInfo $ sformat ("Heard about "%build%" by quorum, but no value \
                                \still has been chosen, declaring conflict!")
                        policy

                -- TODO: conflict!

{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Utilities for various phases of Paxos.

module Sdn.Protocol.Common.Phases
    ( MonadPhase
    , LearningCallback (..)
    , updateLearnedValue
    , warnOnPartialApply
    , learnCStruct
    , confirmCommitted
    , isPolicyUnconfirmed
    ) where

import           Control.Lens                 (at, non, (%=), (.=), (<%=))
import           Control.TimeWarp.Rpc         (MonadRpc)
import           Control.TimeWarp.Timed       (MonadTimed (..))
import           Data.Default                 (def)
import qualified Data.Set                     as S
import           Formatting                   (build, sformat, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra                    (MonadLog, MonadReporting, RpcOptions,
                                               logError, logInfo, presence, submit,
                                               throwOnFail)
import           Sdn.Protocol.Common.Messages
import           Sdn.Protocol.Context
import           Sdn.Protocol.Processes
import           Sdn.Protocol.Versions

-- | Common constraints for all phases.
type MonadPhase cstruct m =
    ( MonadIO m
    , MonadCatch m
    , MonadTimed m
    , MonadRpc RpcOptions m
    , MonadLog m
    , MonadReporting m
    , HasMembers
    , cstruct ~ DeclaredCStruct m
    , PracticalCStruct cstruct
    )

-- * Common

-- | Drop warning if we are going to merge received cstruct, but
-- dropping some of its policies in the process.
warnOnPartialApply
    :: (MonadLog m, CStruct cstruct)
    => cstruct -> cstruct -> m ()
warnOnPartialApply incoming updated = do
    unless (updated `extends` incoming) $
        logInfo $
        sformat ("Some policies were dropped while applying incoming cstruct:\
                 \\n  incoming:  "%build%
                 "\n  new value: "%build)
              incoming updated

-- * Learning

-- | Update learned value with all checks and cautions.
updateLearnedValue
    :: (CStruct cstruct, Eq cstruct)
    => cstruct
    -> TransactionM (ProcessState Learner pv cstruct) [Cmd cstruct]
updateLearnedValue newLearned = do
    prevLearned <- use learnerLearned

    -- sanity check
    if newLearned `extends` prevLearned
    then learnerLearned .= newLearned
    else do
        if prevLearned `extends` newLearned
        then logInfo "Previously learnt cstruct extends new one, ignoring"
        else reportErrorBadLearnedCStruct prevLearned newLearned

    -- report if the interesting happened
    when (newLearned /= prevLearned) $
        reportNewLearnedCStruct newLearned

    return (newLearned `difference` prevLearned)
  where
    reportErrorBadLearnedCStruct prev new =
        logError $
        sformat ("Newly learned cstruct doesn't extend previous one:\n"%
                 "\t"%build%"\n\t->\n\t"%build)
                prev new
    reportNewLearnedCStruct new =
        logInfo $
        sformat ("New learned cstruct: "%build) new

newtype LearningCallback m = LearningCallback
    { runLearningCallback :: NonEmpty (DeclaredCmd m) -> m ()
    }

instance Applicative m => Monoid (LearningCallback m) where
    mempty = LearningCallback $ \_ -> pure ()
    LearningCallback a `mappend` LearningCallback b = LearningCallback $ \p -> a p *> b p

-- | Learning phase of algorithm.
learnCStruct
    :: (MonadPhase cstruct m, HasContextOf Learner pv m)
    => LearningCallback m
    -> (Votes (VersionQuorum pv) cstruct -> Either Text cstruct)
    -> AcceptorId
    -> cstruct
    -> m ()
learnCStruct (LearningCallback callback) combinator accId (cstruct :: cstruct) = do
    newLearnedPolicies <- withProcessStateAtomically $ do
        -- rewrite cstruct kept for this acceptor

        -- we should check here that new cstruct extends previous one.
        -- but the contrary is not an error, because of not-FIFO channels
        updated <- learnerVotes . at accId . non def <%= maxOrSecond cstruct
        warnOnPartialApply cstruct updated

        -- update total learned cstruct
        -- it should be safe (i.e. it does not induce commands losses)
        -- to learn votes even from fast round if recovery is going
        -- to happen then, because recovery deals with liveleness, correctness
        -- always holds.
        learnedDifference <-
            use learnerVotes
            >>= throwOnFail ProtocolError . combinator
            >>= updateLearnedValue

        return learnedDifference

    whenNotNull newLearnedPolicies $ \policyAcceptances -> do
        let policies = map acceptanceCmd policyAcceptances
        submit (processAddress Proposer) (CommittedMsg @cstruct policies)
        callback policyAcceptances

-- * Confirmation to proposal

-- | Remember that given policies were committed.
confirmCommitted
    :: (MonadPhase cstruct m, HasContextOf Proposer pv m)
    => CommittedMsg cstruct -> m ()
confirmCommitted (CommittedMsg policies) = do
    withProcessStateAtomically $ do
        for_ policies $ \policy ->
            proposerUnconfirmedPolicies %= S.delete policy

-- | Helper to check whether policy is in list of unconfirmed policies.
isPolicyUnconfirmed
    :: (MonadPhase cstruct m, HasContextOf Proposer pv m)
    => RawCmd cstruct -> m Bool
isPolicyUnconfirmed policy = do
    withProcessStateAtomically $
        use $ proposerUnconfirmedPolicies . at policy . presence

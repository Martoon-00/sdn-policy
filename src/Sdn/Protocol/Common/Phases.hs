-- | Utilities for various phases of Paxos.

module Sdn.Protocol.Common.Phases
    ( MonadPhase
    , updateLearnedValue
    , warnOnPartialApply
    , confirmCommitted
    , isPolicyUnconfirmed
    ) where

import           Control.Lens                 (at, (%=), (.=))
import           Control.TimeWarp.Rpc         (MonadRpc)
import           Control.TimeWarp.Timed       (MonadTimed (..))
import qualified Data.Set                     as S
import           Formatting                   (build, sformat, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra                    (MonadLog, MonadReporting, logError,
                                               logInfo, presence)
import           Sdn.Protocol.Common.Messages
import           Sdn.Protocol.Context
import           Sdn.Protocol.Processes

-- | Common constraints for all phases.
type MonadPhase m =
    ( MonadIO m
    , MonadCatch m
    , MonadTimed m
    , MonadRpc m
    , MonadLog m
    , MonadReporting m
    , HasMembers
    )

-- * Learning

-- | Update learned value with all checks and cautions.
updateLearnedValue :: Configuration
                   -> TransactionM (ProcessState Learner pv) [Acceptance Policy]
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

-- | Drop warning if we are going to merge received cstruct, but
-- dropping some of its policies in the process.
warnOnPartialApply :: MonadLog m => Configuration -> Configuration -> m ()
warnOnPartialApply incoming updated = do
    unless (updated `extends` incoming) $
        logInfo $
        sformat ("Some policies were dropped while applying incoming cstruct:\
                 \\n  incoming:  "%build%
                 "\n  new value: "%build)
              incoming updated

-- | Remember that given policies were committed.
confirmCommitted
    :: (MonadPhase m, HasContextOf Proposer pv m)
    => CommittedMsg -> m ()
confirmCommitted (CommittedMsg policies) = do
    withProcessStateAtomically $ do
        for_ policies $ \policy ->
            proposerUnconfirmedPolicies %= S.delete policy

-- | Helper to check whether policy is in list of unconfirmed policies.
isPolicyUnconfirmed
    :: (MonadPhase m, HasContextOf Proposer pv m)
    => Policy -> m Bool
isPolicyUnconfirmed policy = do
    withProcessStateAtomically $
        use $ proposerUnconfirmedPolicies . at policy . presence

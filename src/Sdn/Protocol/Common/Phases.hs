-- | Utilities for various phases of Paxos.

module Sdn.Protocol.Common.Phases where

import           Control.Lens           ((.=))
import           Control.TimeWarp.Rpc   (MonadRpc)
import           Control.TimeWarp.Timed (MonadTimed (..))
import           Formatting             (build, sformat, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra              (MonadLog, MonadReporting, logError, logInfo)
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

-- ** Learning

-- | Update learned value with all checks and cautions.
updateLearnedValue :: Configuration -> TransactionM (ProcessState Learner pv) ()
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
warnOnPartUpdate :: MonadLog m => Configuration -> Configuration -> m ()
warnOnPartUpdate incoming updated = do  -- TODO: normal name
    unless (updated `extends` incoming) $  -- TODO: and normal comment below
        logInfo $ sformat ("Incoming cstruct was (partly) dropped:\
                        \\n  incoming:  "%build%
                        "\n  new value: "%build)
              incoming updated



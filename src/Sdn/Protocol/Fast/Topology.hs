{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Topology setting for Fast Paxos.

module Sdn.Protocol.Fast.Topology where

import           Universum

import           Control.TimeWarp.Timed       (Microsecond, interval, sec)
import           Data.Default                 (Default (..))
import           Sdn.Extra.Util               (hoistItem)
import qualified Sdn.Protocol.Classic.Phases  as Classic
import           Sdn.Protocol.Common.Phases   (batchedOrSimpleProposals)
import           Sdn.Protocol.Common.Topology
import qualified Sdn.Protocol.Fast.Phases     as Fast
import           Sdn.Protocol.Processes
import           Sdn.Protocol.Versions        (Fast)

-- | In Fast Paxos, except base settings, we have tunable recovery delay.
data instance CustomTopologySettings Fast =
    FastTopologySettingsPart
    { topologyRecoveryDelay :: Microsecond
    }

instance Default (CustomTopologySettings Fast) where
    def =
        FastTopologySettingsPart
        { topologyRecoveryDelay = interval 1 sec
        }


instance HasVersionProtocolListeners Fast where
    versionProtocolListeners callback =
        ProtocolListeners
        { leaderListeners =
            [ listener @Leader Classic.rememberProposal
            , listener @Leader Classic.phase2a
            , listener @Leader Fast.detectConflicts
            ]
        , acceptorListeners =
            [ listener @Acceptor Classic.phase1b
            , listener @Acceptor Classic.phase2b
            , listener @Acceptor Fast.phase2b
            ]
        , learnerListeners =
            [ listener @Learner $ Classic.learn (hoistItem lift callback)
            , listener @Learner $ Fast.learn (hoistItem lift callback)
            ]
        }

instance HasVersionTopologyActions Fast where
    versionTopologyActions TopologySettings{..} =
        TopologyActions
        { proposeAction = batchedOrSimpleProposals topologyProposalBatchSettings Fast.propose
        , startBallotAction = pass
        , topologyListeners = versionProtocolListeners mempty
        }
      where
        FastTopologySettingsPart{..} = topologyCustomSettings

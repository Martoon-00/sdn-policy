{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Topology setting for Classic Paxos.

module Sdn.Protocol.Classic.Topology where

import           Data.Default                 (Default (..))
import           Universum

import           Sdn.Extra.Util               (hoistItem)
import           Sdn.Protocol.Classic.Phases
import           Sdn.Protocol.Common.Phases   (simpleProposal)
import           Sdn.Protocol.Common.Topology
import           Sdn.Protocol.Processes
import           Sdn.Protocol.Versions        (Classic)

-- | In Classic Paxos with don't have any settings expect base ones.
data instance CustomTopologySettings Classic =
    ClassicTopologySettingsPart

instance Default (CustomTopologySettings Classic) where
    def = ClassicTopologySettingsPart


instance HasVersionProtocolListeners Classic where
    versionProtocolListeners ProtocolListenersSettings{..} =
        ProtocolListeners
        { leaderListeners =
            [ listener @Leader rememberProposal
            , listener @Leader phase2a
            ]
        , acceptorListeners =
            [ listener @Acceptor phase1b
            , listener @Acceptor phase2b
            ]
        , learnerListeners =
            [ listener @Learner $ learn (hoistItem lift callback)
            ]
        }
      where
        callback = listenersLearningCallback

instance HasVersionTopologyActions Classic where
    versionTopologyActions _ =
        TopologyActions
        { proposeAction = simpleProposal (propose . one)
        , startBallotAction = phase1a
        , topologyListeners = versionProtocolListeners def
        }

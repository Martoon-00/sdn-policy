{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Topology setting for Classic Paxos.

module Sdn.Protocol.Classic.Topology where

import           Data.Default                 (Default (..))
import           Universum

import           Sdn.Protocol.Classic.Phases
import           Sdn.Protocol.Common.Topology
import           Sdn.Protocol.Processes
import           Sdn.Protocol.Versions        (Classic)

-- | In Classic Paxos with don't have any settings expect base ones.
data instance CustomTopologySettings Classic =
    ClassicTopologySettingsPart

instance Default (CustomTopologySettings Classic) where
    def = ClassicTopologySettingsPart


instance HasVersionTopologyActions Classic where
    versionTopologyActions _ =
        TopologyActions
        { proposeAction = propose
        , startBallotAction = phase1a
        , leaderListeners =
            [ listener @Leader rememberProposal
            , listener @Leader phase2a
            ]
        , acceptorListeners =
            [ listener @Acceptor phase1b
            , listener @Acceptor phase2b
            ]
        , learnerListeners =
            [ listener @Learner $ learn mempty
            ]
        }


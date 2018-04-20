{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}

-- | States kept by different processes.

module Sdn.Protocol.Common.Context.States where

import           Control.Lens                      (makeLenses)
import           Data.Default                      (Default (def))
import qualified Data.Set                          as S
import qualified Data.Text.Buildable
import           Formatting                        (bprint, build, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra                         (listF, pairF)
import           Sdn.Protocol.Common.Context.Types
import           Sdn.Protocol.Versions

-- * Proposer

-- | State held by proposer.
data ProposerState pv cstruct = ProposerState
    { _proposerProposedPolicies    :: [RawCmd cstruct]
      -- ^ Policies ever proposed with their condition.
    , _proposerUnconfirmedPolicies :: S.Set (RawCmd cstruct)
      -- ^ Policies not yet reported as committed by any learned.
    }

makeLenses ''ProposerState

instance PracticalCStruct cstruct =>
         Buildable (ProposerState pv cstruct) where
    build ProposerState{..} =
        bprint
            ("\n    proposed policies:\n    "%listF "\n    , " build
            %"\n    unconfirmed policies:\n    "%listF "\n    , " build)
            _proposerProposedPolicies
            _proposerUnconfirmedPolicies

instance Default cstruct =>
         Default (ProposerState pv cstruct) where
    def = ProposerState mempty def

-- * Leader

-- TODO full Buildable instances

-- | State kept by leader.
data LeaderState pv cstruct = LeaderState
    { _leaderBallotId         :: BallotId
      -- ^ Number of current ballot
    , _leaderProposedPolicies :: ForBothRoundTypes $ ProposedCommands (RawCmd cstruct)
      -- ^ Policies ever proposed on this classic / fast ballot, in latter case used in recovery
    , _leaderVotes            :: Map BallotId $ Votes ClassicMajorityQuorum cstruct
      -- ^ CStructs received in 2b messages
    , _leaderFastVotes        :: PerCmdVotes FastMajorityQuorum cstruct
      -- ^ CStructs detected in 2b messages of fast ballot
    , _leaderFastPreferredPolicies :: Set $ Cmd cstruct
      -- ^ Policies for which only one acceptance type is (mostly probably) possible.
    }

makeLenses ''LeaderState

instance (ProtocolVersion pv, PracticalCStruct cstruct) =>
         Buildable (LeaderState pv cstruct) where
    build LeaderState {..} =
        bprint
            ("\n    current ballod id: " %build%
             "\n    proposed policies: " %bothRoundsF build%
             "\n    votes: " %ballotMapF build)
            _leaderBallotId
            _leaderProposedPolicies
            _leaderVotes

-- | Initial state of the leader.
instance Default (LeaderState pv cstruct) where
    def = LeaderState def def mempty def def

-- * Acceptor

-- | State kept by acceptor.
data AcceptorState pv cstruct = AcceptorState
    { _acceptorId                   :: AcceptorId
      -- ^ Identificator of this acceptor, should be read-only
    , _acceptorLastKnownBallotId    :: BallotId
      -- ^ Last heard ballotId from leader
    , _acceptorCStruct              :: ForBothRoundTypes cstruct
      -- ^ Gathered CStruct so far
    , _acceptorFastProposedPolicies :: ProposedCommands (RawCmd cstruct)
      -- ^ Policies proposed upon each fast ballot
    }

makeLenses ''AcceptorState

instance (ProtocolVersion pv, PracticalCStruct cstruct) =>
         Buildable (AcceptorState pv cstruct) where
    build AcceptorState{..} =
        bprint
            ("\n    my id: "%build%
             "\n    last known ballot id: "%build%
             "\n    cstruct: "%bothRoundsF build%
             "\n    proposed policies at fast ballots: "%build)
            _acceptorId
            _acceptorLastKnownBallotId
            _acceptorCStruct
            _acceptorFastProposedPolicies

-- | Initial state of acceptor.
defAcceptorState
    :: forall pv cstruct.
       Default cstruct
    => AcceptorId -> (AcceptorState pv cstruct)
defAcceptorState id = AcceptorState id def def def

-- * Learner

-- | State kept by learner.
data LearnerState pv cstruct = LearnerState
    { _learnerVotes     :: Votes (VersionQuorum pv) cstruct
      -- ^ CStructs received from acceptors so far
    , _learnerFastVotes :: Map (RawCmd cstruct) $ Votes (VersionQuorum pv) AcceptanceType
      -- ^ What has been choosen on policy.
      -- Used only in fast ballots for now - TODO: merge
    , _learnerLearned   :: cstruct
      -- ^ Eventually learned cstruct, result of consensus
    }

makeLenses ''LearnerState

instance PracticalCStruct cstruct =>
         Buildable (LearnerState pv cstruct) where
    build LearnerState{..} =
        bprint
            ("\n    heard: "%build%
             "\n    heard fast: "%listF ",\n    " (pairF (build%": "%build))%
             "\n    learned: "%build)
            _learnerVotes
            _learnerFastVotes
            _learnerLearned

-- | Initial state of the learner.
instance Default cstruct => Default (LearnerState pv cstruct) where
    def = LearnerState mempty def def

-- * Overall

data AllStates pv cstruct = AllStates
    { proposerState   :: ProposerState pv cstruct
    , leaderState     :: (LeaderState pv cstruct)
    , acceptorsStates :: [AcceptorState pv cstruct]
    , learnersStates  :: [LearnerState pv cstruct]
    }

instance (ProtocolVersion pv, PracticalCStruct cstruct) =>
         Buildable (AllStates pv cstruct) where
    build AllStates {..} =
        bprint
            ("\n  Proposer state: " %build % "\n\n  Leader state: " %build %
             "\n\n  Acceptors states: " %listF "\n  , " build %
             "\n\n  Learners states: " %listF "\n  , " build)
            proposerState
            leaderState
            acceptorsStates
            learnersStates


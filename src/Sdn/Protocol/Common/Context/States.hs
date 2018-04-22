{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}

-- | States kept by different processes.

module Sdn.Protocol.Common.Context.States where

import           Control.Lens                     (makeLenses)
import           Data.Default                     (Default (def))
import qualified Data.Set                         as S
import qualified Data.Text.Buildable
import           Formatting                       (bprint, build, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra                        (listF, specifyF)
import           Sdn.Protocol.Common.Context.Data
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
    , _leaderProposedPolicies :: ProposedCommands (RawCmd cstruct)
      -- ^ Policies proposed to leader in classic round or in fast round
      -- as recovery measure.
    , _leaderHintPolicies     :: Set (Cmd cstruct)
      -- ^ Policies for which only one acceptance type is (most probably) possible.
    , _leaderVotes            :: Map BallotId $ Votes ClassicMajorityQuorum cstruct
      -- ^ CStructs received in 2b messages
    , _leaderFastVotes        :: PerCmdVotes FastMajorityQuorum cstruct
      -- ^ CStructs detected in 2b messages of fast ballot
    }

makeLenses ''LeaderState

instance (ProtocolVersion pv, PracticalCStruct cstruct) =>
         Buildable (LeaderState pv cstruct) where
    build LeaderState {..} =
        bprint
            ("\n    current ballod id: " %build%
             "\n    proposed policies: " %build%
             "\n    hints: "%listF ", " build%
             "\n    votes: " %ballotMapF build%
             "\n    fast votes: " %listF ", " specifyF)
            _leaderBallotId
            _leaderProposedPolicies
            _leaderHintPolicies
            _leaderVotes
            _leaderFastVotes

-- | Initial state of the leader.
instance Default (LeaderState pv cstruct) where
    def = LeaderState def def def def def

-- * Acceptor

-- | State kept by acceptor.
data AcceptorState pv cstruct = AcceptorState
    { _acceptorId                :: AcceptorId
      -- ^ Identificator of this acceptor, should be read-only
    , _acceptorLastKnownBallotId :: BallotId
      -- ^ Last heard ballotId from leader
    , _acceptorCStruct           :: CStructStore cstruct
      -- ^ Gathered CStruct so far
    }

makeLenses ''AcceptorState

instance (ProtocolVersion pv, PracticalCStruct cstruct) =>
         Buildable (AcceptorState pv cstruct) where
    build AcceptorState{..} =
        bprint
            ("\n    my id: "%build%
             "\n    last known ballot id: "%build%
             "\n    cstruct: "%build)
            _acceptorId
            _acceptorLastKnownBallotId
            _acceptorCStruct

-- | Initial state of acceptor.
defAcceptorState
    :: forall pv cstruct.
       Default cstruct
    => AcceptorId -> (AcceptorState pv cstruct)
defAcceptorState id = AcceptorState id def def

-- * Learner

-- | State kept by learner.
data LearnerState pv cstruct = LearnerState
    { _learnerVotes   :: Votes (VersionQuorum pv) (CStructStore cstruct)
      -- ^ CStructs received from acceptors so far
    , _learnerLearned :: cstruct
      -- ^ Eventually learned cstruct, result of consensus
    }

makeLenses ''LearnerState

instance PracticalCStruct cstruct =>
         Buildable (LearnerState pv cstruct) where
    build LearnerState{..} =
        bprint
            ("\n    heard: "%build%
             "\n    learned: "%build)
            _learnerVotes
            _learnerLearned

-- | Initial state of the learner.
instance Default cstruct => Default (LearnerState pv cstruct) where
    def = LearnerState mempty def

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


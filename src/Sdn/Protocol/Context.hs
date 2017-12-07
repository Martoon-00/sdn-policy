{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Various contexts of processes

module Sdn.Protocol.Context where

import           Control.Concurrent.STM (STM)
import           Control.Lens           (makeLenses)
import           Control.TimeWarp.Rpc   (MonadRpc, NetworkAddress)
import           Control.TimeWarp.Timed (MonadTimed)
import           Data.Default           (Default (def))
import qualified Data.Map               as M
import           Data.Tagged            (Tagged (..))
import qualified Data.Text.Buildable
import           Data.Text.Lazy.Builder (Builder)
import           Formatting             (Format, bprint, build, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra
import           Sdn.Protocol.Versions

-- * General

-- | Context kept by single process.
data ProcessContext s = ProcessContext
    { pcState   :: TVar s   -- ^ Process'es mutable state
    }

-- | Atomically modify state stored by process.
-- If exception is thrown in the process, no changes apply.
withProcessState
    :: ( MonadIO m
       , MonadLog m
       , MonadReporting m
       , MonadReader (ProcessContext s) m
       )
    => PureLog (StateT s STM) a -> m a
withProcessState modifier = do
    var <- pcState <$> ask
    launchPureLog (atomically . modifyTVarS var) modifier

-- * Per-process contexts
-- ** Proposer

data ProposerState = ProposerState
    { _proposerProposedPolicies :: [Policy]
      -- ^ Policies ever proposed (for testing purposes)
    }

makeLenses ''ProposerState

instance Buildable ProposerState where
    build ProposerState{..} =
        bprint
            ("\n    proposed policies:\n    "%buildList "\n    , ")
            _proposerProposedPolicies

instance Default ProposerState where
    def = ProposerState mempty

-- ** Leader

-- * State kept by leader.
data LeaderState pv = LeaderState
    { _leaderBallotId            :: BallotId pv
      -- ^ Number of current ballot
    , _leaderPendingPolicies     :: Map (BallotId pv) [Policy]
      -- ^ Policies proposed upon each ballot
    , _leaderPendingPoliciesFast :: Map (BallotId pv) [Policy]
      -- ^ Policies ever proposed on this fast ballot, used in recovery
    , _leaderVotes               :: Map (BallotId pv) (Votes ClassicMajorityQuorum Configuration)
      -- ^ CStructs received in 2b messages
    , _leaderFastVotes           :: Map (BallotId pv) (Votes FastMajorityQuorum Configuration)
      -- ^ CStructs detected in 2b messages of fast ballot
    }

makeLenses ''LeaderState

instance Buildable (LeaderState pv) where
    build LeaderState{..} =
        bprint
            ("\n    current ballod id: "%build%
             "\n    pending policies: "%buildList "\n    , "%
             "\n    votes: "%buildList "\n    , ")
            _leaderBallotId
            (buildBallotMap _leaderPendingPolicies (buildList ", "))
            (buildBallotMap _leaderVotes build)
      where
        buildBallotMap :: Map (BallotId pv) a -> Format Builder (a -> Builder) -> [Builder]
        buildBallotMap m how =
            M.toList m <&> \(id, v) -> bprint (build%": "%how) id v

-- | Initial state of the leader.
instance ProtocolVersion pv => Default (LeaderState pv) where
    def = Tagged $ LeaderState balId mempty mempty mempty mempty
      where
        balId = startBallotId @pv

-- ** Acceptor

-- * State kept by acceptor.
data AcceptorState pv = AcceptorState
    { _acceptorId              :: AcceptorId
      -- ^ Identificator of this acceptor, should be read-only
    , _acceptorBallotId        :: (BallotId pv)
      -- ^ Last heard ballotId from leader
    , _acceptorCStruct         :: Configuration
      -- ^ Gathered CStruct so far
    , _acceptorPendingPolicies :: Map (BallotId pv) [Policy]
      -- ^ Policies proposed upon each ballot
    }

makeLenses ''AcceptorState

instance Buildable (AcceptorState pv) where
    build AcceptorState{..} =
        bprint
            ("\n    my id: "%build%
             "\n    last known ballot id: "%build%
             "\n    cstruct: "%build)
            _acceptorId
            _acceptorBallotId
            _acceptorCStruct

-- | Initial state of acceptor.
defAcceptorState :: AcceptorId -> (AcceptorState pv)
defAcceptorState id = AcceptorState id balId mempty mempty
  where
    -- didn't give promise about any ballot
    balId = ClassicBallotId (-1)

-- ** Learner

-- * State kept by learner.
data LearnerState = LearnerState
    { _learnerVotes   :: Votes ClassicMajorityQuorum Configuration
      -- ^ CStructs received from acceptors so far
    , _learnerLearned :: Configuration
      -- ^ Eventually learned cstruct, result of consensus
    }

makeLenses ''LearnerState

instance Buildable LearnerState where
    build LearnerState{..} =
        bprint
            ("\n    heard: "%build%
             "\n    learned: "%build)
            _learnerVotes
            _learnerLearned

-- | Initial state of the learner.
instance Default LearnerState where
    def = LearnerState mempty mempty

-- * Misc

data AllStates pv = AllStates
    { proposerState   :: ProposerState
    , leaderState     :: (LeaderState pv)
    , acceptorsStates :: [AcceptorState pv]
    , learnersStates  :: [LearnerState]
    }

instance Buildable (AllStates pv) where
    build AllStates{..} =
        bprint
            (  "\n  Proposer state: "%build%
             "\n\n  Leader state: "%build%
             "\n\n  Acceptors states: "%buildList "\n  , "%
             "\n\n  Learners states: "%buildList "\n  , ")
            proposerState
            leaderState
            acceptorsStates
            learnersStates

-- | Send a message to given participants.
broadcastTo
    :: ( MonadCatch m
       , MonadTimed m
       , MonadRpc m
       , MonadReader (ProcessContext s) m
       , Message msg
       , HasMembers
       )
    => [NetworkAddress] -> msg -> m ()
broadcastTo getAddresses msg = do
    let addresses = getAddresses
    forM_ addresses $ \addr -> submit addr msg

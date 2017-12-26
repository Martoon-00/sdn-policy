{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

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
import           Formatting             (Format, bprint, build, later, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra              (Message, MonadLog, MonadReporting, PureLog,
                                         launchPureLog, listF, modifyTVarS, submit)

-- * General

-- | Context kept by single process.
data ProcessContext s = ProcessContext
    { pcState   :: TVar s   -- ^ Process'es mutable state
    }

-- | Envorinment for transaction modifying process state.
type TransactionM s a = PureLog (StateT s STM) a

-- | Constraints for transaction.
type MonadTransaction s m =
    ( MonadIO m
    , MonadLog m
    , MonadReporting m
    , MonadReader (ProcessContext s) m
    )

-- | Atomically modify state stored by process.
-- If exception is thrown in the process, no changes apply.
withProcessState
    :: MonadTransaction s m => TransactionM s a -> m a
withProcessState modifier = do
    var <- pcState <$> ask
    launchPureLog (atomically . modifyTVarS var) modifier

data ForBothRoundTypes a = ForBothRoundTypes
    { _forClassic :: a
    , _forFast    :: a
    }

makeLenses ''ForBothRoundTypes

instance Monoid a => Monoid (ForBothRoundTypes a) where
    mempty = ForBothRoundTypes mempty mempty
    ForBothRoundTypes a1 b1 `mappend` ForBothRoundTypes a2 b2
        = ForBothRoundTypes (a1 <> a2) (b1 <> b2)

type PerBallot a = Map BallotId a
type PerBallots a = ForBothRoundTypes $ PerBallot a

-- | Formatter for ballot id map
ballotMapF
    :: Buildable BallotId
    => Format Builder (a -> Builder) -> Format r (PerBallot a -> r)
ballotMapF f =
    later $ \m ->
    mconcat $ M.toList m <&> \(id, v) -> bprint (build % ": " %f) id v


-- * Per-process contexts
-- ** Proposer

-- | State held by proposer.
data ProposerState = ProposerState
    { _proposerProposedPolicies :: [Policy]
      -- ^ Policies ever proposed (for testing purposes)
    }

makeLenses ''ProposerState

instance Buildable ProposerState where
    build ProposerState{..} =
        bprint
            ("\n    proposed policies:\n    "%listF "\n    , " build)
            _proposerProposedPolicies

instance Default ProposerState where
    def = ProposerState mempty

-- ** Leader

-- | Whether need in recovery has been checked and recovery was
-- launched if necessary.
data FastBallotStatus
    = FastBallotInProgress  -- fast ballot is executing at moment
    | FastBallotSucceeded   -- fast ballot terminated without need in recovery
    | FastBallotInRecovery  -- recovery has been initiated
    deriving (Eq, Show)

instance Default FastBallotStatus where
    def = FastBallotInProgress

-- | State kept by leader.
data LeaderState = LeaderState
    { _leaderBallotId         :: BallotId
      -- ^ Number of current ballot
    , _leaderPendingPolicies  :: PerBallots [Policy]
      -- ^ Policies ever proposed on this fast ballot, used in recovery
    , _leaderVotes            :: Map BallotId $ Votes ClassicMajorityQuorum Configuration
      -- ^ CStructs received in 2b messages
    , _leaderFastVotes        :: Map BallotId $ Votes FastMajorityQuorum Configuration
      -- ^ CStructs detected in 2b messages of fast ballot
    , _leaderFastBallotStatus :: Map BallotId FastBallotStatus
      -- ^ Whether fast ballot succeeded or failed with conflict.
    , _leaderRecoveryUsed     :: Map BallotId ()
      -- ^ Ballots for which extra recovery ballot was initiated.
    }

makeLenses ''LeaderState

bothRoundsF :: Format Builder (a -> Builder) -> Format r (ForBothRoundTypes a -> r)
bothRoundsF fmt = later $ \ForBothRoundTypes{..} ->
    bprint ("_fast_: "%fmt) _forClassic <>
    bprint ("\n  _classic_: "%fmt) _forFast

instance Buildable LeaderState where
    build LeaderState {..} =
        bprint
            ("\n    current ballod id: " %build %
             "\n    pending policies: " %bothRoundsF (ballotMapF $ listF ", " build) %
             "\n    votes: " %ballotMapF build)
            _leaderBallotId
            _leaderPendingPolicies
            _leaderVotes

-- | Initial state of the leader.
instance Default (Tagged pv LeaderState) where
    def = Tagged $ LeaderState def mempty mempty mempty mempty mempty

-- ** Acceptor

-- | State kept by acceptor.
data AcceptorState = AcceptorState
    { _acceptorId                  :: AcceptorId
      -- ^ Identificator of this acceptor, should be read-only
    , _acceptorLastKnownBallotId   :: BallotId
      -- ^ Last heard ballotId from leader
    , _acceptorCStruct             :: Configuration
      -- ^ Gathered CStruct so far
    , _acceptorFastPendingPolicies :: Map BallotId [Policy]
      -- ^ Policies proposed upon each fast ballot
    }

makeLenses ''AcceptorState

instance Buildable AcceptorState where
    build AcceptorState{..} =
        bprint
            ("\n    my id: "%build%
             "\n    last known ballot id: "%build%
             "\n    cstruct: "%build)
            _acceptorId
            _acceptorLastKnownBallotId
            _acceptorCStruct

-- | Initial state of acceptor.
defAcceptorState :: AcceptorId -> AcceptorState
defAcceptorState id = AcceptorState id def mempty mempty

-- ** Learner

-- | State kept by learner.
data LearnerState = LearnerState
    { _learnerVotes     :: Votes ClassicMajorityQuorum Configuration
      -- ^ CStructs received from acceptors in classic round so far
    , _learnerFastVotes :: Votes FastMajorityQuorum Configuration
      -- ^ CStructs received from acceptors in fast round so far
    , _learnerLearned   :: Configuration
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
    def = LearnerState mempty mempty mempty

-- * Misc

data AllStates = AllStates
    { proposerState   :: ProposerState
    , leaderState     :: LeaderState
    , acceptorsStates :: [AcceptorState]
    , learnersStates  :: [LearnerState]
    }

instance Buildable AllStates where
    build AllStates {..} =
        bprint
            ("\n  Proposer state: " %build % "\n\n  Leader state: " %build %
             "\n\n  Acceptors states: " %listF "\n  , " build %
             "\n\n  Learners states: " %listF "\n  , " build)
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

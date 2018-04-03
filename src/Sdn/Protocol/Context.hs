{-# LANGUAGE GADTs                #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Various contexts of processes

module Sdn.Protocol.Context where

import           Control.Lens           (At (..), Index, IxValue, Ixed (..), at,
                                         makeLenses, makePrisms, (.=), (<<.=))
import           Control.TimeWarp.Rpc   (MonadRpc, NetworkAddress)
import           Control.TimeWarp.Timed (MonadTimed)
import           Data.Default           (Default (def))
import qualified Data.Set               as S
import qualified Data.Text.Buildable
import           Data.Text.Lazy.Builder (Builder)
import           Formatting             (Format, bprint, build, later, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra              (Message, MonadLog, MonadReporting, PureLog,
                                         RpcOptions, launchPureLog, listF, pairF, submit)
import           Sdn.Extra.MemStorage
import           Sdn.Policy.Fake
import           Sdn.Protocol.Versions

-- * General

-- | Context kept by single process.
data ProcessContext ctx = ProcessContext
    { pcState :: ctx  -- ^ Process'es mutable state
    }

-- | Envorinment for transaction modifying process state.
type TransactionM s a = forall n. (MonadCatch n) => PureLog (StateT s n) a

-- | Constraints for transaction.
type MonadTransaction ctx m =
    ( MonadIO m
    , MonadThrow m
    , MonadLog m
    , MonadReporting m
    , MonadReader (ProcessContext ctx) m
    )

-- | Atomically modify state stored by process.
-- If exception is thrown in the process, no changes apply.
withProcessStateAtomically
    :: (MonadTransaction (DeclaredMemStore m s) m, DeclaresMemStore m)
    => TransactionM s a -> m a
withProcessStateAtomically modifier = do
    ProcessContext{..} <- ask
    MemStorage{..} <- getMemStorage
    launchPureLog (atomicallyModifyMemStorage pcState) modifier

data ForBothRoundTypes a = ForBothRoundTypes
    { _forClassic :: a
    , _forFast    :: a
    }

makeLenses ''ForBothRoundTypes

instance Monoid a => Monoid (ForBothRoundTypes a) where
    mempty = ForBothRoundTypes mempty mempty
    ForBothRoundTypes a1 b1 `mappend` ForBothRoundTypes a2 b2
        = ForBothRoundTypes (a1 <> a2) (b1 <> b2)

instance Default a => Default (ForBothRoundTypes a) where
    def = ForBothRoundTypes def def

type PerBallot a = Map BallotId a
type PerBallots a = ForBothRoundTypes $ PerBallot a

-- | Formatter for ballot id map
ballotMapF
    :: Buildable BallotId
    => Format Builder (a -> Builder) -> Format r (PerBallot a -> r)
ballotMapF f = listF "\n  " (pairF ": " build f)

-- | This is where commands from proposer are stored.
-- We need to cache all policies proposed upon specified ballot.
data ProposedCommands a = ProposedCommands
    { _ballotProposedCommands  :: PerBallot [a]
      -- ^ Commands proposed upon given ballots.
    , _pendingProposedCommands :: [a]
      -- ^ Proposed commands to attach to next ballot.
    }

makeLenses ''ProposedCommands

instance Default (ProposedCommands a) where
    def = ProposedCommands mempty mempty

instance Buildable a => Buildable (ProposedCommands a) where
    build ProposedCommands{..} =
        bprint ("pending: "%listF ", " build%"\n"
               %"fixed: "%ballotMapF (listF ", " build))
            _pendingProposedCommands
            _ballotProposedCommands

instance Ixed (ProposedCommands a) where
instance At (ProposedCommands a) where
    at i = ballotProposedCommands . at i
type instance Index (ProposedCommands a) = Index (PerBallot [a])
type instance IxValue (ProposedCommands a) = IxValue (PerBallot [a])

rememberProposedCommandsAt :: BallotId -> ProposedCommands a -> ProposedCommands a
rememberProposedCommandsAt ballotId = execState $ do
    pending <- pendingProposedCommands <<.= []
    ballotProposedCommands . at ballotId .= Just pending

dumpProposedCommands :: MonadState (ProposedCommands a) m => BallotId -> m [a]
dumpProposedCommands ballotId =
    use pendingProposedCommands <* modify (rememberProposedCommandsAt ballotId)

-- * Per-process contexts
-- ** Proposer

data PolicyConfirmation
    = UnconfirmedSince BallotId
    | Confirmed

-- | State held by proposer.
data ProposerState pv = ProposerState
    { _proposerProposedPolicies    :: [Policy]
      -- ^ Policies ever proposed with their condition.
    , _proposerUnconfirmedPolicies :: S.Set Policy
      -- ^ Policies not yet reported as committed by any learned.
    }

makeLenses ''ProposerState

instance Buildable (ProposerState pv) where
    build ProposerState{..} =
        bprint
            ("\n    proposed policies:\n    "%listF "\n    , " build
            %"\n    unconfirmed policies:\n    "%listF "\n    , " build)
            _proposerProposedPolicies
            _proposerUnconfirmedPolicies

instance Default (ProposerState pv) where
    def = ProposerState mempty mempty

-- ** Leader

-- | Whether need in recovery has been checked and recovery was
-- launched if necessary.
data FastBallotStatus
    = FastBallotInProgress  -- fast ballot is executing at moment
    | FastBallotSucceeded   -- fast ballot terminated without need in recovery
    | FastBallotInRecovery  -- recovery has been initiated
    deriving (Eq, Show)

makePrisms ''FastBallotStatus

instance Default FastBallotStatus where
    def = FastBallotInProgress

-- TODO full Buildable instances

-- | State kept by leader.
data LeaderState pv = LeaderState
    { _leaderBallotId         :: BallotId
      -- ^ Number of current ballot
    , _leaderProposedPolicies :: ForBothRoundTypes $ ProposedCommands Policy
      -- ^ Policies ever proposed on this classic / fast ballot, in latter case used in recovery
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

instance ProtocolVersion pv =>
         Buildable (LeaderState pv) where
    build LeaderState {..} =
        bprint
            ("\n    current ballod id: " %build%
             "\n    proposed policies: " %bothRoundsF build%
             "\n    votes: " %ballotMapF build)
            _leaderBallotId
            _leaderProposedPolicies
            _leaderVotes

-- | Initial state of the leader.
instance Default (LeaderState pv) where
    def = LeaderState def def mempty mempty mempty mempty

-- ** Acceptor

-- | State kept by acceptor.
data AcceptorState pv = AcceptorState
    { _acceptorId                   :: AcceptorId
      -- ^ Identificator of this acceptor, should be read-only
    , _acceptorLastKnownBallotId    :: BallotId
      -- ^ Last heard ballotId from leader
    , _acceptorCStruct              :: ForBothRoundTypes Configuration
      -- ^ Gathered CStruct so far
    , _acceptorFastProposedPolicies :: ProposedCommands Policy
      -- ^ Policies proposed upon each fast ballot
    }

makeLenses ''AcceptorState

instance ProtocolVersion pv => Buildable (AcceptorState pv) where
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
defAcceptorState :: forall pv. AcceptorId -> (AcceptorState pv)
defAcceptorState id = AcceptorState id def def def

-- ** Learner

-- | State kept by learner.
data LearnerState pv = LearnerState
    { _learnerVotes   :: Votes (VersionQuorum pv) Configuration
      -- ^ CStructs received from acceptors so far
    , _learnerLearned :: Configuration
      -- ^ Eventually learned cstruct, result of consensus
    }

makeLenses ''LearnerState

instance Buildable (LearnerState pv) where
    build LearnerState{..} =
        bprint
            ("\n    heard: "%build%
             "\n    learned: "%build)
            _learnerVotes
            _learnerLearned

-- | Initial state of the learner.
instance Default (LearnerState pv) where
    def = LearnerState mempty def

-- * Misc

data AllStates pv = AllStates
    { proposerState   :: ProposerState pv
    , leaderState     :: (LeaderState pv)
    , acceptorsStates :: [AcceptorState pv]
    , learnersStates  :: [LearnerState pv]
    }

instance ProtocolVersion pv =>
         Buildable (AllStates pv) where
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
       , MonadRpc RpcOptions m
       , MonadReader (ProcessContext ctx) m
       , Message msg
       , HasMembers
       )
    => [NetworkAddress] -> msg -> m ()
broadcastTo getAddresses msg = do
    let addresses = getAddresses
    forM_ addresses $ \addr -> submit addr msg

{-# LANGUAGE GADTs                #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Various contexts of processes

module Sdn.Protocol.Context where

import           Control.Lens           (At (..), Index, IxValue, Ixed (..), at,
                                         makeLenses, (.=), (<<.=))
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
    launchPureLog ({-# SCC state_modification #-} atomicallyModifyMemStorage pcState) modifier

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
ballotMapF f = listF "\n  " (pairF (build%": "%f))

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

-- ** Leader

type PerCmdVotes qf cstruct = Map (RawCmd cstruct) $ Votes qf AcceptanceType

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

bothRoundsF :: Format Builder (a -> Builder) -> Format r (ForBothRoundTypes a -> r)
bothRoundsF fmt = later $ \ForBothRoundTypes{..} ->
    bprint ("_fast_: "%fmt) _forClassic <>
    bprint ("\n  _classic_: "%fmt) _forFast

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

-- ** Acceptor

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

-- ** Learner

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

-- * Misc

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

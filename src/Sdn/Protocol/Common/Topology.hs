{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Running all of processes required for consensus.

module Sdn.Protocol.Common.Topology where

import           Control.Monad.Catch          (Handler (..), catches)
import           Control.TimeWarp.Logging     (WithNamedLogger, getLoggerName,
                                               modifyLoggerName, setLoggerName)
import           Control.TimeWarp.Rpc         (Method (..), MonadRpc, serve)
import           Control.TimeWarp.Timed       (Microsecond, MonadTimed, for, fork_, hour,
                                               interval, ms, till, virtualTime, wait,
                                               work)
import           Data.Default                 (Default (..))
import           Formatting                   (build, sformat, shown, stext, (%))
import           System.Random                (StdGen, split)
import           Test.QuickCheck              (arbitrary)
import           Universum

import           Sdn.Base
import           Sdn.Extra                    (MonadLog, MonadReporting, RpcOptions,
                                               coloredF, gray, logError, logInfo,
                                               loggerNameT, mkMemStorage, prepareToAct,
                                               readMemStorage, resetColoring, withColor)
import           Sdn.Extra.MemStorage
import           Sdn.Extra.Networking
import qualified Sdn.Extra.Schedule           as S
import qualified Sdn.Policy.Fake              as Fake
import           Sdn.Protocol.Common.Context
import           Sdn.Protocol.Common.Messages
import           Sdn.Protocol.Common.Phases   (BatchingSettings, LearningCallback,
                                               MakeProposal, PolicyTargets (..),
                                               confirmCommitted, isPolicyUnconfirmed)
import           Sdn.Protocol.Processes
import           Sdn.Protocol.Versions

-- | Constraints for running a topology.
type MonadTopology m =
    ( MonadIO m
    , MonadCatch m
    , WithNamedLogger m
    , MonadLog m
    , MonadReporting m
    , MonadTimed m
    , MonadRpc RpcOptions m
    , DeclaresMemStore m
    , PracticalCStruct (DeclaredCStruct m)
    )

type TopologySchedule p = forall m. MonadTopology m => S.Schedule m p

-- | Contains all info to build network which serves consensus algorithm.
data TopologySettings pv cstruct = TopologySettings
    { topologyMembers               :: Members
      -- ^ Participants of given topology.
    , topologyProposalSchedule      :: TopologySchedule (RawCmd cstruct)
      -- ^ Given schedule of ballots,
      -- schedule according to which policies are generated and proposed.
    , topologyProposerInsistance    :: TopologySchedule () -> TopologySchedule ()
      -- ^ Schedule for repeating proposals of single policy.
      -- It will automatically stop executing when proposer finds out that
      -- policy is learned.
    , topologyBallotsSchedule       :: TopologySchedule ()
      -- ^ Schedule of ballots.
    , topologyProposalBatchSettings :: Maybe BatchingSettings
      -- ^ Proposal batching, if specified
    , topologyLifetime              :: Microsecond
      -- ^ How long network should exist.
      -- Once the hour comes, all actions are halted.
    , topologyCustomSettings        :: CustomTopologySettings pv
      -- ^ Settings related to particular version of consensus algorithm.
    }

data family CustomTopologySettings :: * -> *

-- | Example of topology.
instance Default (CustomTopologySettings pv) =>
         Default (TopologySettings pv Fake.Configuration) where
    def =
        TopologySettings
        { topologyMembers = def
        , topologyProposalSchedule = S.generate (Fake.GoodPolicy <$> arbitrary)
        , topologyProposerInsistance = \ballotSchedule -> do
            every3rd <- S.maskExecutions (cycle [True, False, False])
            ballotSchedule <* every3rd
        , topologyBallotsSchedule = S.execute
        , topologyProposalBatchSettings = Nothing
        , topologyLifetime = interval 999 hour
        , topologyCustomSettings = def
        }

-- | Provides info about topology in runtime.
data TopologyMonitor pv m = TopologyMonitor
    { -- | Returns when all active processes in the topology finish
      awaitTermination :: m ()
      -- | Fetch states of all processes in topology
    , readAllStates    :: DeclaredMemStoreTxMonad m (AllStates pv (DeclaredCStruct m))
    }

type ProcessEnv p pv m =
    ProcessContext $
    DeclaredMemStore m $
    ProcessState p pv (DeclaredCStruct m)

-- | Monad in which process (and phases) are supposed to work.
type ProcessM p pv m = ReaderT (ProcessEnv p pv m) m

-- | Create single process.
-- Return handler to read process state.
newProcess
    :: forall p pv m.
       ( MonadIO m
       , MonadTimed m
       , WithNamedLogger m
       , Process p
       , ProtocolVersion pv
       , DeclaresMemStore m
       , Default (DeclaredCStruct m)
       )
    => p
    -> ProcessM p pv m ()
    -> m (DeclaredMemStoreTxMonad m (ProcessState p pv (DeclaredCStruct m)))
newProcess process action = do
    memStorage <- getMemStorage
    stateBox <- mkMemStorage memStorage (initProcessState process)
    fork_ $
        flip runReaderT (ProcessContext stateBox) $
        modifyLoggerName (<> coloredProcessName process) $
        action
    return $ readMemStorage memStorage stateBox

type Listener p pv m = ProcessM p pv m $ Method RpcOptions $ ProcessM p pv m

-- | Construct single messages listener for process.
-- This makes all incoming messages logged, and processes
-- errors occured in listener.
listener
    :: forall p pv msg m.
       ( MonadCatch m
       , MonadLog m
       , MonadReporting m
       , WithNamedLogger m
       , Message msg
       , Buildable msg
       , HasMessageShortcut msg
       , Process p
       , HasContextOf p pv m
       , MonadRpc RpcOptions m
       )
    => (msg -> m ()) -> m (Method RpcOptions m)
listener endpoint = do
    logName <- getLoggerName
    let logName' = loggerNameMod logName
    return . Method $ \msg -> do
        setLoggerName logName' $ do
            logInfo $ sformat (coloredF gray (stext%" "%build)) "<--" msg
            endpoint msg `catches` handlers
  where
    loggerNameMod = (loggerNameT %~ withColor (processColor @p))
                  . (<> messageShortcut @msg)
                  . (loggerNameT %~ resetColoring)
    handlers =
        [ Handler $ logError . sformat (build @ProtocolError)
        , Handler $ logError . sformat ("Strange error: "%shown @SomeException)
        ]

type TopologyLauncher pv m =
    MonadTopology m =>
    StdGen -> TopologySettings pv (DeclaredCStruct m) -> m (TopologyMonitor pv m)

-- | How should various processes react on events.
data ProtocolListeners pv m = ProtocolListeners
    { leaderListeners   :: HasMembersInfo => [Listener Leader pv m]
    , acceptorListeners :: HasMembersInfo => [Listener Acceptor pv m]
    , learnerListeners  :: HasMembersInfo => [Listener Learner pv m]
    }

-- | How processes of various roles are supposed to work.
data TopologyActions pv m = TopologyActions
    { proposeAction     :: HasMembersInfo => MakeProposal (ProcessM Proposer pv m)
    , startBallotAction :: HasMembersInfo => ProcessM Leader pv m ()
    , topologyListeners :: ProtocolListeners pv m
    }

-- | Launch Paxos algorithm.
-- This function gathers all actions and listeners together in a small network.
launchPaxosWith
    :: forall pv m.
       ProtocolVersion pv
    => TopologyActions pv m -> TopologyLauncher pv m
launchPaxosWith TopologyActions{..} seed TopologySettings{..} =
  withMembers topologyMembers $
  withMembersAddresses def $ do
    let (proposalSeed, ballotSeed) = split seed
    let ProtocolListeners{..} = topologyListeners

    getProposerState <- newProcess Proposer . work (for topologyLifetime) $ do
        skipFirst <- S.maskExecutions (False : repeat True)
        makeProposal <- prepareToAct proposeAction

        S.runSchedule_ proposalSeed . S.limited topologyLifetime $ do
            -- wait for servers to bootstrap
            S.delayed (interval 10 ms)
            policy <- topologyProposalSchedule
            let repetitions = do
                    topologyProposerInsistance topologyBallotsSchedule
                    skipFirst
                    S.executeWhile (isPolicyUnconfirmed policy) pass
            S.execute <> repetitions
            lift $ makeProposal policy

        serve (processPort Proposer) =<< sequence
            [ listener @Proposer confirmCommitted
            ]

    getLeaderState <- newProcess Leader . work (for topologyLifetime) $ do
        -- wait for first proposal before start
        S.runSchedule_ ballotSeed $ do
            S.delayed (interval 20 ms)
            S.limited topologyLifetime topologyBallotsSchedule
            lift startBallotAction

        serve (processPort Leader) =<< sequence leaderListeners

    getAcceptorsStates <-
        startListeningProcessesOf Acceptor acceptorListeners

    getLearnersStates <-
        startListeningProcessesOf Learner learnerListeners

    let readAllStates =
            AllStates
            <$> getProposerState
            <*> getLeaderState
            <*> sequence getAcceptorsStates
            <*> sequence getLearnersStates

    curTime <- virtualTime
    let awaitTermination = wait (till 100 ms (curTime + topologyLifetime))

    return TopologyMonitor{..}
  where
    -- launch required number of servers, for processes of given type,
    -- serving given listeners
    startListeningProcessesOf
        :: (HasMembersInfo, MonadTopology m, Process p, Integral i, ProtocolVersion pv, DeclaresMemStore m, Default (DeclaredCStruct m))
        => (i -> p)
        -> [Listener p pv m]
        -> m [DeclaredMemStoreTxMonad m $ ProcessState p pv (DeclaredCStruct m)]
    startListeningProcessesOf processType listeners =
        forM (processesOf processType) $
            \process -> newProcess process . work (for topologyLifetime) $ do
                serve (processPort process) =<< sequence listeners

{-# NOINLINE launchPaxosWith #-}

data ProtocolListenersSettings m = ProtocolListenersSettings
    { listenersLearningCallback :: LearningCallback m
    , listenersPolicyTargets    :: PolicyTargets (DeclaredCStruct m)
    }

instance Applicative m => Default (ProtocolListenersSettings m) where
    def = ProtocolListenersSettings mempty def

-- | Version-custom topology settings.
class ProtocolVersion pv =>
      HasVersionProtocolListeners pv where
    -- | On which messages and how should each process respond.
    versionProtocolListeners
        :: forall m.
           MonadTopology m
        => ProtocolListenersSettings m -> ProtocolListeners pv m

-- | Version-custom topology settings.
class ProtocolVersion pv =>
      HasVersionTopologyActions pv where
    -- | On which messages and how should each process respond.
    versionTopologyActions
        :: forall m.
           MonadTopology m
        => TopologySettings pv (DeclaredCStruct m) -> TopologyActions pv m

-- | Launch version of paxos.
launchPaxos :: HasVersionTopologyActions pv => TopologyLauncher pv m
launchPaxos gen settings = launchPaxosWith (versionTopologyActions settings) gen settings


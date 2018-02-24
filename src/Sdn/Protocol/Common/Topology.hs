{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Running bunch of processes of various roles.

module Sdn.Protocol.Common.Topology where

import           Control.Monad.Catch          (Handler (..), catches)
import           Control.TimeWarp.Logging     (WithNamedLogger, modifyLoggerName)
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
import           Sdn.Extra                    (Message, MonadLog, MonadReporting,
                                               coloredF, gray, logError, logInfo,
                                               loggerNameT, resetColoring, withColor)
import           Sdn.Protocol.Common.Messages
import           Sdn.Protocol.Common.Phases   (confirmCommitted, isPolicyUnconfirmed)
import           Sdn.Protocol.Context
import           Sdn.Protocol.Processes
import           Sdn.Protocol.Versions
import qualified Sdn.Schedule                 as S

-- | Constraints for running a topology.
type MonadTopology m =
    ( MonadIO m
    , MonadCatch m
    , WithNamedLogger m
    , MonadLog m
    , MonadReporting m
    , MonadTimed m
    , MonadRpc m
    )

type TopologySchedule p = forall m. MonadTopology m => S.Schedule m p

-- | Contains all info to build network which serves consensus algorithm.
data TopologySettings pv = TopologySettings
    { topologyMembers            :: Members
      -- ^ Participants of given topology.
    , topologyProposalSchedule   :: TopologySchedule Policy
      -- ^ Given schedule of ballots,
      -- schedule according to which policies are generated and proposed.
    , topologyProposerInsistance :: TopologySchedule () -> TopologySchedule ()
      -- ^ Schedule for repeating proposals of single policy.
      -- It will automatically stop executing when proposer finds out that
      -- policy is learned.
    , topologyBallotsSchedule    :: TopologySchedule ()
      -- ^ Schedule of ballots.
    , topologyLifetime           :: Microsecond
      -- ^ How long network should exist.
      -- Once the hour comes, all actions are halted.
    , topologyCustomSettings     :: CustomTopologySettings pv
      -- ^ Settings related to particular version of consensus algorithm.
    }

data family CustomTopologySettings :: * -> *

-- | Example of topology.
instance Default (CustomTopologySettings pv) =>
         Default (TopologySettings pv) where
    def =
        TopologySettings
        { topologyMembers = def
        , topologyProposalSchedule = S.generate (GoodPolicy <$> arbitrary)
        , topologyProposerInsistance =
            \balSch -> balSch
        , topologyBallotsSchedule = S.execute
        , topologyLifetime = interval 999 hour
        , topologyCustomSettings = def
        }

-- | Provides info about topology in runtime.
data TopologyMonitor pv m = TopologyMonitor
    { -- | Returns when all active processes in the topology finish
      awaitTermination :: m ()
      -- | Fetch states of all processes in topology
    , readAllStates    :: STM (AllStates pv)
    }

-- | Monad in which process (and phases) are supposed to work.
type ProcessM p pv m = ReaderT (ProcessContext $ ProcessState p pv) m

-- | Create single process.
newProcess
    :: forall p pv m.
       ( MonadIO m
       , MonadTimed m
       , WithNamedLogger m
       , Process p
       , ProtocolVersion pv
       )
    => p
    -> ProcessM p pv m ()
    -> m (STM (ProcessState p pv))
newProcess process action = do
    stateBox <- liftIO $ newTVarIO (initProcessState process)
    fork_ $
        flip runReaderT (ProcessContext stateBox) $
        modifyLoggerName (<> coloredProcessName process) $
        action
    return $ readTVar stateBox

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
       )
    => (msg -> m ()) -> Method m
listener endpoint = Method . clarifyLoggerName $ \msg -> do
    logInfo $ sformat (coloredF gray (stext%" "%build)) "<--" msg
    endpoint msg `catches` handlers
  where
    loggerNameMod = (loggerNameT %~ withColor (processColor @p))
                  . (<> messageShortcut @msg)
                  . (loggerNameT %~ resetColoring)
    clarifyLoggerName f (msg :: msg) = modifyLoggerName loggerNameMod $ f msg
    handlers =
        [ Handler $ logError . sformat (build @ProtocolError)
        , Handler $ logError . sformat ("Strange error: "%shown @SomeException)
        ]

type TopologyLauncher pv m =
    MonadTopology m => StdGen -> TopologySettings pv -> m (TopologyMonitor pv m)

-- | How processes of various roles are supposed to work.
data TopologyActions pv m = TopologyActions
    { proposeAction     :: HasMembers => Policy -> ProcessM Proposer pv m ()
    , startBallotAction :: HasMembers => ProcessM Leader pv m ()
    , leaderListeners   :: HasMembers => [Method $ ProcessM Leader pv m]
    , acceptorListeners :: HasMembers => [Method $ ProcessM Acceptor pv m]
    , learnerListeners  :: HasMembers => [Method $ ProcessM Learner pv m]
    }

-- | Launch Paxos algorithm.
-- This function gathers all actions and listeners together in a small network.
launchPaxosWith :: ProtocolVersion pv => TopologyActions pv m -> TopologyLauncher pv m
launchPaxosWith TopologyActions{..} seed TopologySettings{..} = withMembers topologyMembers $ do
    let (proposalSeed, ballotSeed) = split seed

    proposerState <- newProcess Proposer . work (for topologyLifetime) $ do
        S.runSchedule_ proposalSeed . S.limited topologyLifetime $ do
            -- wait for servers to bootstrap
            S.delayed (interval 10 ms)
            policy <- topologyProposalSchedule
            let repetitions =
                    S.executeWhile (isPolicyUnconfirmed policy) $
                    topologyProposerInsistance topologyBallotsSchedule
            S.execute <> repetitions
            lift $ proposeAction policy

        serve (processPort Proposer)
            [ listener @Proposer confirmCommitted
            ]

    leaderState <- newProcess Leader . work (for topologyLifetime) $ do
        -- wait for first proposal before start
        S.runSchedule_ ballotSeed $ do
            S.delayed (interval 20 ms)
            S.limited topologyLifetime topologyBallotsSchedule
            lift startBallotAction

        serve (processPort Leader) leaderListeners

    acceptorsStates <-
        startListeningProcessesOf Acceptor acceptorListeners

    learnersStates <-
        startListeningProcessesOf Learner learnerListeners

    let readAllStates =
            AllStates
            <$> proposerState
            <*> leaderState
            <*> sequence acceptorsStates
            <*> sequence learnersStates

    curTime <- virtualTime
    let awaitTermination = wait (till 100 ms (curTime + topologyLifetime))

    return TopologyMonitor{..}
  where
    -- launch required number of servers, for processes of given type,
    -- serving given listeners
    startListeningProcessesOf
        :: (HasMembers, MonadTopology m, Process p, Integral i, ProtocolVersion pv)
        => (i -> p)
        -> [Method $ ProcessM p pv m]
        -> m [STM $ ProcessState p pv]
    startListeningProcessesOf processType listeners =
        forM (processesOf processType) $
            \process -> newProcess process . work (for topologyLifetime) $ do
                serve (processPort process) listeners

{-# NOINLINE launchPaxosWith #-}

-- | Version-custom topology settings.
class ProtocolVersion pv =>
      HasVersionTopologyActions pv where
    -- | On which messages and how should each process respond.
    versionTopologyActions
        :: forall m.
           MonadTopology m
        => CustomTopologySettings pv -> TopologyActions pv m

-- | Launch version of paxos.
launchPaxos :: HasVersionTopologyActions pv => TopologyLauncher pv m
launchPaxos gen settings = launchPaxosWith (versionTopologyActions customSettings) gen settings
  where
    customSettings = topologyCustomSettings settings



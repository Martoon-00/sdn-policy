{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}

-- | Make up network layout

module Sdn.Protocol.Topology where

import           Control.Monad.Catch      (Handler (..), catches)
import           Control.TimeWarp.Logging (WithNamedLogger, modifyLoggerName)
import           Control.TimeWarp.Rpc     (Method (..), MonadRpc, serve)
import           Control.TimeWarp.Timed   (Microsecond, MonadTimed, for, fork_, hour,
                                           interval, ms, sec, till, virtualTime, wait,
                                           work)
import           Data.Default             (Default (..))
import           Formatting               (build, sformat, shown, (%))
import           Test.QuickCheck          (arbitrary)
import           Universum

import           Sdn.Base
import           Sdn.Extra
import           Sdn.Protocol.Context
import           Sdn.Protocol.Phases
import           Sdn.Protocol.Processes
import           Sdn.Protocol.Versions
import           Sdn.Schedule

type TopologySchedule p = forall m. MonadTopology m => Schedule m p

-- | Contains all info to build network which serves consensus algorithm.
data TopologySettings = TopologySettings
    { topologyMembers          :: Members
    , topologyProposalSchedule :: TopologySchedule Policy
    , topologyBallotsSchedule  :: TopologySchedule ()
    , topologyRecoveryDelay    :: Microsecond
    , topologySeed             :: GenSeed
    , topologyLifetime         :: Microsecond
    }

-- | Example of topology.
instance Default TopologySettings where
    def =
        TopologySettings
        { topologyMembers = def
        , topologyProposalSchedule = generate (GoodPolicy <$> arbitrary)
        , topologyBallotsSchedule = execute
        , topologyRecoveryDelay = interval 1 sec
        , topologySeed = RandomSeed
        , topologyLifetime = interval 999 hour
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

-- | Create single newProcess.
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
    stateBox <- liftIO $ newTVarIO (initProcessState Proxy process)
    fork_ $
        flip runReaderT (ProcessContext stateBox) $
        modifyLoggerName (<> processName process) $
        action
    return $ readTVar stateBox

-- | Create single messages listener for server.
listener
    :: ( MonadCatch m
       , MonadLog m
       , MonadReporting m
       , Message msg
       , Buildable msg
       )
    => (msg -> m ()) -> Method m
listener endpoint = Method $ \msg -> do
    logInfo $ sformat ("Incoming message: "%build) msg
    endpoint msg `catches` handlers
  where
    handlers =
        [ Handler $ logError . sformat (build @ProtocolError)
        , Handler $ logError . sformat ("Strange error: "%shown @SomeException)
        ]

type MonadTopology m =
    ( MonadIO m
    , MonadCatch m
    , WithNamedLogger m
    , MonadLog m
    , MonadReporting m
    , MonadTimed m
    , MonadRpc m
    )

type TopologyLauncher pv m =
    MonadTopology m => TopologySettings -> m (TopologyMonitor pv m)

data TopologyActions pv m = TopologyActions
    { proposeAction     :: HasMembers => Policy -> ProcessM Proposer pv m ()
    , startBallotAction :: HasMembers => ProcessM Leader pv m ()
    , leaderListeners   :: HasMembers => [Method $ ProcessM Leader pv m]
    , acceptorListeners :: HasMembers => [Method $ ProcessM Acceptor pv m]
    , learnerListeners  :: HasMembers => [Method $ ProcessM Learner pv m]
    }

-- | Launch Paxos algorithm.
launchPaxos :: ProtocolVersion pv => TopologyActions pv m -> TopologyLauncher pv m
launchPaxos TopologyActions{..} TopologySettings{..} = withMembers topologyMembers $ do
    let (proposalSeed, ballotSeed) = splitGenSeed topologySeed

    proposerState <- newProcess Proposer . work (for topologyLifetime) $ do
        -- wait for servers to bootstrap
        wait (for 10 ms)
        runSchedule_ proposalSeed $ do
            policy <- limited topologyLifetime topologyProposalSchedule
            lift $ proposeAction policy

    leaderState <- newProcess Leader . work (for topologyLifetime) $ do
        -- wait for first proposal before start
        wait (for 20 ms)
        runSchedule_ ballotSeed $ do
            limited topologyLifetime topologyBallotsSchedule
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
    startListeningProcessesOf
        :: (HasMembers, MonadTopology m, Process p, Integral i, ProtocolVersion pv)
        => (i -> p)
        -> [Method $ ProcessM p pv m]
        -> m [STM $ ProcessState p pv]
    startListeningProcessesOf processType listeners =
        forM (processesOf processType) $
            \process -> newProcess process . work (for topologyLifetime) $ do
                serve (processPort process) listeners

{-# NOINLINE launchPaxos #-}

-- | Launch Classic Paxos.
launchClassicPaxos :: forall m. TopologyLauncher Classic m
launchClassicPaxos = launchPaxos
    TopologyActions
    { proposeAction = propose
    , startBallotAction = phase1a
    , leaderListeners =
        [ listener rememberProposal
        , listener phase2a
        ]
    , acceptorListeners =
        [ listener phase1b
        , listener phase2b
        ]
    , learnerListeners =
        [ listener learn
        ]
    }

-- | Launch Classic Paxos.
launchFastPaxos :: forall m. TopologyLauncher Fast m
launchFastPaxos topologySettings@TopologySettings{..} = launchPaxos
    TopologyActions
    { proposeAction = proposeFast
    , startBallotAction = initFastBallot topologyRecoveryDelay
    , leaderListeners =
        [ listener rememberProposal
        , listener phase2a
        , listener detectConflicts
        ]
    , acceptorListeners =
        [ listener phase1b
        , listener phase2b
        , listener phase2bFast
        , listener acceptorRememberFastProposal
        ]
    , learnerListeners =
        [ listener learn
        , listener learnFast
        ]
    }
    topologySettings

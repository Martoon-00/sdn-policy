{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Make up network layout

module Sdn.Protocol.Topology where

import           Control.Monad.Catch      (Handler (..), catches)
import           Control.TimeWarp.Logging (WithNamedLogger, modifyLoggerName)
import           Control.TimeWarp.Rpc     (Method (..), MonadRpc, serve)
import           Control.TimeWarp.Timed   (Microsecond, MonadTimed, for, fork_, hour,
                                           interval, ms, till, virtualTime, wait, work)
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
    , topologySeed             :: GenSeed
    , topologyBallotsSchedule  :: TopologySchedule ()
    , topologyLifetime         :: Microsecond
    }

-- | Example of topology.
instance Default TopologySettings where
    def =
        TopologySettings
        { topologyMembers = def
        , topologyProposalSchedule = generate (GoodPolicy <$> arbitrary)
        , topologyBallotsSchedule = execute
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
    -> ReaderT (ProcessContext (ProcessState p pv)) m ()
    -> m (STM (ProcessState p pv))
newProcess process action = do
    stateBox <- liftIO $ newTVarIO (initProcessState process)
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

type TopologyLauncher pv =
    forall m. MonadTopology m => TopologySettings -> m (TopologyMonitor pv m)

-- | Launch Classic Paxos algorithm.
launchClassicPaxos :: TopologyLauncher Classic
launchClassicPaxos TopologySettings{..} = withMembers topologyMembers $ do
    let (proposalSeed, ballotSeed) = splitGenSeed topologySeed

    proposerState <- newProcess Proposer . work (for topologyLifetime) $ do
        -- wait for servers to bootstrap
        wait (for 10 ms)
        runSchedule_ proposalSeed $ do
            policy <- limited topologyLifetime topologyProposalSchedule
            lift $ propose policy

    leaderState <- newProcess Leader . work (for topologyLifetime) $ do
        work (for topologyLifetime) $ do
            -- wait for first proposal before start
            wait (for 20 ms)
            runSchedule_ ballotSeed $ topologyBallotsSchedule >> lift phrase1a

        serve (processPort Leader)
            [ listener rememberProposal
            , listener phase2a
            ]

    acceptorsStates <- forM (processesOf Acceptor) $
        \process -> newProcess process . work (for topologyLifetime) $ do
            serve (processPort process)
                [ listener phase1b
                , listener phase2b
                ]

    learnersStates <- forM (processesOf Learner) $
        \process -> newProcess process . work (for topologyLifetime) $ do
            serve (processPort process)
                [ listener learn
                ]

    let readAllStates =
            AllStates
            <$> proposerState
            <*> leaderState
            <*> sequence acceptorsStates
            <*> sequence learnersStates

    curTime <- virtualTime
    let awaitTermination = wait (till 100 ms (curTime + topologyLifetime))

    return TopologyMonitor{..}
{-# NOINLINE launchClassicPaxos #-}

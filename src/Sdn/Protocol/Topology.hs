{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Make up network layout

module Sdn.Protocol.Topology where

import           Control.Monad.Catch      (Handler (..), catches)
import           Control.Monad.Reader     (withReaderT)
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
data TopologyMonitor m = TopologyMonitor
    { -- | Returns when all active processes in the topology finish
      awaitTermination :: m ()
      -- | Fetch states of all processes in topology
    , readAllStates    :: STM AllStates
    }

runWithMembers :: Monad m => Members -> ReaderT Members m a -> m a
runWithMembers = flip runReaderT

-- | Create single newProcess.
newProcess
    :: (MonadIO m, MonadTimed m, WithNamedLogger m, Process p)
    => p
    -> ReaderT (ProcessContext (ProcessState p)) m ()
    -> ReaderT Members m (STM (ProcessState p))
newProcess process action = do
    stateBox <- liftIO $ newTVarIO (initProcessState process)
    fork_ $
        withReaderT (ProcessContext stateBox) $
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
        [ Handler @_ @_ @ProtocolError $ logError . sformat build
        , Handler @_ @_ @SomeException $ logError . sformat ("Strange error: "%shown)
        ]

type MonadTopology m =
    ( MonadIO m
    , MonadCatch m
    , WithNamedLogger m
    , MonadReporting m
    , MonadTimed m
    , MonadRpc m
    )

type TopologyLauncher =
    forall m. MonadTopology m => TopologySettings -> m (TopologyMonitor m)

-- | Launch Classic Paxos algorithm.
launchClassicPaxos :: TopologyLauncher
launchClassicPaxos TopologySettings{..} = runWithMembers topologyMembers $ do
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

    acceptorsStates <- forM (processesOf Acceptor topologyMembers) $
        \process -> newProcess process . work (for topologyLifetime) $ do
            serve (processPort process)
                [ listener phase1b
                , listener phase2b
                ]

    learnersStates <- forM (processesOf Learner topologyMembers) $
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

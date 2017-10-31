-- | Make up network layout

module Sdn.Topology where

import           Control.TimeWarp.Rpc   (Method (..), MonadRpc, serve)
import           Control.TimeWarp.Timed (Microsecond, MonadTimed, for, fork_, interval,
                                         sec, wait, work)
import           Data.Default           (Default (..))
import           System.Wlog            (CanLog, LoggerNameBox, usingLoggerName)
import           Test.QuickCheck        (arbitrary)
import           Universum

import           Sdn.Context
import           Sdn.Messages
import           Sdn.Phases
import           Sdn.Policy
import           Sdn.Processes
import           Sdn.ProposalStrategy
import           Sdn.Types
import           Sdn.Util

-- | Contains all info to build network which serves consensus algorithm.
data Topology = Topology
    { topologyMembers          :: Members
    , topologyProposalStrategy :: ProposalStrategy Policy
    , topologySeed             :: GenSeed
    , topologyBallotDuration   :: Microsecond
    , topologyLifetime         :: Microsecond
    }

-- | Example of topology.
instance Default Topology where
    def =
        Topology
        { topologyMembers = def
        , topologyProposalStrategy =
              generating (interval 3 sec) (GoodPolicy <$> arbitrary)
        , topologySeed = RandomSeed
        , topologyBallotDuration = interval 3 sec
        , topologyLifetime = interval 15 sec
        }

-- | Create single newProcess.
newProcess
    :: (MonadIO m, MonadTimed m, Process p)
    => p
    -> LoggerNameBox (ReaderT (ProcessContext (ProcessState p)) m) ()
    -> ReaderT Members m ()
newProcess process action =
    fork_ $
    inProcessCtx process $
    usingLoggerName (processName process) $
    action

launchClassicPaxos
    :: (MonadIO m, CanLog m, MonadTimed m, MonadRpc m)
    => Topology -> m ()
launchClassicPaxos Topology{..} = flip runReaderT topologyMembers $ do
    newProcess Proposer $ do
        let strategy = limited topologyLifetime topologyProposalStrategy
        execStrategy topologySeed strategy $ \policy ->
            submit (processAddress Leader) (ProposalMsg policy)

    newProcess Leader $ do
        work (for topologyLifetime) $
            forever $ phrase1a >> wait (for topologyBallotDuration)

        serve (processPort Leader)
            [ Method rememberProposal
            , Method phase2a
            ]

    forM_ (processesOf Acceptor topologyMembers) $
        \process -> newProcess process $ do
            serve (processPort process)
                [ Method phase1b
                , Method phase2b
                ]

    forM_ (processesOf Learner topologyMembers) $
        \process -> newProcess process $ do
             serve (processPort process)
                [ Method learn
                ]



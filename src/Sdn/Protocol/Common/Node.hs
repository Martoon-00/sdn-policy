{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- | Running processes required for single node.

module Sdn.Protocol.Common.Node where

import           Control.Monad.Reader         (withReaderT)
import           Control.Monad.Trans.Control  (embed_)
import           Control.TimeWarp.Logging     (WithNamedLogger, modifyLoggerName,
                                               usingLoggerName)
import           Control.TimeWarp.Rpc         (Dict (..), Method, Port, hoistMethod,
                                               localhost, pickEvi, runMsgPackRpc, serve,
                                               withExtendedRpcOptions)
import           Control.TimeWarp.Timed       (fork_)
import           Data.Default                 (Default (..))
import           Data.Tagged                  (Tagged (..), untag)
import           Universum

import           Sdn.Base
import           Sdn.Extra.Logging
import           Sdn.Extra.MemStorage
import           Sdn.Extra.Util               (declareMonadicMark, prepareToAct)
import           Sdn.Protocol.Common.Context
import           Sdn.Protocol.Common.Phases   (BatchingSettings (..),
                                               LearningCallback (..), batchingProposal)
import           Sdn.Protocol.Common.Topology (ProcessEnv, ProcessM,
                                               ProtocolListeners (..),
                                               ProtocolListenersSettings (..),
                                               versionProtocolListeners)
import qualified Sdn.Protocol.Fast            as Fast
import           Sdn.Protocol.Processes
import           Sdn.Protocol.Versions


-- | Identifier of controller process.
newtype ProcessId = ProcessId Int
    deriving (Eq, Ord, Enum, Show, Num, Real, Integral)

-- | Options assosiated with policies compoition protocol.
data ProtocolOptions = ProtocolOptions
    { protocolPorts             :: ProcessId -> Port
      -- ^ Port, with which given process which should participate in consensus protocol.
    , protocolTotalProcesses    :: Int
      -- ^ Overall number of controllers in network.
    , protocolLeaderId          :: ProcessId
      -- ^ Whether given controller is Paxos leader.
      -- For now this thing will be hardcoded.
    , protocolProposalsBatching :: BatchingSettings
    }

-- | Initiate some events in protocol.
data ProtocolHandlers cstruct = ProtocolHandlers
    { protocolMakeProposal :: RawCmd cstruct -> IO ()
      -- ^ Induce new policy proposal.
    , protocolShutdown     :: IO ()
      -- ^ Stop protocol execution.
    }

-- | How to react on various events in consensus protocol.
data ProtocolCallbacks cstruct = ProtocolCallbacks
    { protocolOnLearned :: NonEmpty (Cmd cstruct) -> IO ()
      -- ^ Executed when policy is learned.
    }

type TaggedProcessEnv p pv m = Tagged p $ ProcessEnv p pv m

data SuperProcessState pv m = SuperProcessState
    { subProposerState :: TaggedProcessEnv Proposer pv m
    , subLeaderState   :: TaggedProcessEnv Leader pv m
    , subAcceptorState :: TaggedProcessEnv Acceptor pv m
    , subLearnerState  :: TaggedProcessEnv Learner pv m
    , superProcessId   :: ProcessId
    }

type SuperProcessM pv m = ReaderT (SuperProcessState pv m) m

withSuperProcessCtx
    :: ( MonadIO m
       , ProtocolVersion pv
       , DeclaresMemStore m
       , Default (DeclaredCStruct m)
       )
    => ProcessId -> SuperProcessM pv m a -> m a
withSuperProcessCtx processId action = do
    memStorage <- getMemStorage
    let mkProcessStore p = Tagged . ProcessContext
                       <$> mkMemStorage memStorage (initProcessState p)
    subProposerState <- mkProcessStore Proposer
    subLeaderState <- mkProcessStore Leader
    subAcceptorState <- mkProcessStore (Acceptor $ fromIntegral processId)
    subLearnerState <- mkProcessStore (Learner $ fromIntegral processId)
    let superProcessId = processId
    runReaderT action SuperProcessState{..}

inProcess
    :: forall p pv m a.
       (Monad m, WithNamedLogger m, Process p, HasMembers)
    => (SuperProcessState pv m -> Tagged p (ProcessEnv p pv m))
    -> ProcessM p pv m a
    -> SuperProcessM pv m a
inProcess getter action = do
    processId <- superProcessId <$> ask
    let process = takeSomeProcess @p processId
    withReaderT (untag . getter) $
        modifyLoggerName (<> coloredProcessName process) action

inListenerM
    :: forall p pv m (o :: [*]).
       (Monad m, WithNamedLogger m, Process p, HasMembers)
    => (SuperProcessState pv m -> Tagged p (ProcessEnv p pv m))
    -> ProcessM p pv m (Method o (ProcessM p pv m))
    -> SuperProcessM pv m (Method o (SuperProcessM pv m))
inListenerM getter = inProcess getter . fmap (hoistMethod $ inProcess getter)

runProtocolNode
    :: forall cstruct.
       (PracticalCStruct cstruct)
    => ProtocolOptions
    -> ProcessId
    -> ProtocolCallbacks cstruct
    -> (ProtocolHandlers cstruct -> IO ())
    -> IO ()
runProtocolNode ProtocolOptions{..} curProcessId ProtocolCallbacks{..} fillProtocolHandlers = do
    let runLogging = runNoErrorReporting . usingLoggerName mempty

    let learnersSettings = def
            { listenersLearningCallback = LearningCallback $ liftIO . protocolOnLearned }

    -- environment initialization
    runMsgPackRpc $ withExtendedRpcOptions (pickEvi Dict) $
        runLogging $
        declareMemStorage ioRefMemStorage $
        declareMonadicMark @(CStructType cstruct) $
        withMembers members $
        withMembersAddresses membersAddresses $
        withSuperProcessCtx curProcessId $ do
            protocolMakeProposal <- inProcess subProposerState $ do
                makeProposal <- prepareToAct $ batchingProposal protocolProposalsBatching Fast.propose
                embed_ makeProposal
            protocolShutdown <- pure $ return ()

            liftIO $ fillProtocolHandlers ProtocolHandlers{..}

            when (protocolLeaderId == curProcessId) $
                fork_ $ S.runSchedule_ (mkStdGen 0) $ do
                    S.periodic (interval 10 sec)
                    lift $ inProcess subLeaderState Classic.phase1a

            let ProtocolListeners{..} = versionProtocolListeners @Fast learnersSettings

            listeners <- sequence $ mconcat
                [ inListenerM subLeaderState <$> leaderListeners
                , inListenerM subAcceptorState <$> acceptorListeners
                , inListenerM subLearnerState <$> learnerListeners
                ]

            fork_ $ serve curPort listeners

  where
     curPort = protocolPorts curProcessId
     members =
         Members
         { acceptorsNum = protocolTotalProcesses
         , learnersNum = protocolTotalProcesses
         }
     membersAddresses = fmap (localhost, )
         MembersAddrInfo
         { proposerAddrInfo = Nothing
         , leaderAddrInfo = protocolPorts protocolLeaderId
         , acceptorsAddrInfos = protocolPorts . fromIntegral
         , learnersAddrInfos = protocolPorts . fromIntegral
         }



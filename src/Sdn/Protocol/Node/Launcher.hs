{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- | Running processes required for single node.

module Sdn.Protocol.Node.Launcher where

import           Control.Monad.Reader         (withReaderT)
import           Control.Monad.Trans.Control  (embed_)
import           Control.TimeWarp.Logging     (WithNamedLogger, modifyLoggerName, usingLoggerName)
import           Control.TimeWarp.Rpc         (Dict (..), Method, hoistMethod, localhost, pickEvi,
                                               runMsgPackUdpOpts, serve, udpMessageSizeLimit,
                                               withExtendedRpcOptions)
import           Control.TimeWarp.Timed       (fork_, interval, sec)
import           Data.Coerce                  (coerce)
import           Data.Default                 (Default (..))
import           Data.Tagged                  (Tagged (..), untag)
import           System.Random                (mkStdGen)
import           Universum

import           Sdn.Base
import           Sdn.Extra.Logging
import           Sdn.Extra.MemStorage
import           Sdn.Extra.Networking
import qualified Sdn.Extra.Schedule           as S
import           Sdn.Extra.Util               (declareMonadicMark, prepareToAct)
import qualified Sdn.Protocol.Classic         as Classic
import           Sdn.Protocol.Common.Context
import           Sdn.Protocol.Common.Phases   (LearningCallback (..), batchingProposal)
import           Sdn.Protocol.Common.Topology (ProcessEnv, ProcessM, ProtocolListeners (..),
                                               ProtocolListenersSettings (..), proposerListeners,
                                               versionProtocolListeners)
import qualified Sdn.Protocol.Fast            as Fast
import           Sdn.Protocol.Node.Options
import           Sdn.Protocol.Processes
import           Sdn.Protocol.Versions


-- | Initiate some events in protocol.
data ProtocolHandlers cstruct = ProtocolHandlers
    { protocolMakeProposal :: RawCmd cstruct -> IO ()
      -- ^ Induce new policy proposal.
    , protocolShutdown     :: IO ()
      -- ^ Stop protocol execution.
    , protocolProcessId    :: GeneralProcessId
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
    , superProcessId   :: GeneralProcessId
    }

type SuperProcessM pv m = ReaderT (SuperProcessState pv m) m

withSuperProcessCtx
    :: ( MonadIO m
       , ProtocolVersion pv
       , DeclaresMemStore m
       , Default (DeclaredCStruct m)
       )
    => GeneralProcessId -> SuperProcessM pv m a -> m a
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
    => NodeOptions
    -> GeneralProcessId
    -> ProtocolCallbacks cstruct
    -> IO (ProtocolHandlers cstruct)
runProtocolNode NodeOptions{..} curProcessId ProtocolCallbacks{..} = do
    let runLogging = runNoErrorReporting . usingLoggerName mempty . setDropLoggerName

    let learnersSettings = def
            { listenersLearningCallback = LearningCallback $ liftIO . protocolOnLearned }

    -- environment initialization
    -- runMsgPackRpc $ withExtendedRpcOptions (pickEvi Dict) $
    runMsgPackUdpOpts def{ udpMessageSizeLimit = 15000 } $ withExtendedRpcOptions (pickEvi Dict) $
        runListenersCacheFor (== curPort) $
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
            let protocolProcessId = curProcessId

            when (protocolLeaderId == coerce curProcessId) $
                fork_ $ S.runSchedule_ (mkStdGen 0) $ do
                    S.periodic (interval 1 sec)
                    lift $ inProcess subLeaderState Classic.phase1a

            let ProtocolListeners{..} = versionProtocolListeners @Fast learnersSettings

            listeners <- sequence $ mconcat
                [ inListenerM subProposerState <$> proposerListeners
                , inListenerM subLeaderState <$> leaderListeners
                , inListenerM subAcceptorState <$> acceptorListeners
                , inListenerM subLearnerState <$> learnerListeners
                ]

            fork_ $ serve curPort listeners

            return ProtocolHandlers{..}
  where
     curPort = protocolPorts curProcessId
     members =
         Members
         { acceptorsNum = protocolTotalProcesses
         , learnersNum = protocolTotalProcesses
         }
     membersAddresses = fmap (localhost, )
         MembersAddrInfo
         { proposerAddrInfo = ProposerAddrInfoEvaled protocolPorts
         , leaderAddrInfo = protocolPorts $ coerce protocolLeaderId
         , acceptorsAddrInfos = protocolPorts . fromIntegral
         , learnersAddrInfos = protocolPorts . fromIntegral
         }

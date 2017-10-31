{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Various contexts of processes

module Sdn.Context where

import           Control.Concurrent.STM (STM)
import           Control.Lens           (makeLenses)
import           Control.Monad.Reader   (withReaderT)
import           Control.TimeWarp.Rpc   (MonadRpc, NetworkAddress, RpcRequest (..))
import           Control.TimeWarp.Timed (MonadTimed)
import           Data.Default           (Default (def))
import           System.Wlog            (NamedPureLogger, WithLogger, launchNamedPureLog)
import           Universum

import           Sdn.Policy
import           Sdn.Quorum
import           Sdn.Roles
import           Sdn.Types
import           Sdn.Util


-- * General

data ProcessContext s = ProcessContext
    { pcState   :: TVar s
    , pcMembers :: Members
    }

type HasContext s m =
    ( MonadIO m
    , MonadReader (ProcessContext s) m
    )

withProcessState
    :: (MonadIO m, WithLogger m, MonadReader (ProcessContext s) m)
    => StateT s (NamedPureLogger STM) a -> m a
withProcessState modifier = do
    var <- pcState <$> ask
    launchNamedPureLog (liftIO . atomically) $ do
        st <- lift $ readTVar var
        (res, st') <- runStateT modifier st
        lift $ writeTVar var st'
        return res

inProcessCtx
    :: MonadIO m
    => s -> ReaderT (ProcessContext s) m a -> ReaderT Members m a
inProcessCtx initState action = do
    var <- liftIO $ newTVarIO initState
    withReaderT (ProcessContext var) action

ctxMembers :: MonadReader (ProcessContext s) m => m Members
ctxMembers = pcMembers <$> ask

-- * Per-process contexts
-- ** Leader

data LeaderState = LeaderState
    { _leaderBallotId        :: BallotId
    , _leaderCStruct         :: Configuration
    , _leaderPendingPolicies :: [Policy]
    , _leaderVotes           :: Map BallotId (Votes Configuration)
    }

makeLenses ''LeaderState

instance Default LeaderState where
    def = LeaderState def mempty mempty mempty

inLeaderCtx
    :: MonadIO m
    => ReaderT (ProcessContext LeaderState) m a -> ReaderT Members m a
inLeaderCtx = inProcessCtx def

-- ** Acceptor

data AcceptorState = AcceptorState
    { _acceptorId       :: AcceptorId
    , _acceptorBallotId :: BallotId
    , _acceptorCStruct  :: Configuration
    }

makeLenses ''AcceptorState

defAcceptorState :: AcceptorId -> AcceptorState
defAcceptorState id = AcceptorState id (BallotId (-1)) mempty

inAcceptorCtx
    :: MonadIO m
    => AcceptorId
    -> ReaderT (ProcessContext AcceptorState) m a
    -> ReaderT Members m a
inAcceptorCtx = inProcessCtx . defAcceptorState

-- ** Learner

data LearnerState = LearnerState
    { _learnerVotes   :: Votes Configuration
    , _learnerLearned :: Configuration
    }

makeLenses ''LearnerState

instance Default LearnerState where
    def = LearnerState mempty mempty

inLearnerCtx
    :: MonadIO m
    => ReaderT (ProcessContext LearnerState) m a -> ReaderT Members m a
inLearnerCtx = inProcessCtx def

-- * Misc

broadcastTo
    :: ( MonadTimed m
       , MonadRpc m
       , MonadReader (ProcessContext s) m
       , RpcRequest msg
       , Response msg ~ ()
       )
    => (Members -> [NetworkAddress]) -> msg -> m ()
broadcastTo getAddresses msg = do
    members <- pcMembers <$> ask
    let addresses = getAddresses members
    forM_ addresses $ \addr -> submit addr msg

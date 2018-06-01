{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | All network-related functions.

module Sdn.Extra.Networking
    ( submit
    , broadcastTo
    , declareMessage
    , RpcOptions
    , Message

    , ListenersCache (..)
    , runListenersCacheFor
    ) where

import           Control.Lens                 (at, ix, makeLensesFor, non', _Empty)
import           Control.Monad.Base           (MonadBase)
import           Control.Monad.Trans.Control  (MonadBaseControl (..))
import           Control.TimeWarp.Logging     (WithNamedLogger)
import           Control.TimeWarp.Rpc         (MonadRpc)
import qualified Control.TimeWarp.Rpc         as Rpc
import qualified Control.TimeWarp.Rpc.ExtOpts as ExtOpt
import           Control.TimeWarp.Timed       (MonadTimed, ThreadId)
import qualified Data.Typeable                as Typeable
import           Formatting                   (build, sformat, shown, (%))
import qualified Language.Haskell.TH          as TH
import           Universum

import           Sdn.Extra.Util               (MFunctored (..), WrappedM (..))

-- | Declare instance for one-way message.
declareMessage :: Rpc.MessageId -> TH.Name -> TH.Q [TH.Dec]
declareMessage msgId msgType = Rpc.mkRequest msgType ''() msgId

data RpcOptionTypeable
instance Rpc.RpcOptions RpcOptionTypeable where
    type RpcConstraints RpcOptionTypeable r =
        (Typeable r, Typeable (Rpc.Response r), Typeable (Rpc.ExpectedError r))

-- | Options used by protocol.
type RpcOptions = '[Rpc.RpcOptionMessagePack, Rpc.RpcOptionNoReturn, RpcOptionTypeable]

-- | Constraints required for any message type.
type Message msg = (Rpc.RpcRequest msg, Rpc.RpcConstraints RpcOptions msg)


-- | Alias for 'Rpc.send', restricted to messages satisfying 'Message' constraint.
submit
    :: (MonadCatch m, MonadTimed m, MonadRpc RpcOptions m, Message msg)
    => Rpc.NetworkAddress -> msg -> m ()
submit = Rpc.send

-- | Send a message to multiple given participants.
broadcastTo
    :: (MonadCatch m, MonadTimed m, MonadRpc RpcOptions m, Message msg)
    => [Rpc.NetworkAddress] -> msg -> m ()
broadcastTo getAddresses msg = do
    let addresses = getAddresses
    forM_ addresses $ \addr -> submit addr msg


-- | State for 'ListenersMemory'.
data KnownListeners (o :: [*]) m = KnownListeners
    { _klListeners :: Map Rpc.Port $ Map Rpc.MessageId (Rpc.Method o m)
    , _klIsSelf    :: Rpc.Port -> Bool
    }

makeLensesFor [("_klListeners", "klListeners")] ''KnownListeners

instance MFunctored (KnownListeners o) where
    hoistItem how (KnownListeners li is) =
        KnownListeners (fmap (fmap (hoistItem how)) li) is

-- | Remembers all served methods
newtype ListenersCache (o :: [*]) m a = ListenersCache
    { unwrapListenersMemory :: ReaderT (IORef $ KnownListeners o m) m a
    } deriving (Functor, Applicative, Monad, MonadIO,
              MonadThrow, MonadCatch, MonadMask, MonadBase __,
              MonadTimed, WithNamedLogger)

type instance ThreadId (ListenersCache o m) = ThreadId m

runListenersCacheFor
    :: MonadIO m
    => (Rpc.Port -> Bool) -> ListenersCache o m a -> m a
runListenersCacheFor _klIsSelf (ListenersCache action) = do
    let _klListeners = mempty
    st <- newIORef $ KnownListeners{..}
    runReaderT action st

instance WrappedM (ListenersCache o m) where
    type UnwrappedM (ListenersCache o m) =
        ReaderT (IORef $ KnownListeners o m) m

instance MonadTrans (ListenersCache o) where
    lift action = ListenersCache $ lift action

instance MonadBaseControl b m => MonadBaseControl b (ListenersCache o m) where
    type StM (ListenersCache o m) a = StM m a
    liftBaseWith f = packM $ liftBaseWith $ \runInBase -> f (runInBase . unpackM)
    restoreM = packM . restoreM


instance ( MonadIO m
         , MonadMask m
         , Rpc.RpcOptions o
         , Rpc.HasOption RpcOptionTypeable o
         , MonadRpc o m
         ) =>
         MonadRpc o (ListenersCache o m) where
    send addr@(host, port) (msg :: msg) =
        ListenersCache $ do
            KnownListeners {..} <- readIORef =<< ask
            if host == Rpc.localhost && _klIsSelf port
                then handleLocally _klListeners
                else lift $ Rpc.send addr msg
      where
        handleLocally listeners =
            let mid = Rpc.messageId @msg Proxy
            in case listeners ^? ix port . ix mid of
                Nothing ->
                    throwM $ Rpc.NetworkProblem $
                    sformat
                        ("Method " %build % " not found at port " %shown)
                        mid port
                Just (Rpc.Method (f :: msg' -> _)) -> do
                    Rpc.Evi evi <- pure $ ExtOpt.hasOption @RpcOptionTypeable @o
                    Rpc.Dict <- pure $ evi @msg Proxy
                    Rpc.Dict <- pure $ evi @msg' Proxy
                    case Typeable.eqT @msg @msg' of
                        Just Typeable.Refl -> lift $ f msg
                        Nothing ->
                            error "Cannot pass message to listener, type mismatch"
    serve port methods =
        ListenersCache $ do
            st <- ask
            isSelfPort <- _klIsSelf <$> readIORef st
            let methods' =
                    map
                        (hoistItem (flip runReaderT st . unwrapListenersMemory))
                        methods
            let addMethods =
                    atomicModifyIORef' st $ \s ->
                        (foldl' addMethod s methods', ())
            let removeMethods =
                    atomicModifyIORef' st $ \s ->
                        (foldl' removeMethod s methods', ())
            let forwardServe =
                    Rpc.serve
                        port
                        (map (hoistItem unwrapListenersMemory) methods)
            if isSelfPort port
                then addMethods >> forwardServe `finally` removeMethods
                else forwardServe
      where
        addMethod listeners method =
            listeners & klListeners . at port . non' _Empty .
            at (Rpc.methodMessageId method) %~
            insertIfAbsent method
        insertIfAbsent method =
            \case
                Nothing -> Just method
                Just _ -> error "Method with such message id is already present"
        removeMethod listeners method =
            listeners & klListeners . at port . non' _Empty .
            at (Rpc.methodMessageId method) .~
            Nothing

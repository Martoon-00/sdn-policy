-- | Helpers in properties creation.

module Test.Sdn.Util where

import           Universum

import           Control.Concurrent.Async (async, wait)
import qualified Control.Concurrent.STM   as STM
import           Control.Monad.Random     (Random, evalRand, getRandom)
import           Test.QuickCheck          (Property, property)
import           Test.QuickCheck.Gen      (Gen (..))
import           Test.QuickCheck.Property (failed, reason, succeeded)

import           Sdn.Protocol

-- | First monad is expected to be wrapped out on protocol start,
-- inner monad to be wrapped out after protocol termination.
type ProtocolProperty m = STM AllStates -> m (m (Either Text ()))

type PropertyChecker = AllStates -> Either Text ()

-- | Combines properties into large one.
protocolProperties
    :: MonadIO m
    => TopologyMonitor m
    -> [ProtocolProperty m]
    -> m (Either Text ())
protocolProperties monitor mkProperties = do
    let propertiesMM = sequence mkProperties (readAllStates monitor)
    propertiesM <- sequence propertiesMM
    awaitTermination monitor
    properties <- sequence propertiesM
    return $ sequence_ properties

eitherToProp :: Either Text () -> Property
eitherToProp = property . \case
    Left err -> failed { reason = toString err }
    Right () -> succeeded

arbitraryRandom :: Random a => Gen a
arbitraryRandom = MkGen $ \gc _ -> evalRand getRandom gc

-- | Property which always fails. Useful, when want to watch some test scenario.
justFail :: Monad m => ProtocolProperty m
justFail _ = pure . pure $ Left "¯\\_(ツ)_/¯"

-- | Property checked when protocol is claimed to be completed.
eventually
    :: MonadIO m
    => PropertyChecker -> ProtocolProperty m
eventually checker readState = return $ do
    allStates <- atomically readState
    return $ checker allStates

-- | Property checked every time state of some process changes.
invariant
    :: MonadIO m
    => PropertyChecker -> ProtocolProperty m
invariant checker readState = do
    finished <- liftIO $ newTVarIO False
    checkerThread <-
        liftIO . async . atomically $ do
            allStates <- readState
            case checker allStates of
                Left err -> return (Left err)
                Right () -> fmap Right $ STM.check =<< readTVar finished

    return $ do
        atomically $ writeTVar finished True
        liftIO $ wait checkerThread

fails :: Monad m => ProtocolProperty m -> ProtocolProperty m
fails f readStates = flipEither <<$>> f readStates
  where
    flipEither =
        either (\_ -> Right ()) (\_ -> Left "Property unexpectedly succeeded")

-- | Helpers in properties creation.

module Test.Sdn.Overall.Properties.Util where

import           Universum

import           Control.Concurrent.Async (async, wait)
import qualified Control.Concurrent.STM   as STM
import           Test.QuickCheck          (Property, property)
import           Test.QuickCheck.Property (failed, reason, succeeded)

import           Sdn.Protocol

type PropertyOutcome = Either Text ()

-- | First monad is expected to be wrapped out on protocol start,
-- inner monad to be wrapped out after protocol termination.
type ProtocolProperty pv m =
    STM (AllStates pv) -> m (m (AllStates pv, PropertyOutcome))

type PropertyChecker pv = AllStates pv -> Either Text ()

-- | Combines properties into large one.
protocolProperties
    :: MonadIO m
    => TopologyMonitor pv m
    -> [ProtocolProperty pv m]
    -> m (Maybe (AllStates pv, Text))
protocolProperties monitor mkProperties = do
    putText "Prop 1"
    let propertiesMM = sequence mkProperties (readAllStates monitor)
    putText "Prop 2"
    propertiesM <- sequence propertiesMM
    putText "Prop 3"
    awaitTermination monitor
    putText "Prop 4"
    properties <- sequence propertiesM
    putText "Prop 5"
    return $ head $
        [ (states, err)
        | (states, Left err) <- properties
        ]

eitherToProp :: Either Text () -> Property
eitherToProp = property . \case
    Left err -> failed { reason = toString err }
    Right () -> succeeded

-- | Property which always fails. Useful, when want to watch some test scenario.
justFail :: Monad m => ProtocolProperty pv m
justFail _ = pure . pure . (error "Failed!", ) $ Left "¯\\_(ツ)_/¯"

-- | Property checked when protocol is claimed to be completed.
eventually
    :: MonadIO m
    => PropertyChecker pv -> ProtocolProperty pv m
eventually checker readState = return $ do
    allStates <- atomically readState
    return (allStates, checker allStates)

-- | Property checked every time state of some process changes.
invariant
    :: MonadIO m
    => PropertyChecker pv -> ProtocolProperty pv m
invariant checker readState = do
    finished <- liftIO $ newTVarIO False
    checkerThread <-
        liftIO . async . atomically $ do
            allStates <- readState
            case checker allStates of
                Left err -> return (allStates, Left err)
                Right () -> fmap ((allStates, ) . Right) $
                            STM.check =<< readTVar finished

    return $ do
        atomically $ writeTVar finished True
        liftIO $ wait checkerThread

fails :: Monad m => ProtocolProperty pv m -> ProtocolProperty pv m
fails f readStates = fmap flipEither <<$>> f readStates
  where
    flipEither =
        either (\_ -> Right ()) (\_ -> Left "Property unexpectedly succeeded")

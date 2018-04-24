{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures      #-}

-- | Controller based on Generalized Paxos algorithm.

module Main where

import           Control.Lens               (at, zoom, (.=), (<<.=))
import           Control.TimeWarp.Timed     (for, fork_, ms, runTimedIO, wait)
import qualified Data.Map                   as M
import           Formatting                 (build, formatToString, sformat, shown, (%))
import qualified Network.Data.OF13.Handlers as OF
import qualified Network.Data.OF13.Server   as OF
import qualified Network.Data.OpenFlow      as OF
import           Universum

import           Sdn.Base
import           Sdn.Extra.Util             (atomicModifyIORefS, decompose)
import           Sdn.Protocol.Common

import           Options
import           Sdn.Policy.OpenFlow


main :: IO ()
main = do
    ControllerOptions{..} <- getControllerOptions

    registeredCallbacks <- newIORef mempty
    protocolHandlersVar <- newEmptyMVar

    runTimedIO . fork_ . liftIO $
        runProtocolNode @Configuration protocolOptions curProcessId
            (protocolCallbacks registeredCallbacks)
            (putMVar protocolHandlersVar)

    protocolHandlers <- takeMVar protocolHandlersVar

    -- give some time for server to be brought up
    runTimedIO $ wait (for 20 ms)
    putStrLn @Text "Consensus protocol initiated"

    runPlatform (protocolHandlers, registeredCallbacks) platformOptions curProcessId
  where
    protocolCallbacks :: CallbacksRegister -> ProtocolCallbacks Configuration
    protocolCallbacks rc =
        ProtocolCallbacks
        { protocolOnLearned = \ps -> do
            callbacks <- atomicModifyIORefS rc $ do
                forM ps $ \policyAcceptance -> do
                    let (_, policy) = decompose policyAcceptance
                    mCallback <- at (policyXid policy) <<.= Nothing
                    return $ ($ policyAcceptance) `fmap` mCallback

            sequence_ $ catMaybes $ toList callbacks
        }

type CallbacksRegister = IORef $ M.Map OF.TransactionID (Acceptance Policy -> IO ())

type ProtocolAccess = (ProtocolHandlers Configuration, CallbacksRegister)

runPlatform :: ProtocolAccess -> PlatformOptions -> ProcessId -> IO ()
runPlatform protocolAccess PlatformOptions{..} processId = do
    let port = platformPorts processId
    OF.runServer port onConnect
  where
    onConnect sw = handshake sw >> return (messageHandler protocolAccess sw)

handshake :: OF.Switch -> IO ()
handshake sw = OF.sendToSwitch sw $ OF.CSHello 0

installPolicy
    :: ProtocolAccess
    -> Policy
    -> (AcceptanceType -> IO ())
    -> IO ()
installPolicy (ProtocolHandlers{..}, callbacksRegister) policy onLearned = do
    isNew <- atomicModifyIORefS callbacksRegister . zoom (at (policyXid policy)) $ do
        let callback policyAcceptance = do
                removeCallback
                onLearned (acceptanceType policyAcceptance)
        oldCallback <- get
        case oldCallback of
            Nothing -> put (Just callback) $> True
            Just _  -> pure False

    if isNew
        then protocolMakeProposal policy
        else putStrLn $ sformat ("Do not request for policy "%build%" again") policy
  where
    removeCallback = atomicModifyIORefS callbacksRegister $
        at (policyXid policy) .= Nothing

messageHandler
    :: ProtocolAccess
    -> OF.Switch
    -> Maybe OF.SCMessage
    -> IO ()
messageHandler protocolAccess sw = \case
    Nothing -> putStrLn @Text "Disconnecting\n"
    Just msg -> handleMsg msg
  where
    handleMsg = \case
        OF.PacketIn xid (OF.PacketInfo (Just bufferId) _ inPort reason _ _) -> do
            putStrLn $ formatToString ("Incoming packet #"%shown%" on port "%shown%" with buffer "%shown
                                      %" (reason: "%shown%")")
                       xid inPort bufferId reason

            let actions = OF.flood
            let out = OF.bufferedPacketOut bufferId (Just inPort) actions

            let policy = Policy xid (OF.actionSequenceToList actions)
            installPolicy protocolAccess policy $ \case
                RejectedT -> putStrLn $ "Policy rejected: " <> pretty policy
                AcceptedT -> do
                    putStrLn $ "Policy installed: " <> pretty policy
                    OF.sendToSwitch sw $ OF.PacketOut xid out

        OF.PacketIn _ (OF.PacketInfo Nothing _ _ _ _ _) -> do
            putStrLn @Text "Packet in without buffer specified, skipping"

        other -> OF.handleHandshake onUnhandled sw other

    onUnhandled _ msg = putStrLn $ "Unhandled message from switch: " ++ show msg



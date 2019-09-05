{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures      #-}

-- | Controller based on Generalized Paxos algorithm.

module Main where

import           Control.Lens                 (at, non', (.=), (<<%=), (<<.=), _Empty)
import           Control.TimeWarp.Timed       (currentTime, for, ms, runTimedIO, wait)
import           Data.Coerce                  (coerce)
import qualified Data.Map                     as M
import           Formatting                   (formatToString, shown, (%))
import qualified Network.Data.OF13.Handlers   as OF
import qualified Network.Data.OF13.Server     as OF
import qualified Network.Data.OpenFlow        as OF
import           Universum

import           Sdn.Base
import           Sdn.Extra.Util               (type (%), atomicModifyIORefS, decompose)

import           OptionsController
import           Sdn.Policy.OpenFlow
import           Sdn.Policy.PseudoConflicting
import           Sdn.Protocol.Node

type TL = 200  -- average latency

-- type ConflictsFrac = TimeLimitMillis TL (0 % 1)
-- type ConflictsFrac = TimeLimitMillis TL (1000 % 4419)
-- type ConflictsFrac = TimeLimitMillis TL (1000 % 3200)
-- type ConflictsFrac = TimeLimitMillis TL (1000 % 2209)
type ConflictsFrac = TimeLimitMillis TL (1000 % 1600)

type Policy' = PseudoConflicting ConflictsFrac Policy
type Configuration' = PseudoConflicting ConflictsFrac Configuration

instance PracticalCStruct (PseudoConflicting ConflictsFrac Configuration)


main :: IO ()
main = do
    ControllerOptions{..} <- getControllerOptions

    registeredCallbacks <- newIORef mempty

    protocolHandlers <-
        runProtocolNode
            @Configuration'
            protocolOptions
            curProcessId
            (protocolCallbacks registeredCallbacks)

    -- give some time for server to be brought up
    runTimedIO $ wait (for 20 ms)
    putStrLn @Text "Consensus protocol initiated"

    runPlatform (protocolHandlers, registeredCallbacks) platformOptions curProcessId
  where
    protocolCallbacks :: CallbacksRegister -> ProtocolCallbacks Configuration'
    protocolCallbacks callbacksReg =
        ProtocolCallbacks
        { protocolOnLearned = \policyAccs -> do
            callbacks <- atomicModifyIORefS callbacksReg $ do
                forM policyAccs $ \(fmap unPseudoConflicting -> policyAcceptance) -> do
                    let (_, policy) = decompose policyAcceptance
                    mCallbacks <- at (policyCoord policy) <<.= Nothing
                    let callbacks = fromMaybe [] mCallbacks
                    return $ ($ policyAcceptance) `fmap` callbacks

            sequence_ $ fold $ toList callbacks
        }

type CallbacksRegister = IORef $ M.Map PolicyCoord [Acceptance Policy -> IO ()]

type ProtocolAccess = (ProtocolHandlers Configuration', CallbacksRegister)

runPlatform :: ProtocolAccess -> PlatformOptions -> GeneralProcessId -> IO ()
runPlatform protocolAccess PlatformOptions{..} processId = do
    let port = platformPorts processId
    OF.runServer port onConnect
  where
    onConnect sw = handshake sw >> messageHandler protocolAccess sw

handshake :: OF.Switch -> IO ()
handshake sw = OF.sendToSwitch sw $ OF.CSHello 0

installPolicy
    :: ProtocolAccess
    -> Policy
    -> (AcceptanceType -> IO ())
    -> IO ()
installPolicy (ProtocolHandlers{..}, callbacksRegister) policy onLearned = do
    oldCallbacks <- atomicModifyIORefS callbacksRegister $
        at (policyCoord policy) . non' _Empty <<%= (callback :)

    let isNewPolicy = null oldCallbacks
    when isNewPolicy $
        protocolMakeProposal (PseudoConflicting policy)
  where
    callback policyAcceptance = do
        removeCallback
        onLearned (acceptanceType policyAcceptance)
    removeCallback = atomicModifyIORefS callbacksRegister $
        at (policyCoord policy) .= Nothing

messageHandler
    :: ProtocolAccess
    -> OF.Switch
    -> IO (Maybe OF.SCMessage -> IO ())
messageHandler protocolAccess@(ProtocolHandlers{..}, _) sw = do
    switchState <- newIORef Nothing
    return $ \case
        Nothing -> putStrLn @Text "Disconnecting\n"
        Just msg -> handleMsg switchState msg
  where
    handleMsg switchState = \case
        OF.Features _ features -> do
            writeIORef switchState (Just $ OF.switchID features)
        OF.PacketIn xid (OF.PacketInfo (Just bufferId) _ inPort reason _ _) -> do
            putStrLn $ formatToString ("Incoming packet #"%shown%" on port "%shown%" with buffer "%shown
                                      %" (reason: "%shown%")")
                       xid inPort bufferId reason

            sid <- fromMaybe (error "switch id unknown") <$> readIORef switchState

            let actions = OF.flood
            let out = OF.bufferedPacketOut bufferId (Just inPort) actions

            time <- runTimedIO currentTime

            let policy = Policy (xid, sid)
                                (OF.actionSequenceToList actions)
                                (coerce protocolProcessId)
                                time
            installPolicy protocolAccess policy $ \case
                RejectedT -> do
                    putStrLn $ "Policy rejected: " <> pretty policy
                    OF.sendToSwitch sw $ OF.PacketOut xid out
                AcceptedT -> do
                    putStrLn $ "Policy installed, telling switch: " <> pretty policy
                    OF.sendToSwitch sw $ OF.PacketOut xid out

        OF.PacketIn _ (OF.PacketInfo Nothing _ _ _ _ _) -> do
            putStrLn @Text "Packet in without buffer specified, skipping"

        other -> OF.handleHandshake onUnhandled sw other

    onUnhandled _ msg = putStrLn $ "Unhandled message from switch: " ++ show msg

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types     #-}

module Main where

import           Control.TimeWarp.Logging (logInfo, usingLoggerName)
import           Control.TimeWarp.Rpc     ((:<<) (..), Dict (..), MonadRpc,
                                           runDelaysLayer, runMsgPackUdp, runPureRpc,
                                           withExtendedRpcOptions)
import           Control.TimeWarp.Timed   (MonadTimed, for, sec, wait)
import           Prelude                  (read)
import           System.Random            (split)
import           Universum

import           Options
import           Sdn.Extra                (RpcOptions, dropDesc, genSoundWord, generateM,
                                           runNoErrorReporting, setDropLoggerName)
import           Sdn.Protocol

main :: IO ()
main = do
    -- get options
    options@ProtocolOptions{..} <- getProtocolOptions
    putText ""
    putText $ "Executing with following options:\n" <> pretty options
    putText ""

    -- construct seed in appropriate form to use in protocol
    seed <- case poSeed of
        Just s  -> pure s
        Nothing -> do
            seed <- generateM $ genSoundWord 5
            putText $ "Using " <> show seed <> " as seed\n"
            return seed
    let genesisGen = read $ toString seed
        (gen1, gen2) = split genesisGen

    -- convert settings
    TopologySettingsBox settings <- buildTopologySettings poTopologySettings

    -- environment initialization and protocol launch
    let stuff :: (forall m. (MonadIO m, MonadTimed m, MonadRpc RpcOptions m, MonadCatch m) => m ())
        stuff = runDelaysLayer (dropDesc poDelays) gen1 . runNoErrorReporting . usingLoggerName mempty $ do
            -- disable logging if config says so
            let tuneLogging =
                    if poEnableLogging
                    then identity
                    else setDropLoggerName

            tuneLogging $ do
                wait (for 1 sec)
                logInfo "Starting"

                -- execute consensus
                monitor <- launchPaxos gen2 settings
                awaitTermination monitor

    -- execute in emulation/as-is
    if poQuick
    then runPureRpc $ withExtendedRpcOptions (Evi Dict) stuff
    else runMsgPackUdp $ withExtendedRpcOptions (Evi Dict) stuff

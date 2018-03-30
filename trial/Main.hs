{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types     #-}

module Main where

import           Control.TimeWarp.Logging (logInfo, usingLoggerName)
import           Control.TimeWarp.Rpc     ((:<<) (..), Dict (..), MonadRpc,
                                           runDelaysLayer, runMsgPackRpc, runPureRpc,
                                           withExtendedRpcOptions)
import           Control.TimeWarp.Timed   (MonadTimed, for, sec, wait)
import           Prelude                  (read)
import           System.Random            (split)
import           Universum

import           Options
import           Sdn.Extra                (RpcOptions, dropDesc, genSoundWord, generateM,
                                           runNoErrorReporting)
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

    -- initialize environment
    let stuff :: (forall m. (MonadIO m, MonadTimed m, MonadRpc RpcOptions m, MonadCatch m) => m ())
        stuff = runDelaysLayer (dropDesc poDelays) gen1 . runNoErrorReporting . usingLoggerName mempty $ do
            wait (for 1 sec)
            logInfo "Starting"

            -- execute consensus
            monitor <- launchPaxos gen2 settings
            awaitTermination monitor

    if poQuick
    then runPureRpc $ withExtendedRpcOptions (Evi Dict) stuff
    else runMsgPackRpc $ withExtendedRpcOptions (Evi Dict) stuff

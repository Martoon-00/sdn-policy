module Main where

import           Control.TimeWarp.Logging (logInfo, usingLoggerName)
import           Control.TimeWarp.Rpc     (runMsgPackRpc)
import           Control.TimeWarp.Timed   (for, sec, wait)
import           Universum

import           Options
import           Sdn.Extra                (runNoErrorReporting)
import           Sdn.Protocol

main :: IO ()
main = do
    options@ProtocolOptions{..} <- getProtocolOptions
    let topologySettings = buildTopologySettings poTopologySettings
    putText ""
    putText $ "Executing with following options:\n" <> pretty options
    putText ""

    -- initialize environment
    runMsgPackRpc . runNoErrorReporting . usingLoggerName mempty $ do
        wait (for 1 sec)
        logInfo "Starting"

        -- execute consensus
        case poType of
            ClassicProtocol ->
                launchClassicPaxos topologySettings >>= awaitTermination
            FastProtocol ->
                launchFastPaxos topologySettings >>= awaitTermination


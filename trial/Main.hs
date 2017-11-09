module Main where

import           Control.TimeWarp.Logging (usingLoggerName)
import           Control.TimeWarp.Rpc     (runMsgPackRpc)
import           Data.Default             (def)
import           Universum

import           Sdn.Protocol             (awaitTermination, launchClassicPaxos)

main :: IO ()
main =
    -- initialize environment
    runMsgPackRpc . usingLoggerName mempty $
        -- execute consensus with default topology
        launchClassicPaxos def >>= awaitTermination

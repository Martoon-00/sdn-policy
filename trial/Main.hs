module Main where

import           Control.TimeWarp.Rpc (runMsgPackRpc)
import           Data.Default         (def)
import           Universum

import           Sdn.Protocol         (awaitCompletion, launchClassicPaxos)

main :: IO ()
main = runMsgPackRpc $ launchClassicPaxos def >>= awaitCompletion

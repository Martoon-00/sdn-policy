module Main where

import           Control.TimeWarp.Logging (usingLoggerName)
import           Control.TimeWarp.Rpc     (runMsgPackRpc)
import           Control.TimeWarp.Timed   (interval, sec)
import           Test.QuickCheck
import           Universum

import           Sdn.Base
import           Sdn.Protocol
import           Sdn.Schedule

main :: IO ()
main =
    -- initialize environment
    runMsgPackRpc . usingLoggerName mempty $
        -- execute consensus
        launchClassicPaxos demoTopology >>= awaitTermination

-- | Simple example of network topology.
demoTopology :: TopologySettings
demoTopology = TopologySettings
    { topologyMembers = Members { acceptorsNum = 3, learnersNum = 1 }
    , topologyProposalSchedule =
        generating (GoodPolicy <$> arbitrary)  -- single proposal of policy
    , topologyBallotsSchedule = execute  -- start ballot once
    , topologySeed = RandomSeed
    , topologyLifetime = interval 1 sec
    }

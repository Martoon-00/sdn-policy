module Main where

import           Control.TimeWarp.Logging (usingLoggerName)
import           Control.TimeWarp.Rpc     (runMsgPackRpc)
import           Control.TimeWarp.Timed   (interval, sec)
import           Test.QuickCheck          (arbitrary)
import           Universum

import           Sdn.Base
import           Sdn.Extra
import           Sdn.Protocol
import           Sdn.Schedule

main :: IO ()
main =
    -- initialize environment
    runMsgPackRpc . runNoErrorReporting . usingLoggerName mempty $
        -- execute consensus
        launchFastPaxos demoTopology >>= awaitTermination

-- | Simple example of network topology.
demoTopology :: TopologySettings
demoTopology = TopologySettings
    { topologyMembers = Members { acceptorsNum = 3, learnersNum = 1 }
    , topologyProposalSchedule =
        generate (GoodPolicy <$> arbitrary)  -- single proposal of policy
    , topologyBallotsSchedule = execute  -- start ballot once
    , topologyRecoveryDelay = interval 1 sec
    , topologySeed = RandomSeed
    , topologyLifetime = interval 1 sec
    }

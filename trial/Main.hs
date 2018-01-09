module Main where

import           Control.TimeWarp.Logging (logInfo, usingLoggerName)
import           Control.TimeWarp.Rpc     (runPureRpc)
import           Control.TimeWarp.Timed   (for, sec, wait)
import           Universum

import           Options
import           Sdn.Extra                (dropDesc, runNoErrorReporting)
import           Sdn.Protocol
import qualified Sdn.Schedule             as S

main :: IO ()
main = do
    options@ProtocolOptions{..} <- getProtocolOptions
    let topologySettings = buildTopologySettings poTopologySettings
    putText ""
    putText $ "Executing with following options:\n" <> pretty options
    putText ""

    TopologySettingsBox settings' <- pure topologySettings
    let (gen1, gen2) = S.splitGenSeed (topologySeed settings')
    let settings = settings'{ topologySeed = gen1 }

    -- initialize environment
    gen2' <- S.getGenSeed gen2
    runPureRpc (dropDesc poDelays) gen2' . runNoErrorReporting . usingLoggerName mempty $ do
        wait (for 1 sec)
        logInfo "Starting"

        -- execute consensus
        launchPaxos settings >>= awaitTermination


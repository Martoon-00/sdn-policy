-- | Controller which will be used as real application.

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types     #-}

module Main where

import           Control.TimeWarp.Logging (usingLoggerName)
import           Control.TimeWarp.Rpc     (runMsgPackUdp)
import           Control.TimeWarp.Timed   (interval, minute, sec)
import           System.Random            (mkStdGen)
import           Test.QuickCheck          (arbitrary)
import           Universum

import           Sdn.Base
import           Sdn.Extra                (declareMemStorage, ioRefMemStorage, logInfo,
                                           runNoErrorReporting, setDropLoggerName)
import           Sdn.Protocol
import qualified Sdn.Schedule             as S

main :: IO ()
main = do
    -- logging off
    let runLogging = runNoErrorReporting . usingLoggerName mempty . setDropLoggerName

    -- environment initialization
    runMsgPackUdp . runLogging $ declareMemStorage ioRefMemStorage $ do
        return ()
        logInfo "Starting"

        -- execute consensus
        monitor <- launchPaxos @Fast gen settings
        awaitTermination monitor
  where
    gen = mkStdGen 42

    settings =
        TopologySettings
        { topologyMembers = Members{ acceptorsNum = 3, learnersNum = 1 }
        , topologyProposalSchedule = do
            S.times 1000
            S.generate (GoodPolicy <$> arbitrary)
        , topologyProposerInsistance = \_ -> S.repeating 3 (interval 1 sec)
        , topologyBallotsSchedule = S.periodic (interval 1 sec)
        , topologyLifetime = interval 1 minute
        , topologyCustomSettings =
            FastTopologySettingsPart
            { topologyRecoveryDelay = interval 1 sec
            }
        }


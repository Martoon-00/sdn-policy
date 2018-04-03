-- | Controller which will be used as real application.

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types     #-}

module Main where

import           Control.Lens             ((+=))
import           Control.TimeWarp.Logging (setLoggerName, usingLoggerName)
import           Control.TimeWarp.Rpc     (MsgPackUdpOptions (..), runMsgPackUdpOpts)
import           Control.TimeWarp.Timed   (for, fork_, interval, minute, sec, wait)
import           Data.Default             (def)
import           System.Random            (mkStdGen)
import           Test.QuickCheck          (arbitrary, resize)
import           Universum

import           Sdn.Base
import           Sdn.Extra                (atomicModifyIORefS, declareMemStorage,
                                           ioRefMemStorage, logInfo, runNoErrorReporting,
                                           setDropLoggerName)
import           Sdn.Protocol
import qualified Sdn.Protocol.Classic     as Classic
import           Sdn.Protocol.Common      (LearningCallback (..))
import qualified Sdn.Protocol.Fast        as Fast
import qualified Sdn.Schedule             as S

main :: IO ()
main = do
    -- logging off
    let runLogging = runNoErrorReporting . usingLoggerName mempty
        networkOptions = def{ udpMessageSizeLimit = 15000 }

    -- environment initialization
    runMsgPackUdpOpts networkOptions . runLogging $ declareMemStorage ioRefMemStorage $ setDropLoggerName $ do
        return ()
        logInfo "Starting"

        learnedCounter <- newIORef 0

        let callback = LearningCallback $ \policies -> do
                atomicModifyIORefS learnedCounter (identity += length policies)
                setLoggerName mempty $ logInfo $ "Mem" -- <> show policies

        let topologyActions =
              (versionTopologyActions @Fast $ topologyCustomSettings settings)
                { learnerListeners =
                    [ listener @Learner $ Classic.learn callback
                    , listener @Learner $ Fast.learn callback
                    ]
                }

        fork_ . forever $ do
            learned <- readIORef learnedCounter
            logInfo $ "Learned " <> pretty learned
            wait (for 1 sec)

        -- execute consensus
        monitor <- launchPaxosWith topologyActions gen settings
        awaitTermination monitor
  where
    gen = mkStdGen 42

    settings =
        TopologySettings
        { topologyMembers = Members{ acceptorsNum = 3, learnersNum = 1 }
        , topologyProposalSchedule = do
            S.times 1000
            S.generate (GoodPolicy <$> resize 1000000 arbitrary)
        , topologyProposerInsistance = \_ -> S.repeating 3 (interval 1 sec)
        , topologyBallotsSchedule = S.periodic (interval 1 sec)
        , topologyLifetime = interval 1 minute
        , topologyCustomSettings =
            FastTopologySettingsPart
            { topologyRecoveryDelay = interval 1 sec
            }
        }


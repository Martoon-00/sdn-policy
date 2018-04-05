-- | Controller which will be used as real application.

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types     #-}

module Main where

import           Control.Lens             ((+=))
import           Control.TimeWarp.Logging (usingLoggerName)
import           Control.TimeWarp.Rpc     (MsgPackUdpOptions (..), runMsgPackUdpOpts)
import           Control.TimeWarp.Timed   (for, fork_, interval, minute, sec, wait)
import           Data.Default             (def)
import           System.Random            (mkStdGen)
import           Universum

import           Sdn.Base
import           Sdn.Extra                (atomicModifyIORefS, declareMemStorage,
                                           declareMonadicMark, ioRefMemStorage, logInfo,
                                           runNoErrorReporting)
import           Sdn.Policy.Fake
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
    runMsgPackUdpOpts networkOptions . runLogging $ declareMemStorage ioRefMemStorage $ declareMonadicMark @(CStructType Configuration) $ do
        logInfo "Starting"

        learnedCounter <- newIORef 0

        let callback = LearningCallback $ \policies -> do
                atomicModifyIORefS learnedCounter (identity += length policies)

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

    settings :: TopologySettings Fast Configuration
    settings =
        TopologySettings
        { topologyMembers = Members{ acceptorsNum = 3, learnersNum = 1 }
        , topologyProposalSchedule = do
            S.wrapped $ \push ->
                forM_ [1..3000 :: Int] $ \p ->
                    push (GoodPolicy $ fromIntegral p)
        , topologyProposerInsistance = \_ -> mempty  -- S.repeating 3 (interval 1 sec)
        , topologyBallotsSchedule = S.periodic (interval 1 sec)
        , topologyLifetime = interval 1 minute
        , topologyCustomSettings =
            FastTopologySettingsPart
            { topologyRecoveryDelay = interval 1 sec
            }
        }


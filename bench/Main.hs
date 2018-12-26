-- | Controller which will be used as real application.

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types     #-}

module Main where

import           Control.Lens             ((+=))
import           Control.TimeWarp.Logging (usingLoggerName)
import           Control.TimeWarp.Rpc     (Dict (..), MsgPackUdpOptions (..), pickEvi,
                                           runMsgPackUdpOpts, withExtendedRpcOptions)
import           Control.TimeWarp.Timed   (for, fork_, interval, ms, sec, wait)
import           Data.Default             (def)
import           System.Random            (mkStdGen)
import           Universum

import           Sdn.Base
import           Sdn.Extra                (atomicModifyIORefS, declareMemStorage,
                                           declareMonadicMark, ioRefMemStorageUnsafe, logInfo,
                                           runNoErrorReporting, setDropLoggerName)
import qualified Sdn.Extra.Schedule       as S
import           Sdn.Policy.Fake
import           Sdn.Protocol
import           Sdn.Protocol.Common      (BatchingSettings (..), LearningCallback (..))

main :: IO ()
main = do
    -- logging off
    let runLogging = runNoErrorReporting . usingLoggerName mempty
        networkOptions = def{ udpMessageSizeLimit = 15000 }

    -- environment initialization
    runMsgPackUdpOpts networkOptions $
      withExtendedRpcOptions (pickEvi Dict) $
      runLogging $
      declareMemStorage ioRefMemStorageUnsafe $
      declareMonadicMark @(CStructType Configuration) $
      setDropLoggerName $ do
        logInfo "Starting"

        learnedCounter <- newIORef 0

        let callback = LearningCallback $ \policies -> do
                atomicModifyIORefS learnedCounter (identity += length policies)

        let listenersSettings =
                def{ listenersLearningCallback = callback }
        let topologyActions =
              (versionTopologyActions @Fast settings)
                { topologyListeners = versionProtocolListeners @Fast listenersSettings
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
        , topologyBallotsSchedule = S.periodic (interval 10 sec)
        , topologyProposalBatchSettings = Just proposalBatchSettings
        , topologyLifetime = interval 10 sec
        , topologyCustomSettings = FastTopologySettingsPart{}
        }

    proposalBatchSettings =
        BatchingSettings
        { batchMaxSize = 10
        , batchMaxJitter = interval 10 ms
        }

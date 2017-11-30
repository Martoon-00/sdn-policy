{-# LANGUAGE Rank2Types #-}

-- | Test launcher of protocol.

module Test.Sdn.Overall.Launcher
    ( TestLaunchParams (..)
    , testLaunch
    ) where

import           Universum

import           Control.TimeWarp.Logging    (setLoggerName, usingLoggerName)
import           Control.TimeWarp.Rpc        (runPureRpc)
import qualified Control.TimeWarp.Rpc        as D
import           Control.TimeWarp.Timed      (runTimedT)
import           Data.Default
import           Formatting                  (build, sformat, stext, (%))
import           System.Random               (mkStdGen, split)
import           Test.QuickCheck             (Blind (..), Property, forAll)
import           Test.QuickCheck.Gen         (chooseAny)
import           Test.QuickCheck.Monadic     (monadicIO, stop)
import           Test.QuickCheck.Property    (failed, reason, succeeded)

import           Sdn.Extra
import           Sdn.Protocol
import qualified Sdn.Schedule                as S
import           Test.Sdn.Overall.Properties


data TestLaunchParams = TestLaunchParams
    { testLauncher   :: TopologyLauncher
    , testSettings   :: TopologySettings
    , testDelays     :: D.Delays
    , testProperties :: forall m. MonadIO m => [ProtocolProperty m]
    }

instance Default TestLaunchParams where
    def = TestLaunchParams
        { testLauncher = launchClassicPaxos
          -- ^ use Classic Paxos protocol
        , testSettings = def
          -- ^ default topology settings allow to execute
          -- 1 ballot with 1 policy proposed
        , testDelays = D.steady
          -- ^ no message delays
        , testProperties = basicProperties
          -- ^ set of reasonable properties for any good consensus launch
        }

testLaunch :: TestLaunchParams -> Property
testLaunch TestLaunchParams{..} =
    forAll (Blind <$> chooseAny) $ \(Blind seed) -> do
        let (gen1, gen2) =
                split (mkStdGen seed)
            launch :: MonadTopology m => m (TopologyMonitor m)
            launch =
                testLauncher
                testSettings { topologySeed = S.FixedSeed gen2 }
            runEmulation =
                runTimedT .
                runPureRpc testDelays gen1 .
                usingLoggerName mempty
            failProp err = do
                lift . runEmulation . runNoErrorReporting . setLoggerName mempty $
                    awaitTermination =<< launch
                stop failed{ reason = toString err }

        monadicIO $ do
            -- launch silently
            (errors, propErrors) <- lift . runEmulation . runErrorReporting $ do
                monitor <- setDropLoggerName launch
                protocolProperties monitor testProperties

            -- check errors log
            unless (null errors) $
                failProp $
                    "Protocol errors:\n" <>
                    mconcat (intersperse "\n" errors)

            -- check properties
            whenJust propErrors $ \(states, err) ->
                failProp $ sformat (stext%"\nFor states: "%build) err states

            stop succeeded


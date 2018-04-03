{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Test launcher of protocol.

module Test.Sdn.Overall.Launcher
    ( TestLaunchParams (..)
    , testLaunch
    ) where

import           Universum

import           Control.TimeWarp.Logging    (usingLoggerName)
import           Control.TimeWarp.Rpc        ((:<<) (Evi), Dict (..), runDelaysLayer,
                                              runPureRpc, withExtendedRpcOptions)
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
import           Test.Sdn.Overall.Properties


data TestLaunchParams pv = TestLaunchParams
    { testSettings   :: TopologySettings pv
    , testDelays     :: D.Delays
    , testProperties :: forall m. MonadIO m => [ProtocolProperty pv m]
    , testStub       :: Proxy pv
    }

instance Default (CustomTopologySettings pv) =>
         Default (TestLaunchParams pv) where
    def =
        TestLaunchParams
        { testSettings = def
            -- ^ default topology settings allow to execute
            -- 1 ballot with 1 policy proposed
        , testDelays = D.steady
            -- ^ no message delays
        , testProperties = basicProperties
            -- ^ set of reasonable properties for any good consensus launch
        , testStub = Proxy
            -- ^ Just for convenience of 'def' usage
        }

testLaunch
    :: forall pv.
       HasVersionTopologyActions pv
    => TestLaunchParams pv -> Property
testLaunch TestLaunchParams{..} =
    forAll (Blind <$> chooseAny) $ \(Blind seed) -> do
        let (gen1, gen2) =
                split (mkStdGen seed)
            launch :: MonadTopology m => m (TopologyMonitor pv m)
            launch =
                launchPaxos gen2 testSettings
            runMemStorage = declareMemStorage stmMemStorage
            failProp err = do
                lift $
                    runTimedT .
                    runPureRpc .
                    withExtendedRpcOptions (Evi Dict) .
                    runDelaysLayer testDelays gen1 .
                    runNoErrorReporting .
                    usingLoggerName mempty $
                    runMemStorage $
                        awaitTermination =<< launch
                stop failed{ reason = toString err }

        monadicIO $ do
            -- launch silently
            (errors, propErrors) <- lift $
                runTimedT $
                runPureRpc $
                withExtendedRpcOptions (Evi Dict) $
                runDelaysLayer testDelays gen1 $
                runErrorReporting $
                usingLoggerName mempty $
                runMemStorage $ do
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


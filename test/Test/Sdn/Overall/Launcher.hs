{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Test launcher of protocol.

module Test.Sdn.Overall.Launcher
    ( TestLaunchParams (..)
    , testPropertiesL

    , testLaunch

    , defTopologySettings
    ) where

import           Universum

import           Control.Lens                (makeLensesFor)
import           Control.TimeWarp.Logging    (usingLoggerName)
import           Control.TimeWarp.Rpc        ((:<<) (Evi), Dict (..), runDelaysLayer,
                                              runPureRpcExt, withExtendedRpcOptions)
import qualified Control.TimeWarp.Rpc        as D
import           Data.Default
import           Formatting                  (build, sformat, stext, (%))
import           System.Random               (mkStdGen, split)
import           Test.QuickCheck             (Blind (..), Property, forAll)
import           Test.QuickCheck.Gen         (chooseAny)
import           Test.QuickCheck.Monadic     (monadicIO, stop)
import           Test.QuickCheck.Property    (failed, reason, succeeded)

import           Sdn.Base
import           Sdn.Extra.Logging
import           Sdn.Extra.MemStorage
import           Sdn.Extra.Util              (declareMonadicMark, emulationOptions)
import           Sdn.Policy.Fake
import           Sdn.Protocol
import           Test.Sdn.Overall.Properties


data TestLaunchParams pv = TestLaunchParams
    { testSettings   :: TopologySettings pv Configuration
    , testDelays     :: D.Delays
    , testProperties :: forall m. (MonadIO m, DeclaredCStruct m ~ Configuration)
                     => [ProtocolProperty pv m]
    , testStub       :: Proxy pv
    }

makeLensesFor
    [ ("testProperties", "testPropertiesL")
    ] ''TestLaunchParams

instance ( Default (CustomTopologySettings pv)
         ) =>
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
            launch
                :: (MonadTopology m, DeclaredCStruct m ~ Configuration)
                => m (TopologyMonitor pv m)
            launch =
                launchPaxos gen2 testSettings
            runMemStorage = declareMemStorage stmMemStorage
            failProp err = do
                lift $
                    runPureRpcExt emulationOptions .
                    withExtendedRpcOptions (Evi Dict) .
                    runDelaysLayer testDelays gen1 .
                    runNoErrorReporting .
                    usingLoggerName mempty $
                    runMemStorage $
                    declareMonadicMark @(CStructType Configuration) $
                        awaitTermination =<< launch
                stop failed{ reason = toString err }

        monadicIO $ do
            -- launch silently
            (errors, propErrors) <- lift $
                runPureRpcExt emulationOptions $
                withExtendedRpcOptions (Evi Dict) $
                runDelaysLayer testDelays gen1 $
                runErrorReporting $
                usingLoggerName mempty $
                runMemStorage $
                declareMonadicMark @(CStructType Configuration) $ do
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

-- | Default for 'TopologySettings' restricted to use 'Configuration' as cstruct.
defTopologySettings
    :: Default (CustomTopologySettings pv)
    => TopologySettings pv Configuration
defTopologySettings = def

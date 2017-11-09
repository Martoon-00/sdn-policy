{-# LANGUAGE Rank2Types #-}

-- | Tests for classic paxos.

module Test.Sdn.ClassicSpec
    ( spec
    ) where

import           Universum

import           Control.TimeWarp.Rpc    (runPureRpc)
import qualified Control.TimeWarp.Rpc    as D
import           Control.TimeWarp.Timed  (runTimedT)
import           Data.Default
import           System.Random           (mkStdGen, split)
import           Test.Hspec              (Spec, describe)
import           Test.Hspec.QuickCheck   (prop)
import           Test.QuickCheck         (Property, arbitrary, forAll)
import           Test.QuickCheck.Monadic (monadicIO, stop)

import           Sdn.Extra
import qualified Sdn.ProposalStrategy    as PS
import           Sdn.Protocol
import           Test.Sdn.Properties
import           Test.Sdn.Util

spec :: Spec
spec = describe "classic" $ do
    prop "simple" simple

data TestLaunchParams = TestLaunchParams
    { testLauncher   :: TopologyLauncher
    , testSettings   :: TopologySettings
    , testDelays     :: D.Delays
    , testProperties :: forall m. MonadIO m => [ProtocolProperty m]
    }

instance Default TestLaunchParams where
    def = TestLaunchParams
        { testLauncher = launchClassicPaxos
        , testSettings = def
        , testDelays = D.steady
        , testProperties = basicProperties
        }

testLaunch :: TestLaunchParams -> Property
testLaunch TestLaunchParams{..} =
    forAll arbitrary $ \seed -> do
        let (gen1, gen2) = split (mkStdGen seed)
        ioToProperty . runTimedT . runPureRpc testDelays gen1 . setDropLoggerName $ do
            monitor <- testLauncher testSettings{ topologyProposalSeed = PS.FixedSeed gen2 }
            protocolProperties monitor testProperties
  where
    ioToProperty :: IO Property -> Property
    ioToProperty propM = monadicIO $ stop =<< lift propM


simple :: Property
simple = testLaunch def

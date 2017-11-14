{-# LANGUAGE Rank2Types #-}

-- | Tests for classic paxos.

module Test.Sdn.ClassicSpec
    ( spec
    ) where

import           Universum

import           Control.TimeWarp.Logging (setLoggerName, usingLoggerName)
import           Control.TimeWarp.Rpc     (runPureRpc)
import qualified Control.TimeWarp.Rpc     as D
import           Control.TimeWarp.Timed   (Millisecond, Second, hour, interval, runTimedT,
                                           sec)
import           Data.Default
import           Formatting               (build, sformat, stext, (%))
import           System.Random            (mkStdGen, split)
import           Test.Hspec               (Spec, describe)
import           Test.Hspec.QuickCheck    (prop)
import           Test.QuickCheck          (Blind (..), Positive (..), Property,
                                           Small (..), arbitrary, forAll, oneof, (==>))
import           Test.QuickCheck.Monadic  (monadicIO, stop)
import           Test.QuickCheck.Property (failed, reason, succeeded)

import           Sdn.Base
import           Sdn.Extra
import           Sdn.Protocol
import qualified Sdn.Schedule             as S
import           Test.Sdn.Properties
import           Test.Sdn.Util

spec :: Spec
spec = do

    -- artifical scenarious which check whether protocol at least slightly works
    describe "primitive cases" $ do

        prop "simple" $
            -- launch with default test settings
            -- see @instance Default TestLaunchParams@ below for their definition
            testLaunch def

        prop "acceptor unavailable" $
            testLaunch def
            { testDelays =
                D.forAddress (processAddress (Acceptor 1))
                    D.blackout
            }

        prop "too many acceptors unavailable" $
            testLaunch def
            { testDelays =
                D.forAddressesList (processAddress . Acceptor <$> [1, 2])
                    D.blackout
            , testProperties =
                [ fails (eventually proposedPoliciesWereLearned)
                ]
            }

        prop "good and bad policies" $
            -- TODO: optimize algorithm and get rid of 'Small'
            \(Small (n :: Word)) ->

            testLaunch def
            { testSettings = def
                { topologyProposalSchedule =
                    S.times n $
                    S.generate . oneof $
                        [ GoodPolicy <$> arbitrary
                        , BadPolicy <$> arbitrary
                        ]
                }
            }

        prop "all conflicting policies" $
            \(Positive (Small n)) ->

            testLaunch def
            { testSettings = def
                { topologyProposalSchedule =
                    S.times n $
                    S.generate (BadPolicy <$> arbitrary)
                }
            , testProperties =
                [ invariant learnedPoliciesWereProposed
                , eventually learnersAgree
                , eventually $ numberOfLearnedPolicies _Accepted (== 1)
                ]
            }


        prop "delays" $
            testLaunch def
            { testDelays = D.uniform (0, 1 :: Second)
            , testSettings = def
                { topologyBallotsSchedule = S.delayed (interval 2 sec) S.execute
                }
            }


    -- bunch of complex scenarious involving introduction of many policies
    describe "real life cases" $

       prop "no conflicts" $
       \(Positive (Small proposalsNum)) ->
       \(Positive (Small balDelay)) ->
       proposalsNum > balDelay ==>

            testLaunch def
            { testSettings = def
                { topologyProposalSchedule =
                    S.repeating proposalsNum (interval 1 sec) $
                    S.generate (GoodPolicy <$> arbitrary)
                , topologyBallotsSchedule = mconcat
                    [ S.repeating
                        (proposalsNum `div` balDelay)
                        (interval (fromIntegral balDelay) sec)
                        S.execute
                    , finalBallot
                    ]
                }
            , testDelays = D.uniform (0, 50 :: Millisecond)
            }

  where
    -- one final ballot for all proposals which weren't in made in time
    finalBallot :: MonadTopology m => S.Schedule m ()
    finalBallot = S.delayed (interval 1 hour) S.execute

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
    forAll (Blind <$> arbitraryRandom) $ \(Blind seed) -> do
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

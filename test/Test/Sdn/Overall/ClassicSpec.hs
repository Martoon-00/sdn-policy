-- | Tests for classic paxos.

module Test.Sdn.Overall.ClassicSpec
    ( spec
    ) where

import           Universum

import qualified Control.TimeWarp.Rpc        as D
import           Control.TimeWarp.Timed      (Millisecond, Second, hour, interval, sec)
import           Data.Default
import           Test.Hspec                  (Spec, describe, pending)
import           Test.Hspec.QuickCheck       (prop)
import           Test.QuickCheck             (Positive (..), Small (..), arbitrary, oneof,
                                              (==>))

import           Sdn.Base
import           Sdn.Protocol
import qualified Sdn.Schedule                as S
import           Test.Sdn.Overall.Launcher
import           Test.Sdn.Overall.Properties

spec :: Spec
spec = do

    -- artifical scenarious which check whether protocol at least slightly works
    describe "primitive cases" $ do

        prop "simple" $
            -- launch with default test settings
            -- see @instance Default TestLaunchParams@ below for their definition
            testLaunch @Classic def

        prop "acceptor unavailable" $
            testLaunch @Classic def
            { testDelays =
                D.forAddress (processAddress (Acceptor 1))
                    D.blackout
            }

        prop "too many acceptors unavailable" $
            testLaunch @Classic def
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

            testLaunch @Classic def
            { testSettings = def
                { topologyProposalSchedule = do
                    S.times n
                    S.generate . oneof $
                        [ GoodPolicy <$> arbitrary
                        , BadPolicy <$> arbitrary
                        ]
                }
            }

        prop "all conflicting policies" $
            \(Positive (Small n)) ->

            testLaunch @Classic def
            { testSettings = def
                { topologyProposalSchedule = do
                    S.times n
                    S.generate (BadPolicy <$> arbitrary)
                }
            , testProperties =
                [ invariant learnedPoliciesWereProposed
                , eventually learnersAgree
                , eventually $ numberOfLearnedPolicies _Accepted (== 1)
                ]
            }


        prop "delays" $
            testLaunch @Classic def
            { testDelays = D.uniform (0, 1 :: Second)
            , testSettings = def
                { topologyBallotsSchedule = S.delayed (interval 2 sec)
                }
            }

        prop "temporaly no quorum of acceptors is accessible" $
            pending

        prop "highly interleaving ballots" $
            -- this may be problematic with current implementation
            -- because values from old ballots won't be leart
            pending

    -- bunch of complex scenarious involving introduction of many policies
    describe "real life cases" $

       prop "no conflicts" $
       \(Positive (Small proposalsNum)) ->
       \(Positive (Small balDelay)) ->
       proposalsNum > balDelay ==>

            testLaunch @Classic def
            { testSettings = def
                { topologyProposalSchedule = do
                    S.repeating proposalsNum (interval 1 sec)
                    S.generate (GoodPolicy <$> arbitrary)
                , topologyBallotsSchedule = mconcat
                    [ S.repeating
                        (proposalsNum `div` balDelay)
                        (interval (fromIntegral balDelay) sec)
                    , finalBallot
                    ]
                }
            , testDelays = D.uniform (0, 50 :: Millisecond)
            }

  where
    -- one final ballot for all proposals which weren't made in time
    finalBallot :: MonadTopology m => S.Schedule m ()
    finalBallot = S.delayed (interval 1 hour)

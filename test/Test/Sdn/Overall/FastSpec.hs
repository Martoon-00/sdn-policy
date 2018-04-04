-- | Special properties for fast version of algorithm.

module Test.Sdn.Overall.FastSpec
    ( spec
    ) where

import           Universum

import qualified Control.TimeWarp.Rpc        as D
import           Data.Default
import           Test.Hspec                  (Spec, describe)
import           Test.Hspec.QuickCheck       (prop)
import           Test.QuickCheck             (Small (..), arbitrary, forAll, suchThat)

import           Sdn.Base
import           Sdn.Policy.Fake
import           Sdn.Protocol
import qualified Sdn.Schedule                as S
import           Test.Sdn.Overall.Launcher
import           Test.Sdn.Overall.Properties

spec :: Spec
spec = do
    let testLaunchF = testLaunch @Fast

    describe "no recovery" $ do
        prop "normal condition" $
            testLaunchF $ def
            { testProperties =
                  eventually (recoveryWasUsed False)
                : basicProperties
            }

        prop "acceptor unavailable" $
            testLaunchF def
            { testSettings = def
                { topologyMembers = def { acceptorsNum = 5 }
                }
            , testDelays =
                D.forAddress (processAddress (Acceptor 1)) D.blackout
            , testProperties =
                eventually (recoveryWasUsed False)
              : basicProperties
            }

    describe "recovery" $ do
        prop "1 ballot, many conflicting policies" $
            forAll (arbitrary `suchThat` (>= 5)) $
                \(Small n :: Small Word) ->
            testLaunchF def
            { testSettings = defTopologySettings
                { topologyProposalSchedule = do
                      S.times n
                      S.generate (BadPolicy <$> arbitrary)
                }
            , testProperties =
                -- eventually (recoveryWasUsed True) :  -- dunno how to force recovery
                basicProperties
            }


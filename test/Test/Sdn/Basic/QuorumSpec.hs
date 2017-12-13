{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Tests for various quorum families used.

module Test.Sdn.Basic.QuorumSpec
    ( spec
    ) where

import           Test.Hspec            (Spec, describe, shouldBe, shouldSatisfy)
import           Test.Hspec.Core.Spec  (SpecWith)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (arbitrary, forAll, suchThat)
import           Universum

import           Sdn.Base

spec :: Spec
spec = do
    describe "simple majority quorum" $ do
        describe "QuorumFamilty" $ do
            describe "classic majority" $ do
                checkQuorum @ClassicMajorityQuorum 6 4
                checkQuorum @ClassicMajorityQuorum 7 4
                checkQuorum @ClassicMajorityQuorum 8 5

            describe "fast majority" $ do
                checkQuorum @FastMajorityQuorum 7 6
                checkQuorum @FastMajorityQuorum 8 7
                checkQuorum @FastMajorityQuorum 24 19
                checkQuorum @FastMajorityQuorum 25 19

        describe "QuorumIntersectionFamily" $ do
            describe "fast majority" $ do
                checkQuorumIntersection 4 4 4
                checkQuorumIntersection 8 6 5
                checkQuorumIntersection 8 8 7
                checkQuorumIntersection 9 6 4
                checkQuorumIntersection 9 7 5

checkQuorum :: forall qf. QuorumFamily qf => Int -> Int -> SpecWith ()
checkQuorum acceptorsNum threashold =
    withMembers Members{..} $
    describe (show threashold <> "/" <> show acceptorsNum) $ do
        prop "isQuorum" $ \votes ->
            isQuorum @qf @() votes `shouldBe` (length votes >= threashold)

        prop "isMinQuorum" $ \votes ->
            isMinQuorum @qf @() votes `shouldBe` (length votes == threashold)
  where
    learnersNum = 1
{-# NOINLINE checkQuorum #-}

checkQuorumIntersection
    :: Int -> Int -> Int -> SpecWith ()
checkQuorumIntersection acceptorsNum heardNum threashold =
    withMembers Members{..} $
    describe (show threashold <> "/" <> show heardNum <> "/" <> show acceptorsNum) $ do
        prop "isIntersectionWithQuorum" $
            forAll (arbitrary `suchThat` \x -> length x == heardNum) $
                \q ->
            forAll arbitrary $
                \v ->
            do
                isSubIntersectionWithQuorum @FastMajorityQuorum @() @() q v
                    `shouldBe` (length v >= threashold)

                when (length v >= threashold && isQuorum q) $
                    coerceVotes @ClassicMajorityQuorum v `shouldSatisfy` isQuorum
  where
    learnersNum = 1
{-# NOINLINE checkQuorumIntersection #-}

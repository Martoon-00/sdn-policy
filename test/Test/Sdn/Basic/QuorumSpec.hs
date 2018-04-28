{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Tests for various quorum families used.

module Test.Sdn.Basic.QuorumSpec
    ( spec
    ) where

import           Test.Hspec            (Spec, describe, shouldBe)
import           Test.Hspec.Core.Spec  (SpecWith)
import           Test.Hspec.QuickCheck (prop)
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


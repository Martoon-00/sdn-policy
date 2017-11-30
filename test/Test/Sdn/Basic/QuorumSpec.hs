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
        describe "classic majority" $ do
            checkQuorum @ClassicMajorityQuorum 6 4
            checkQuorum @ClassicMajorityQuorum 7 4
            checkQuorum @ClassicMajorityQuorum 8 5

        describe "classic majority" $ do
            checkQuorum @FastMajorityQuorum 7 6
            checkQuorum @FastMajorityQuorum 8 7
            checkQuorum @FastMajorityQuorum 24 19
            checkQuorum @FastMajorityQuorum 25 19

checkQuorum :: forall qf. QuorumFamily qf => Int -> Int -> SpecWith ()
checkQuorum acceptors threashold =
    describe (show threashold <> "/" <> show acceptors) $ do
        prop "isQuorum" $ \votes ->
            let members = Members { acceptorsNum = acceptors, learnersNum = 1 }
            in  isQuorum @qf @() members votes `shouldBe` (length votes >= threashold)

        prop "isMinQuorum" $ \votes ->
            let members = Members { acceptorsNum = acceptors, learnersNum = 1 }
            in  isMinQuorum @qf @() members votes `shouldBe` (length votes == threashold)


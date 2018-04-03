{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE Rank2Types          #-}

-- | Tests for various quorum families used.

module Test.Sdn.Basic.CStructSpec
    ( spec
    ) where

import           Data.Default          (def)
import           GHC.Exts              (fromList)
import           Test.Hspec            (Spec, SpecWith, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Gen, arbitrary, elements, forAll, listOf1, resize,
                                        sublistOf, (===))
import           Universum

import           Sdn.Base
import           Sdn.Extra
import           Sdn.Policy.Fake

spec :: Spec
spec = do
    describe "combination" $ do
        describe "examples" $ do
            prop "good policies" $
                withMembers def{ acceptorsNum = 3 } $
                    combination @Configuration @ClassicMajorityQuorum
                    [ (AcceptorId 1, mkNeatPolicies [1])
                    , (AcceptorId 2, mkNeatPolicies [1, 2])
                    ]
                    === Right [Accepted $ GoodPolicy 1]

        compareCombinations @ClassicMajorityQuorum combination combinationDefault
        compareCombinations @FastMajorityQuorum combination combinationDefault

    describe "intersecting combination"$ do
        describe "examples" $ do
            prop "good policies" $
                withMembers def{ acceptorsNum = 5 } $
                    intersectingCombination @Configuration @FastMajorityQuorum
                    (fromList $ [1..5] <&> \i -> (AcceptorId i, mkNeatPolicies [i..5]))
                    === Right (mkNeatPolicies [4..5])

        compareCombinations @FastMajorityQuorum intersectingCombination intersectingCombinationDefault

  where
    mkNeatPolicies = fromList . map (Accepted . GoodPolicy . fromIntegral)


type CombinationFun qf = Votes qf Configuration -> Either Text Configuration

compareCombinations
    :: forall qf.
       (HasMembers => CombinationFun qf)
    -> (HasMembers => CombinationFun qf)
    -> SpecWith ()
compareCombinations combFun1 combFun2 =
    prop "policy specific impl. vs straightforward impl." $
    forAll (resize 15 arbitrary) $ \members ->
    withMembers members $
        forAll (resize 5 $ listOf1 $ arbitrary @Policy) $
            \availablePolicies ->
        forAll (genVotesFromPolicies availablePolicies) $
            \(votes :: Votes qf Configuration) ->
        combFun1 votes === combFun2 votes
  where
    genVotesFromPolicies :: HasMembers => [Policy] -> Gen (Votes qf Configuration)
    genVotesFromPolicies policies = do
        policiesAcc <- forM policies $
                       \policy -> elements [Accepted policy, Rejected policy]
        genVotes . genJust $ mkConfig <$> sublistOf policiesAcc



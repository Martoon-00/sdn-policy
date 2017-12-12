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
import           Test.QuickCheck       (arbitrary, forAll, listOf1, resize, sublistOf,
                                        (===))
import           Universum

import           Sdn.Base
import           Sdn.Extra

spec :: Spec
spec = do
    describe "combination" $ do
        describe "examples" $ do
            prop "good policies" $
                withMembers def{ acceptorsNum = 3 } $
                    combination @Configuration @_ @ClassicMajorityQuorum
                    [ (AcceptorId 1, mkNeatPolicies [1])
                    , (AcceptorId 2, mkNeatPolicies [1, 2])
                    ]
                    === Right [Accepted $ GoodPolicy "1"]

        compareCombinations @ClassicMajorityQuorum combination combinationDefault
        compareCombinations @FastMajorityQuorum combination combinationDefault

    describe "intersecting combination"$ do
        describe "examples" $ do
            prop "good policies" $
                withMembers def{ acceptorsNum = 5 } $
                    intersectingCombination @Configuration @_ @FastMajorityQuorum
                    (fromList $ [1..5] <&> \i -> (AcceptorId i, mkNeatPolicies [i..5]))
                    === Right (mkNeatPolicies [4..5])

        compareCombinations @FastMajorityQuorum intersectingCombination intersectingCombinationDefault

  where
    mkNeatPolicies = fromList . map (Accepted . GoodPolicy . show)


type CombinationFun qf = Votes qf Configuration -> Either Text Configuration

compareCombinations
    :: forall qf.
       (HasMembers => CombinationFun qf)
    -> (HasMembers => CombinationFun qf)
    -> SpecWith ()
compareCombinations combFun combFunDefault =
    prop "policy specific impl. vs straightforward impl." $
    forAll (resize 30 arbitrary) $ \members ->
    withMembers members $
        forAll (resize 5 $ listOf1 arbitrary) $
            \availablePolicies ->
        forAll (genVotes . genJust $ mkConfig <$> sublistOf availablePolicies) $
            \(votes :: Votes qf Configuration) ->
        combFun votes === combFunDefault votes




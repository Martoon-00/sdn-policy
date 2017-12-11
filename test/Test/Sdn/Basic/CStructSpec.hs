{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists     #-}

-- | Tests for various quorum families used.

module Test.Sdn.Basic.CStructSpec
    ( spec
    ) where

import           Data.Default          (def)
import qualified Data.Set              as S
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Property, arbitrary, forAll, listOf1, resize,
                                        sublistOf, (===))
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
                    [ (AcceptorId 1, [Accepted $ GoodPolicy "1"])
                    , (AcceptorId 2, [Accepted $ GoodPolicy "1", Accepted $ GoodPolicy "2"])
                    ]
                    === Right [Accepted $ GoodPolicy "1"]

        describe "policy specific impl. vs straightforward impl." $ do
            prop "classic majority quorum" $
                forAll (resize 30 arbitrary) $
                    compareCombinations @ClassicMajorityQuorum

            prop "fast majority quorum" $
                forAll (resize 30 arbitrary) $
                    compareCombinations @FastMajorityQuorum

compareCombinations :: forall qf. QuorumFamily qf => Members -> Property
compareCombinations members = withMembers members $
    forAll (resize 5 $ listOf1 arbitrary) $
        \availablePolicies ->
    forAll (genVotes . genJust $ mkConfig <$> sublistOf availablePolicies) $
        \(votes :: Votes ClassicMajorityQuorum Configuration) ->
    combination votes === combinationDefault votes


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
import           Test.QuickCheck       (arbitrary, forAll, listOf1, resize, sublistOf,
                                        (===))
import           Universum

import           Sdn.Base

spec :: Spec
spec = do
    describe "combination" $ do
        describe "examples" $ do
            prop "good policies" $
                combination @Configuration @_ @ClassicMajorityQuorum def{ acceptorsNum = 3 }
                [ (AcceptorId 1, [Accepted $ GoodPolicy "1"])
                , (AcceptorId 2, [Accepted $ GoodPolicy "1", Accepted $ GoodPolicy "2"])
                ]
                === Right [Accepted $ GoodPolicy "1"]

        prop "policy specific impl. vs straightforward impl." $
            forAll (resize 30 arbitrary) $  -- for 'combinationDefault'
                \members@Members{..} ->
            forAll (resize 5 $ listOf1 arbitrary) $
                \availablePolicies ->
            forAll (resize acceptorsNum $ genVotes (S.fromList <$> sublistOf availablePolicies)) $
                \(votes :: Votes ClassicMajorityQuorum Configuration) ->
            combination members votes === combinationDefault members votes


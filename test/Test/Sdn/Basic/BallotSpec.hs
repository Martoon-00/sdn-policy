{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}

-- | Tests for various quorum families used.

module Test.Sdn.Basic.BallotSpec
    ( spec
    ) where

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.Core.Spec  (SpecWith)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary, (===))
import           Universum

import           Sdn.Base
import           Sdn.Protocol

spec :: Spec
spec = do
    describe "NumBallot instance" $ do
        describe "classic" $ do
            checkNumBallot @Classic

        describe "fast" $ do
            checkNumBallot @Fast

checkNumBallot
    :: forall pv.
       (ProtocolVersion pv, With [Show, Arbitrary] (BallotId pv))
    => SpecWith ()
checkNumBallot =
    prop "here and there" $ \(ballotId :: BallotId pv) ->
         (ballotId & flatBallotId %~ identity) === ballotId


{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Most of the primitive types used by protocol

module Sdn.Base.Types where

import           Control.Lens        (Wrapped (..))
import           Data.Default        (Default (..))
import           Data.MessagePack    (MessagePack)
import qualified Data.Text.Buildable
import           Formatting          (bprint, build, (%))
import           Test.QuickCheck     (Arbitrary (..), getNonNegative)
import           Universum

import           Sdn.Extra.Util      (rightSpaced)

-- | Type of the round.
data BallotType
    = SomeRound     -- when no one cares
    | ClassicRound  -- classic rounds
    | FastRound     -- fast rounds

instance Buildable (Proxy 'ClassicRound) where
    build _ = ""

instance Buildable (Proxy 'FastRound) where
    build _ = "fast"

instance Buildable (Proxy 'SomeRound) where
    build _ = "some"

-- | Ballot number.
newtype BallotId (t :: BallotType) = BallotId Int
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral, MessagePack, Generic)

instance Wrapped (BallotId t)

instance Buildable (Proxy t) => Buildable (BallotId t) where
    build (BallotId bid) =
        bprint (rightSpaced build%"ballot #"%build) (Proxy @t) bid

instance Default (BallotId t) where
    def = BallotId 0

instance Arbitrary (BallotId t) where
    arbitrary = BallotId . getNonNegative <$> arbitrary

coerceBallotId :: forall b a. BallotId a -> BallotId b
coerceBallotId (BallotId a) = BallotId a



-- | Identifier of acceptor.
newtype AcceptorId = AcceptorId Int
    deriving (Eq, Ord, Show, Enum, Num, MessagePack, Real, Integral)

instance Buildable AcceptorId where
    build (AcceptorId i) = bprint ("acceptor #"%build) i


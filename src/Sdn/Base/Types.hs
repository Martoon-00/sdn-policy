{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Most of the primitive types used by protocol

module Sdn.Base.Types where

import           Control.Lens        (Iso', Wrapped (..), from, iso)
import           Data.Default        (Default (..))
import           Data.MessagePack    (MessagePack)
import qualified Data.Text.Buildable
import           Formatting          (bprint, build, (%))
import           Test.QuickCheck     (Arbitrary (..), oneof)
import           Universum

-- | Raw ballot number.
newtype BallotCounter = BallotCounter Int
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral, MessagePack, Generic)

instance Wrapped BallotCounter

instance Buildable BallotCounter where
    build (BallotCounter bid) = bprint ("ballot #"%build) bid

instance Default BallotCounter where
    def = BallotCounter 0

instance Arbitrary BallotCounter where
    arbitrary = BallotCounter <$> arbitrary

-- | Allows arithmetic on ballots.
-- Implementations of methods below should be sensible.
class NumBallot b where
    flatBallotId :: Iso' b Rational

startBallotId :: NumBallot b => b
startBallotId = 0 ^. from flatBallotId

nextFreshBallotId :: NumBallot b => b -> b
nextFreshBallotId = flatBallotId %~ fromIntegral . identity @Int . floor . (+1)

prestartBallotId :: NumBallot b => b
prestartBallotId = (-1) ^. from flatBallotId

-- | Ballot id used in Classic Paxos.
newtype ClassicBallotId = ClassicBallotId BallotCounter
    deriving (Eq, Ord, Show, Num, MessagePack, Generic)

instance Wrapped ClassicBallotId

instance NumBallot ClassicBallotId where
    flatBallotId = _Wrapped' . _Wrapped' . iso fromIntegral round

instance Buildable ClassicBallotId where
    build = bprint ("classic "%build)

instance Arbitrary ClassicBallotId where
    arbitrary = ClassicBallotId <$> arbitrary

-- | Ballot id used in Fast Paxos.
data FastBallotId
    = FastBallotId BallotCounter
    | RecoveryBallotId BallotCounter
    deriving (Eq, Show, Generic)

instance Ord FastBallotId where
    compare = compare `on` view flatBallotId

instance Buildable FastBallotId where
    build = \case
        FastBallotId c -> bprint ("fast "%build) c
        RecoveryBallotId c -> bprint ("recovery "%build) c

instance MessagePack FastBallotId

instance NumBallot FastBallotId where
    flatBallotId = iso toFlat fromFlat
      where
        toFlat = \case
            FastBallotId c -> fromIntegral c
            RecoveryBallotId c -> fromIntegral c + 0.5
        fromFlat c =
            if even @Int $ round (2 * c)
            then FastBallotId (round c)
            else RecoveryBallotId (floor c)

instance Arbitrary FastBallotId where
    arbitrary = oneof
        [ FastBallotId <$> arbitrary
        , RecoveryBallotId <$> arbitrary
        ]


-- | Identifier of acceptor.
newtype AcceptorId = AcceptorId Int
    deriving (Eq, Ord, Show, Enum, Num, MessagePack, Real, Integral, Buildable)


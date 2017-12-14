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

-- | Ballot number.
newtype BallotId = BallotId Int
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral, MessagePack, Generic)

instance Wrapped BallotId

instance Buildable BallotId where
    build (BallotId bid) = bprint ("ballot #" %build) bid

instance Default BallotId where
    def = BallotId 0

instance Arbitrary BallotId where
    arbitrary = BallotId . getNonNegative <$> arbitrary


-- | Identifier of acceptor.
newtype AcceptorId = AcceptorId Int
    deriving (Eq, Ord, Show, Enum, Num, MessagePack, Real, Integral)

instance Buildable AcceptorId where
    build (AcceptorId i) = bprint ("acceptor #"%build) i


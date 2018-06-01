{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Most of the primitive types used by protocol

module Sdn.Base.Types where

import           Control.Lens        (Wrapped (..))
import           Data.Binary         (Binary)
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


-- | Identifier of abstract process.
newtype ProcessId pt = ProcessId Int
    deriving (Eq, Ord, Enum, Show, Num, MessagePack, Binary, Real, Integral)

instance Buildable (Proxy pt) => Buildable (ProcessId pt) where
    build (ProcessId i) = bprint (build%" #"%build) (Proxy @pt) i

data GeneralProcessTag
data ProposerTag
data LeaderTag
data AcceptorTag
data LearnerTag

type AcceptorId = ProcessId AcceptorTag

instance Buildable (Proxy AcceptorTag) where
    build _ = "acceptor"

type GeneralProcessId = ProcessId GeneralProcessTag

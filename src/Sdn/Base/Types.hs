{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Most of the primitive types used by protocol

module Sdn.Base.Types where

import           Data.Default        (Default (..))
import           Data.MessagePack    (MessagePack)
import qualified Data.Text.Buildable
import           Formatting          (bprint, build, (%))
import           Test.QuickCheck     (Arbitrary (..), CoArbitrary (..), getPositive,
                                      resize)
import           Universum

newtype BallotId = BallotId Int
    deriving (Eq, Ord, Show, Num, Generic)

instance Buildable BallotId where
    build (BallotId bid) = bprint ("ballot #"%build) bid

instance Default BallotId where
    def = BallotId 0

instance MessagePack BallotId


data Members = Members
    { acceptorsNum :: Int
    , learnersNum  :: Int
    } deriving (Show)

instance Default Members where
    def = Members
        { acceptorsNum = 3
        , learnersNum = 1
        }

instance Arbitrary Members where
    arbitrary =
        Members
            <$> (getPositive <$> arbitrary)
            <*> (getPositive <$> arbitrary)

instance CoArbitrary Members where
    -- this is not very beautiful, but I'd hardly ever use more than one learner
    coarbitrary Members{..} = resize acceptorsNum


newtype AcceptorId = AcceptorId Int
    deriving (Eq, Ord, Show, Enum, Num, MessagePack, Real, Integral, Buildable)


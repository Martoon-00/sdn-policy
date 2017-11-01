{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Most of the primitive types used by protocol

module Sdn.Base.Types where

import           Data.Default        (Default (..))
import           Data.MessagePack    (MessagePack)
import qualified Data.Text.Buildable
import           Formatting          (bprint, build, (%))
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
    }

instance Default Members where
    def = Members
        { acceptorsNum = 3
        , learnersNum = 1
        }


newtype AcceptorId = AcceptorId Int
    deriving (Eq, Ord, Show, Enum, Num, MessagePack, Real, Integral)

instance Buildable AcceptorId where
    build (AcceptorId id) = bprint ("from accptor #"%build) id
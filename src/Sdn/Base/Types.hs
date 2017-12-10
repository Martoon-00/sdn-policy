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


newtype AcceptorId = AcceptorId Int
    deriving (Eq, Ord, Show, Enum, Num, MessagePack, Real, Integral, Buildable)


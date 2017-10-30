{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Most of the primitive types used by protocol

module Sdn.Types where

import           Data.Default     (Default (..))
import           Data.MessagePack (MessagePack)
import           Universum

newtype BallotId = BallotId Int
    deriving (Eq, Ord, Show, Num, Generic)

instance Default BallotId where
    def = BallotId 0

instance MessagePack BallotId


{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Different versions of protocol.

module Sdn.Protocol.Versions where

import           Control.Lens        (makeLenses)
import qualified Data.Text.Buildable
import           Universum

-- * Protocol versions.

class (
      ) =>
      ProtocolVersion pv where

-- | Tag for classic version of protocol.
data Classic

instance Buildable (Proxy Classic) where
    build _ = ""

instance ProtocolVersion Classic where

-- | Tag for fast version of protocol with classic version used for recovery.
data Fast

instance Buildable (Proxy Fast) where
    build _ = "fast"

instance ProtocolVersion Fast where

-- * Utilities

data ForBothBallotTypes a = ForBothBallotTypes
    { _forClassicRound :: a
    , _forFastRound    :: a
    } deriving (Eq)

makeLenses ''ForBothBallotTypes


type family PerBallotType pv x where
    PerBallotType Classic x = x
    PerBallotType Fast x = ForBothBallotTypes x

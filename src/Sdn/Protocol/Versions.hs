{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Different versions of protocol.

module Sdn.Protocol.Versions where

import           Control.Lens        (makeLenses)
import qualified Data.Text.Buildable
import           Universum

import           Sdn.Base.Quorum

-- * Protocol versions.

class ( QuorumFamily (VersionQuorum pv)
      ) =>
      ProtocolVersion pv where
    type VersionQuorum pv :: *

-- | Tag for classic version of protocol.
data Classic

instance Buildable (Proxy Classic) where
    build _ = ""

instance ProtocolVersion Classic where
    type VersionQuorum Classic = ClassicMajorityQuorum

-- | Tag for fast version of protocol with classic version used for recovery.
data Fast

instance Buildable (Proxy Fast) where
    build _ = "fast"

instance ProtocolVersion Fast where
    type VersionQuorum Fast = FastMajorityQuorum

-- * Utilities

data ForBothBallotTypes a = ForBothBallotTypes
    { _forClassicRound :: a
    , _forFastRound    :: a
    } deriving (Eq)

makeLenses ''ForBothBallotTypes


type family PerBallotType pv x where
    PerBallotType Classic x = x
    PerBallotType Fast x = ForBothBallotTypes x

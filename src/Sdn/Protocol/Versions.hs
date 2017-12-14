{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Different versions of protocol.

module Sdn.Protocol.Versions where

import           Control.Lens        (makeLenses)
import           Data.Default
import qualified Data.Text.Buildable
import           Universum

import           Sdn.Base.Types

-- * Protocol versions.

class ( HasStartBallotId pv
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


class HasStartBallotId pv where
    type StartBallotType pv :: BallotType
    startBallotId :: BallotId (StartBallotType pv)
    default startBallotId
        :: Default (BallotId (StartBallotType pv))
        => BallotId (StartBallotType pv)
    startBallotId = def

instance HasStartBallotId Classic where
    type StartBallotType Classic = 'ClassicRound

instance HasStartBallotId Fast where
    type StartBallotType Fast = 'FastRound

-- * Utilities

data ForBothBallotTypes a = ForBothBallotTypes
    { _forClassicRound :: a
    , _forFastRound    :: a
    } deriving (Eq)

makeLenses ''ForBothBallotTypes


type family PerBallotType pv x where
    PerBallotType Classic x = x
    PerBallotType Fast x = ForBothBallotTypes x

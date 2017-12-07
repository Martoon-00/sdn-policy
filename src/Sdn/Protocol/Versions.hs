{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Different versions of protocol.

module Sdn.Protocol.Versions where

import           Data.Default   (def)
import           Universum

import           Sdn.Base.Types

-- | Tag for classic version of protocol.
data Classic
-- | Tag for fast version of protocol with classic version used for recovery.
data Fast


-- | Methods which depend on used version protocol.
class (Buildable (BallotId pv), NumBallot (BallotId pv)) =>
      ProtocolVersion pv where
    type BallotId pv :: *

    startBallotId :: BallotId pv
    nextFreshBallotId :: BallotId pv -> BallotId pv

instance ProtocolVersion Classic where
    type BallotId Classic = ClassicBallotId

    startBallotId = ClassicBallotId def
    nextFreshBallotId = (+1)

instance ProtocolVersion Fast where
    type BallotId Fast = FastBallotId

    startBallotId = FastBallotId def
    nextFreshBallotId = \case
        FastBallotId c -> FastBallotId (c + 1)
        RecoveryBallotId c -> FastBallotId (c + 1)

-- | Carrying protocol version of monad.
class Monad m => MonadProtocolVersion pv m | m -> pv where
    getProtocolVersion :: m (Proxy pv)

-- | And implementation for 'MonadProtocolVersion'.
newtype ProtocolVersionT pv m a = ProtocolVersionT
    { runProtocolVersion :: m a
    } deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (ProtocolVersionT pv) where
    lift = ProtocolVersionT

instance (Monad m, ProtocolVersion pv) =>
         MonadProtocolVersion pv (ProtocolVersionT pv m) where
    getProtocolVersion = pure Proxy

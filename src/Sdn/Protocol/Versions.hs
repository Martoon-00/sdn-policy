{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Different versions of protocol.

module Sdn.Protocol.Versions where

import           Control.TimeWarp.Rpc (RpcRequest (..))
import           Data.MessagePack     (MessagePack (..))
import           Language.Haskell.TH  as TH
import           Universum

import           Sdn.Base.Types

class ( NumBallot (BallotId pv)
      , Ord (BallotId pv)
      , Buildable (BallotId pv)
      , MessagePack (BallotId pv)
      ) =>
      ProtocolVersion pv where
    type BallotId pv :: *

-- | Tag for classic version of protocol.
data Classic
instance ProtocolVersion Classic where
    type BallotId Classic = ClassicBallotId

-- | Tag for fast version of protocol with classic version used for recovery.
data Fast
instance ProtocolVersion Fast where
    type BallotId Fast = FastBallotId


-- | Carrying protocol version of monad.
class Monad m => MonadProtocolVersion pv m | m -> pv where
    getProtocolVersion :: m (Proxy pv)

-- | And implementation for 'MonadProtocolVersion'.
newtype ProtocolVersionT pv m a = ProtocolVersionT
    { runProtocolVersion :: m a
    } deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (ProtocolVersionT pv) where
    lift = ProtocolVersionT

instance (Monad m) =>
         MonadProtocolVersion pv (ProtocolVersionT pv m) where
    getProtocolVersion = pure Proxy


-- | Template-haskell fun to reduce boilerplate.
declareMessagePV :: TH.Name -> TH.Q [TH.Dec]
declareMessagePV msgName = do
    let msgType = pure $ ConT msgName
    let msgStr = show @_ @String msgName
    [d| instance ProtocolVersion pv => MessagePack ($msgType pv)
        instance ProtocolVersion pv => RpcRequest ($msgType pv) where
            type Response ($msgType pv) = ()
            type ExpectedError ($msgType pv) = Void
            methodName _ = msgStr
      |]


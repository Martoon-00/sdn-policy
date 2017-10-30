{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Various utility functions

module Sdn.Util where

import           Universum

import           Control.TimeWarp.Rpc   (MonadRpc (..), NetworkAddress, RpcRequest (..),
                                         mkRequest)
import           Control.TimeWarp.Timed (MonadTimed (..), fork_)
import           Language.Haskell.TH    (Dec, Name, Q)

declareMessage :: Name -> Q [Dec]
declareMessage msgType = mkRequest msgType ''()


-- | Send asyncronously, supposing that remote method call returns nothing.
submit
    :: (MonadTimed m, MonadRpc m, RpcRequest msg, Response msg ~ ())
    => NetworkAddress -> msg -> m ()
submit = fork_ ... send

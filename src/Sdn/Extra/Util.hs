{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Various utility functions

module Sdn.Extra.Util where

import           Formatting             (Format, bprint, build, later)
import           Universum

import           Control.TimeWarp.Rpc   (MonadRpc (..), NetworkAddress, RpcRequest (..),
                                         mkRequest)
import qualified Control.TimeWarp.Rpc   as Rpc
import           Control.TimeWarp.Timed (MonadTimed (..), fork_)
import           Data.Text.Lazy.Builder (Builder)
import           Language.Haskell.TH    (Dec, Name, Q)

-- | Declare instance for one-way message.
declareMessage :: Name -> Q [Dec]
declareMessage msgType = mkRequest msgType ''()

type Message msg = (RpcRequest msg, Response msg ~ ())

-- | Send asyncronously, supposing that remote method call returns nothing.
submit
    :: (MonadCatch m, MonadTimed m, MonadRpc m, Message msg)
    => NetworkAddress -> msg -> m ()
submit = fork_ ... Rpc.submit

buildList
    :: (Container l, Buildable (Element l))
    => Builder -> Format r (l -> r)
buildList delim =
    later $ \(toList -> values) ->
    if null values
    then "[]"
    else mconcat $
         one "[ " <> (intersperse delim $ bprint build <$> values) <> one " ]"

modifyTVarS :: TVar s -> StateT s STM a -> STM a
modifyTVarS var modifier = do
    st <- readTVar var
    (res, st') <- runStateT modifier st
    writeTVar var st'
    return res

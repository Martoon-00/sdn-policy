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
import           Control.TimeWarp.Timed (MonadTimed (..), fork_)
import           Language.Haskell.TH    (Dec, Name, Q)

-- | Declare instance for one-way message.
declareMessage :: Name -> Q [Dec]
declareMessage msgType = mkRequest msgType ''()

-- | Send asyncronously, supposing that remote method call returns nothing.
submit
    :: (MonadTimed m, MonadRpc m, RpcRequest msg, Response msg ~ ())
    => NetworkAddress -> msg -> m ()
submit = fork_ ... send

buildList :: Buildable a => Format r ([a] -> r)
buildList =
    later $ \values ->
    mconcat $
        one "[" <> (intersperse ", " $ bprint build <$> values) <> one "]"


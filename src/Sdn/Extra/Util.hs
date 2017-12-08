{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Various utility functions

module Sdn.Extra.Util where

import           Control.Lens           (iso)
import           Data.MessagePack       (MessagePack)
import           Formatting             (Format, bprint, build, formatToString, later,
                                         shown, (%))
import           Universum

import           Control.TimeWarp.Rpc   (MonadRpc (..), NetworkAddress, RpcRequest (..),
                                         mkRequest)
import qualified Control.TimeWarp.Rpc   as Rpc
import           Control.TimeWarp.Timed (MonadTimed (..), fork_)
import           Data.Text.Lazy.Builder (Builder)
import qualified GHC.Exts               as Exts
import qualified Language.Haskell.TH    as TH

-- | Declare instance for one-way message.
declareMessage :: TH.Name -> TH.Q [TH.Dec]
declareMessage msgType = do
    dec1 <- [d| instance MessagePack $getFullType |]
    dec2 <- mkRequest msgType ''()
    return $ dec2 <> dec1
  where
    getFullType = do
        (name, vars) <- TH.reify msgType >>= \case
            TH.TyConI (TH.NewtypeD _ nname typeVars _ _ _) -> pure (nname, typeVars)
            TH.TyConI (TH.DataD _ dname typeVars _ _ _) -> pure (dname, typeVars)
            TH.TyConI (TH.TySynD tname typeVars _) -> pure (tname, typeVars)
            _ -> fail $ formatToString ("Type "%shown%" not found") msgType
        typeArgs <- replicateM (length vars) $ TH.VarT <$> TH.newName "a"
        pure $ foldl TH.AppT (TH.ConT name) typeArgs


type Message msg = (RpcRequest msg, Response msg ~ ())

-- | Send asyncronously, supposing that remote method call returns nothing.
submit
    :: (MonadCatch m, MonadTimed m, MonadRpc m, Message msg)
    => NetworkAddress -> msg -> m ()
submit = fork_ ... Rpc.submit

-- | Builder for list.
buildList
    :: (Container l, Buildable (Element l))
    => Builder -> Format r (l -> r)
buildList delim =
    later $ \(toList -> values) ->
    if null values
    then "[]"
    else mconcat $
         one "[ " <> (intersperse delim $ bprint build <$> values) <> one " ]"

-- | Extended modifier for 'TVar'.
modifyTVarS :: TVar s -> StateT s STM a -> STM a
modifyTVarS var modifier = do
    st <- readTVar var
    (res, st') <- runStateT modifier st
    writeTVar var st'
    return res

-- | Lens which looks inside the list-like structure
listL :: Exts.IsList l => Lens' l [Exts.Item l]
listL = iso Exts.toList Exts.fromList

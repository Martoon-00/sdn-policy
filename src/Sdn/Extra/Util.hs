{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Various utility functions

module Sdn.Extra.Util where

import           Control.Lens           (Iso, Iso', iso)
import           Control.TimeWarp.Rpc   (MonadRpc (..), NetworkAddress, RpcRequest (..),
                                         mkRequest)
import qualified Control.TimeWarp.Rpc   as Rpc
import           Control.TimeWarp.Timed (MonadTimed (..), fork_)
import           Data.Coerce            (coerce)
import           Data.MessagePack       (MessagePack)
import           Data.Text.Lazy.Builder (Builder)
import           Formatting             (Format, bprint, build, formatToString, later,
                                         shown, (%))
import           Formatting.Internal    (Format (..))
import qualified GHC.Exts               as Exts
import qualified Language.Haskell.TH    as TH
import           Test.QuickCheck        (Gen, suchThat)
import           Universum
import           Unsafe                 (unsafeFromJust)

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
listL
    :: (Exts.IsList (t a), Exts.IsList (t b))
    => Iso (t a) (t b) [Exts.Item (t a)] [Exts.Item (t b)]
listL = iso Exts.toList Exts.fromList

-- | Try generating until getting 'Just'.
genJust :: Gen (Maybe a) -> Gen a
genJust gen = unsafeFromJust <$> gen `suchThat` isJust

-- | Move from pure exception to monadic one.
throwOnFail
    :: (Exception e', MonadThrow m)
    => (e -> e') -> Either e a -> m a
throwOnFail mkException = either (throwM . mkException) pure

-- | Modify text produced by formatter.
mapfText :: (Builder -> Builder) -> Format a b -> Format a b
mapfText how (Format f) = Format $ f . \g t -> g $ how t

as :: forall b a. Coercible a b => Iso' a b
as = iso coerce coerce

-- | Add space at right if formatter returns non-empty text.
rightSpaced :: Format a b -> Format a b
rightSpaced = mapfText $ \x -> if x == "" then "" else x <> " "

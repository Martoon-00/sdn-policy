{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Memory storage used to keep state of processes.

module Sdn.Extra.MemStorage
    ( MemStorage (..)
    , ioRefMemStorage
    , stmMemStorage

    , HasMemStorage
    , MemStoreTxMonad
    , takeMemStorage
    , withMemStorage

    , MemStoreDecl (..)
    , declareMemStorage
    , DeclaresMemStore
    , DeclaredMemStore
    , DeclaredMemStoreTxMonad
    , getMemStorage
    ) where

import           Control.TimeWarp.Logging (WithNamedLogger)
import           Control.TimeWarp.Rpc     (MonadRpc)
import           Control.TimeWarp.Timed   (MonadTimed, ThreadId)
import           Data.Reflection          (Given (..), give)
import           Universum

import           Sdn.Extra.Logging
import           Sdn.Extra.Util           (atomicModifyIORefExcS, modifyTVarS)

-- | Base monad in which store allows to read/modify its entires.
type family MemStoreTxMonad (store :: * -> *) :: * -> *

-- | Set of operations for memory store.
data MemStorage store = forall modM. MonadCatch modM => MemStorage
    { mkMemStorage
        :: forall s m. (MonadIO m)
        => s -> m (store s)
      -- ^ Store creation.
      -- Works not in 'MemStoreTxMonad' for simplicity.
    , atomicallyModifyMemStorage
        :: forall s m a. (MonadIO m, MonadThrow m, MonadThrow (MemStoreTxMonad store))
        => store s -> StateT s modM a -> m a
      -- ^ Store content atomic modification.
      -- Works not in 'MemStoreTxMonad' for simplicity.
    , readMemStorage
        :: forall s. store s -> MemStoreTxMonad store s
      -- ^ Read store content.
    }

type instance MemStoreTxMonad IORef = IO

-- | Creates mem storage based on 'IORef'.
ioRefMemStorage :: MemStorage IORef
ioRefMemStorage = MemStorage newIORef atomicModifyIORefExcS readIORef

type instance MemStoreTxMonad TVar = STM

-- | Creates mem storage based on 'TVar'.
stmMemStorage :: MemStorage TVar
stmMemStorage = MemStorage (atomically . newTVar) (atomically ... modifyTVarS) readTVar


-- | Says mem storage is carried in the context.
type HasMemStorage store =
    ( Given (MemStorage store)
    , MonadCatch (MemStoreTxMonad store)
    )

-- | Pick mem storage from the context.
takeMemStorage :: HasMemStorage store => MemStorage store
takeMemStorage = given

-- | Provides mem storage context.
withMemStorage
    :: MonadCatch (MemStoreTxMonad store)
    => MemStorage store
    -> (HasMemStorage store => a)
    -> a
withMemStorage = give


-- | Allows to deterministically get storage type from monad stack used.
newtype MemStoreDecl (store :: * -> *) m a = MemStoreDecl
    { runMemStoreDecl :: m a
    } deriving (Functor, Applicative, Monad, MonadIO,
                MonadThrow, MonadCatch, MonadMask,
                MonadTimed, MonadRpc (o :: [*]), MonadLog, MonadReporting, WithNamedLogger)

type instance ThreadId (MemStoreDecl store m) = ThreadId m

-- | Binds specific mem storage type to the monad stack.
declareMemStorage
    :: MonadCatch (MemStoreTxMonad store)
    => MemStorage store
    -> (HasMemStorage store =>
            MemStoreDecl store m a)
    -> m a
declareMemStorage ms action = runMemStoreDecl $ withMemStorage ms action

-- | Get type of store used by 'MemStorage' in given monad.
type family DeclaredMemStore m where
    DeclaredMemStore (MemStoreDecl store m) = store
    DeclaredMemStore (t m) = DeclaredMemStore m

type DeclaresMemStore m = HasMemStorage (DeclaredMemStore m)

type DeclaredMemStoreTxMonad m = MemStoreTxMonad (DeclaredMemStore m)

-- | Get mem storage in given monad.
getMemStorage
    :: (Monad m, DeclaresMemStore m)
    => m (MemStorage (DeclaredMemStore m))
getMemStorage = pure takeMemStorage

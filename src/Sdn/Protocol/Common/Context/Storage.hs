{-# LANGUAGE Rank2Types #-}

-- | Atomic storage of single process state.

module Sdn.Protocol.Common.Context.Storage where

import           Universum

import           Sdn.Extra              (MonadLog, MonadReporting, PureLog,
                                          launchPureLog)
import           Sdn.Extra.MemStorage


-- | Context kept by single process.
data ProcessContext ctx = ProcessContext
    { pcState :: ctx  -- ^ Process'es mutable state
    }

-- | Envorinment for transaction modifying process state.
type TransactionM s a = forall n. (MonadCatch n) => PureLog (StateT s n) a

-- | Constraints for transaction.
type MonadTransaction ctx m =
    ( MonadIO m
    , MonadThrow m
    , MonadLog m
    , MonadReporting m
    , MonadReader (ProcessContext ctx) m
    )

-- | Atomically modify state stored by process.
-- If exception is thrown in the process, no changes apply.
withProcessStateAtomically
    :: (MonadTransaction (DeclaredMemStore m s) m, DeclaresMemStore m)
    => TransactionM s a -> m a
withProcessStateAtomically modifier = do
    ProcessContext{..} <- ask
    MemStorage{..} <- getMemStorage
    launchPureLog ({-# SCC state_modification #-} atomicallyModifyMemStorage pcState) modifier

-- | Provides batching opportunities.

module Sdn.Extra.Batching
   ( BatchingSettings (..)
   , batchedAction
   ) where

import           Control.Monad.Catch    (handleAll)
import           Control.TimeWarp.Timed (Microsecond, MonadTimed, for, fork, interval, ms,
                                         throwTo, wait)
import           Data.Default           (Default (..))
import qualified Data.Text.Buildable
import           Formatting             (bprint, build, (%))
import           Universum

import           Sdn.Extra.Util         (PreparedAction (..))

data BatchingSettings = BatchingSettings
    { batchMaxSize   :: Int
      -- ^ How much items should be collected to submit a batch.
    , batchMaxJitter :: Microsecond
      -- ^ How much time to wait before sending batch of items.
    }

instance Default BatchingSettings where
    def =
        BatchingSettings
        { batchMaxSize = 3
        , batchMaxJitter = interval 10 ms
        }

instance Buildable BatchingSettings where
    build BatchingSettings{..} =
        bprint ("  max batch size: "%build%
              "\n  max jitter: "%build)
            batchMaxSize
            batchMaxJitter

data PostponeSubmission = PostponeSubmission
    deriving (Show)

instance Exception PostponeSubmission


-- | Wrappes a proposal function so that it becomes to dump items in batches,
-- dictated by 'BatchingSettings'.
--
-- Note: this is not yet perfect. It waits for 'batchMaxJitter' even if there are
-- no items yet, while we want to wait only after first item has arrived.
batchedAction
    :: (MonadIO m, MonadCatch m, MonadTimed m)
    => BatchingSettings -> (NonEmpty a -> m ()) -> PreparedAction a m
batchedAction BatchingSettings{..} makeProposal = PreparedAction $ do
    when (batchMaxJitter < interval 1 ms) $
        error $ "Too small jitter passed: " <> show batchMaxJitter

    batchRef <- newIORef (0, [])
    periodicSubmitterTid <- fork $ preventBigJitter batchRef
    return $ rememberAndMaybeSubmit periodicSubmitterTid batchRef
  where
    preventBigJitter batchRef =
        handleAll (\_ -> preventBigJitter batchRef) $
            forever $ do
                wait (for batchMaxJitter)
                batch <- atomicModifyIORef batchRef $ \(_, remembered) ->
                    ((0, []), remembered)

                whenNotNull batch makeProposal

    rememberAndMaybeSubmit periodicSubmitterTid batchRef proposal = do
        mReadyBatch <- atomicModifyIORef batchRef $ \(num, remembered) ->
            if num + 1 >= batchMaxSize
            then ((0, []), Just (proposal :| remembered))
            else ((num + 1, proposal : remembered), Nothing)

        whenJust mReadyBatch $ \proposals -> do
            throwTo periodicSubmitterTid PostponeSubmission
            makeProposal proposals



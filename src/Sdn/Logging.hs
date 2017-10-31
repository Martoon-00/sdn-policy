-- | Configure logging for network.

module Sdn.Logging where

import           System.Wlog
import           System.Wlog.Formatter
import           Universum

import           Sdn.Processes

(&>) :: s -> State s () -> s
(&>) = flip execState

processNameT :: Process p => p -> Text
processNameT = pretty . processName

initLogging :: MonadIO m => m ()
initLogging =
    setupLogging (Just centiUtcTimeF) $
        productionB{ _lcTree = tree, _lcTermSeverity = Just Debug }
  where
    tree :: LoggerTree
    tree = mempty { _ltSeverity = Just Debug }


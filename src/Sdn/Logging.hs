-- | Configure logging for network.

module Sdn.Logging where

import           Control.Lens          (Iso', iso)
import qualified System.Console.ANSI   as ANSI
import           System.Wlog
import           System.Wlog.Formatter
import           Universum


(&>) :: s -> State s () -> s
(&>) = flip execState

loggerNameT :: Iso' LoggerName Text
loggerNameT = iso pretty (fromString . toString)

initLogging :: MonadIO m => m ()
initLogging =
    setupLogging (Just centiUtcTimeF) $
        productionB{ _lcTree = tree, _lcTermSeverity = Just Debug }
  where
    tree :: LoggerTree
    tree = mempty { _ltSeverity = Just Debug }

withColor :: ANSI.ColorIntensity -> ANSI.Color -> Text -> Text
withColor intensity color text =
    mconcat
    [ toText $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground intensity color]
    , text
    , toText $ ANSI.setSGRCode [ANSI.Reset]
    ]

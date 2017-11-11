{-# LANGUAGE DataKinds #-}

-- | Configure logging for network.

module Sdn.Extra.Logging where

import           Control.Concurrent             (forkIO)
import qualified Control.Concurrent.STM.TBMChan as TBM
import           Control.Lens                   (Iso', iso)
import           Control.TimeWarp.Logging       (LoggerName (..), WithNamedLogger (..))
import           Control.TimeWarp.Timed         (Microsecond, MonadTimed (..))
import           Data.Time.Units                (toMicroseconds)
import           Formatting                     (left, right, sformat, (%))
import           GHC.IO.Unsafe                  (unsafePerformIO)
import qualified System.Console.ANSI            as ANSI
import           Universum


loggerNameT :: Iso' LoggerName Text
loggerNameT = iso pretty (fromString . toString)

withColor :: ANSI.ColorIntensity -> ANSI.Color -> Text -> Text
withColor intensity color text =
    mconcat
    [ toText $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground intensity color]
    , text
    , toText $ ANSI.setSGRCode [ANSI.Reset]
    ]

data LogEntry = LogEntry LoggerName Microsecond Text

loggingFormatter :: LogEntry -> Text
loggingFormatter (LogEntry name (toMicroseconds -> time) msg) =
    mconcat
    [ inGray "["
    , pretty name
    , inGray "]"
    , " "
    , inGray "["
    , timeText
    , inGray "]"
    , " "
    , msg
    ]
  where
    inGray = withColor ANSI.Dull ANSI.White
    timeText =
        let seconds = time `div` 1000000
            centiseconds = time `div` 10000 `mod` 100
        in sformat (left 3 '0'%":"%right 2 '0') seconds centiseconds

logBuffer :: TBM.TBMChan LogEntry
logBuffer = unsafePerformIO $ do
    chan <- TBM.newTBMChanIO 100
    _ <- forkIO . void . runMaybeT . forever $ do
        entry <- MaybeT . atomically $ TBM.readTBMChan chan
        lift . putText $ loggingFormatter entry

    return chan
{-# NOINLINE logBuffer #-}

type MonadLog m = With [MonadIO, MonadTimed, WithNamedLogger] m

dropName :: LoggerName
dropName = "drop"

isDropName :: LoggerName -> Bool
isDropName (LoggerName name) =
    let LoggerName dropName' = dropName
    in  dropName' `isPrefixOf` name

setDropLoggerName :: WithNamedLogger m => m a -> m a
setDropLoggerName = modifyLoggerName (dropName <> )

logInfo :: MonadLog m => Text -> m ()
logInfo msg = do
    time <- virtualTime
    name <- getLoggerName
    let entry = LogEntry name time msg
    unless (isDropName name) $
        atomically $ TBM.writeTBMChan logBuffer entry

logError :: MonadLog m => Text -> m ()
logError = logInfo . (withColor ANSI.Dull ANSI.Red "Error: " <> )

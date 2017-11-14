{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Configure logging for network.

module Sdn.Extra.Logging
    ( MonadLog
    , loggerNameT
    , withColor
    , setDropLoggerName
    , logInfo
    , logError

    , MonadReporting
    , ErrorReporting (..)
    , runErrorReporting
    , NoErrorReporting (..)
    , reportError
    ) where

import           Control.Concurrent             (forkIO)
import qualified Control.Concurrent.STM.TBMChan as TBM
import           Control.Lens                   (Iso', iso)
import           Control.Monad.Reader           (mapReaderT)
import           Control.TimeWarp.Logging       (LoggerName (..), LoggerNameBox (..),
                                                 WithNamedLogger (..))
import           Control.TimeWarp.Rpc           (MonadRpc)
import           Control.TimeWarp.Timed         (Microsecond, MonadTimed (..), ThreadId)
import           Data.Time.Units                (toMicroseconds)
import           Formatting                     (left, sformat, (%))
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
    deriving (Show)

loggingFormatter :: LogEntry -> Text
loggingFormatter (LogEntry name (toMicroseconds -> time) msg) =
    mconcat
    [ inGray "["
    , pretty name
    , inGray "]"
    , " "
    , inGray $ mconcat
        [ "["
        , timeText
        , "]"
        ]
    , " "
    , msg
    ]
  where
    inGray = withColor ANSI.Dull ANSI.White
    timeText =
        let seconds = time `div` 1000000
            centiseconds = time `div` 10000 `mod` 100
        in sformat (left 3 '0'%":"%left 2 '0') seconds centiseconds

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


class Monad m => MonadReporting m where
    reportError :: Text -> m ()
    default reportError
        :: (MonadTrans t, Monad n, MonadReporting n, t n ~ m)
        => Text -> m ()
    reportError = lift . reportError

instance MonadReporting m => MonadReporting (ReaderT __ m) where
instance MonadReporting m => MonadReporting (LoggerNameBox m) where

newtype ErrorReporting m a = ErrorReporting
    { getErrorReporting :: ReaderT (TVar [Text]) m a
    } deriving ( Functor, Applicative, Monad, MonadIO, MonadTrans
               , MonadThrow, MonadCatch
               , MonadState __, MonadTimed, MonadRpc, WithNamedLogger)

type instance ThreadId (ErrorReporting m) = ThreadId m

instance MonadReader r m => MonadReader r (ErrorReporting m) where
    reader = lift . reader
    ask = lift ask
    local modifier = ErrorReporting . mapReaderT (local modifier) . getErrorReporting

runErrorReporting :: MonadIO m => ErrorReporting m a -> m ([Text], a)
runErrorReporting (ErrorReporting action) = do
    var <- newTVarIO mempty
    res <- runReaderT action var
    errs <- readTVarIO var
    return (reverse errs, res)

instance MonadIO m => MonadReporting (ErrorReporting m) where
    reportError err = ErrorReporting $ do
        var <- ask
        atomically $ modifyTVar' var (err :)

newtype NoErrorReporting m a = NoErrorReporting
    { runNoErrorReporting :: m a
    } deriving ( Functor, Applicative, Monad, MonadIO
               , MonadThrow, MonadCatch
               , MonadState __, MonadTimed, MonadRpc, WithNamedLogger)

instance MonadTrans NoErrorReporting where
    lift = NoErrorReporting

type instance ThreadId (NoErrorReporting m) = ThreadId m

instance Monad m => MonadReporting (NoErrorReporting m) where
    reportError _ = pass

logError :: (MonadLog m, MonadReporting m) => Text -> m ()
logError msg = do
    logInfo $ withColor ANSI.Dull ANSI.Red "Error: " <> msg
    loggerName <- getLoggerName
    reportError (pretty loggerName <> ": " <> msg)


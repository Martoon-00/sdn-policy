{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Configure logging for network.

module Sdn.Extra.Logging
    ( MonadLog
    , loggerNameT
    , withColor
    , resetColoring
    , setDropLoggerName
    , isDropLoggerName
    , logInfo
    , logError

    , MonadReporting
    , ErrorReporting (..)
    , runErrorReporting
    , NoErrorReporting (..)
    , reportError
    , PureLog
    , launchPureLog
    , DroppingLog
    , runDroppingLog
    ) where

import           Control.Concurrent       (forkIO, threadDelay)
import           Control.Lens             (Iso', iso, makeLenses, (%=))
import           Control.Monad.Reader     (mapReaderT)
import           Control.TimeWarp.Logging (LoggerName (..), LoggerNameBox (..),
                                           WithNamedLogger (..))
import           Control.TimeWarp.Rpc     (DelaysLayer, ExtendedRpcOptions, MonadRpc)
import           Control.TimeWarp.Timed   (Microsecond, MonadTimed (..), ThreadId, ms)
import           Data.List                (isInfixOf)
import qualified Data.Text                as T
import           Data.Time.Units          (toMicroseconds)
import           Formatting               (build, left, sformat, stext, (%))
import           GHC.IO.Unsafe            (unsafePerformIO)
import qualified System.Console.ANSI      as ANSI
import           Universum                hiding (pass)

import           Sdn.Extra.Util           (MonadicMark (..), coloredF, gray)

-- * Util

loggerNameT :: Iso' LoggerName Text
loggerNameT = iso pretty (fromString . toString)

withColor :: (ANSI.ColorIntensity, ANSI.Color) -> Text -> Text
withColor (intensity, color) text =
    mconcat
    [ toText $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground intensity color]
    , text
    , toText $ ANSI.setSGRCode [ANSI.Reset]
    ]

resetColoring :: Text -> Text
resetColoring text =
    let t:ts = T.splitOn "\ESC" text
        removeColoring = T.drop 1 . T.dropWhile (/= 'm')
    in  mconcat $ t : map removeColoring ts

data LogEntry = LogEntry LoggerName Microsecond Text
    deriving (Show)

loggingFormatter :: LogEntry -> Text
loggingFormatter (LogEntry name (toMicroseconds -> time) msg) =
    sformat (coloredF gray ("["%build%"]") % " "
            %coloredF gray ("["%build%"]") % " "
            %stext)
        name
        timeText
        msg
  where
    timeText =
        let seconds = time `div` 1000000
            centiseconds = time `div` 10000 `mod` 100
        in sformat (left 3 '0'%":"%left 2 '0') seconds centiseconds

dropName :: LoggerName
dropName = "-"

isDropLoggerName :: LoggerName -> Bool
isDropLoggerName (LoggerName name) =
    let LoggerName dropName' = dropName
    in  (dropName' <> ".") `isInfixOf` name

setDropLoggerName :: WithNamedLogger m => m a -> m a
setDropLoggerName = modifyLoggerName (dropName <> )

-- * Logging

class Monad m => MonadLog m where
    logInfoPack :: [Text] -> m ()
    default logInfoPack
        :: (MonadTrans t, Monad n, MonadLog n, t n ~ m)
        => [Text] -> m ()
    logInfoPack = lift . logInfoPack

logInfo :: MonadLog m => Text -> m ()
logInfo = logInfoPack . one

instance MonadLog m => MonadLog (ReaderT r m)
instance MonadLog m => MonadLog (StateT r m)
instance MonadLog m => MonadLog (MaybeT m)
instance MonadLog m => MonadLog (MonadicMark mark m)

logBuffer :: IORef [LogEntry]
logBuffer = unsafePerformIO $ do
    var <- newIORef []
    _ <- forkIO . forever $ do
        entries <- atomicModifyIORef var (\es -> ([], es))
        forM_ (reverse entries) $ \entry ->
            putText $ loggingFormatter entry
        threadDelay (fromIntegral . toMicroseconds $ ms 10)

    return var
{-# NOINLINE logBuffer #-}

instance With [MonadIO, MonadTimed] m => MonadLog (LoggerNameBox m) where
    logInfoPack msgs = do
        name <- getLoggerName
        unless (isDropLoggerName name) $ do
            time <- virtualTime
            let entries = LogEntry name time <$> msgs
            liftIO $ atomicModifyIORef logBuffer (\es -> (entries ++ es, ()))

-- * Error reporting

class Monad m => MonadReporting m where
    reportError :: Text -> m ()
    default reportError
        :: (MonadTrans t, Monad n, MonadReporting n, t n ~ m)
        => Text -> m ()
    reportError = lift . reportError

instance MonadReporting m => MonadReporting (ReaderT __ m) where
instance MonadReporting m => MonadReporting (StateT __ m) where
instance MonadReporting m => MonadReporting (MaybeT m) where
instance MonadReporting m => MonadReporting (LoggerNameBox m) where
instance MonadReporting m => MonadReporting (DelaysLayer m) where
instance MonadReporting m => MonadReporting (ExtendedRpcOptions (o :: [*]) (os :: [*]) m)
instance MonadReporting m => MonadReporting (MonadicMark __ m)

-- ** Error reporting enabled

newtype ErrorReporting m a = ErrorReporting
    { getErrorReporting :: ReaderT (TVar [Text]) m a
    } deriving ( Functor, Applicative, Monad, MonadIO, MonadTrans
               , MonadThrow, MonadCatch
               , MonadState __, MonadTimed, MonadRpc (os :: [*]), WithNamedLogger, MonadLog)

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


instance (MonadIO m, WithNamedLogger m) =>
         MonadReporting (ErrorReporting m) where
    reportError err =
        ErrorReporting $ do
            var <- ask
            logName <- getLoggerName
            let msg = pretty logName <> ": " <> err
            atomically $ modifyTVar' var (msg :)

-- ** Error reporting disabled

newtype NoErrorReporting m a = NoErrorReporting
    { runNoErrorReporting :: m a
    } deriving ( Functor, Applicative, Monad, MonadIO
               , MonadThrow, MonadCatch
               , MonadState __
               , MonadTimed, MonadRpc (os :: [*]), WithNamedLogger, MonadLog)

instance MonadTrans NoErrorReporting where
    lift = NoErrorReporting

type instance ThreadId (NoErrorReporting m) = ThreadId m

instance Monad m => MonadReporting (NoErrorReporting m) where
    reportError _ = return ()

logError :: (MonadLog m, MonadReporting m) => Text -> m ()
logError msg = do
    logInfo $ withColor (ANSI.Dull, ANSI.Red) "Error: " <> msg
    reportError msg

-- * Pure logging & error reporting

data LogAndError = LogAndError
    { _logsPart :: [Text]
    , _errsPart :: [Text]
    }

makeLenses ''LogAndError

instance Monoid LogAndError where
    mempty = LogAndError mempty mempty
    LogAndError l1 e1 `mappend` LogAndError l2 e2 =
        LogAndError (l1 <> l2) (e1 <> e2)

-- | Monad which carries made logs.
newtype PureLog m a = PureLog (StateT LogAndError m a)
    deriving ( Functor, Applicative, Monad, MonadIO, MonadTrans
             , MonadThrow, MonadCatch, MonadReader __)

launchPureLog
    :: (MonadCatch m, MonadLog n, MonadReporting n)
    => (forall x. m (x, a) -> n (x, b)) -> PureLog m a -> n b
launchPureLog hoist' (PureLog action) = do
    (logs, res) <- hoist' $ swap <$> runStateT action mempty
    logInfoPack (_logsPart logs)
    mapM_ reportError (_errsPart logs)
    return res

instance MonadState s m => MonadState s (PureLog m) where
    get = lift get
    put = lift . put
    state = lift . state

instance Monad m => MonadLog (PureLog m) where
    logInfoPack msgs = PureLog $
        logsPart %= (msgs ++)

instance Monad m => MonadReporting (PureLog m) where
    reportError err = PureLog $ errsPart %= (err :)

-- | Monad which drops logs.
newtype DroppingLog m a = DroppingLog
    { runDroppingLog :: m a
    } deriving ( Functor, Applicative, Monad, MonadIO
               , MonadThrow, MonadCatch, MonadReader __, MonadState __)

instance MonadTrans DroppingLog where
    lift = DroppingLog

instance Monad m => MonadLog (DroppingLog m) where
    logInfoPack _ = return ()

instance Monad m => MonadReporting (DroppingLog m) where
    reportError _ = return ()


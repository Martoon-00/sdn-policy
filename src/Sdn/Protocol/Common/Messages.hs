{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Common for messages of various protocols.

module Sdn.Protocol.Common.Messages
    ( HasMessageShortcut (..)
    ) where

import           Control.TimeWarp.Logging (LoggerName)

-- | Suffix used in logging for incoming messages of given type.
class HasMessageShortcut msg where
    messageShortcut :: LoggerName

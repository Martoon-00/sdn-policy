{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Common for messages of various protocols.

module Sdn.Protocol.Common.Messages
    ( HasMessageShortcut (..)

    , CommittedMsg (..)
    ) where

import           Control.TimeWarp.Logging (LoggerName)
import qualified Data.Text.Buildable
import           Formatting               (bprint, build, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra                (declareMessage, listF)

-- | Suffix used in logging for incoming messages of given type.
class HasMessageShortcut msg where
    messageShortcut :: LoggerName


newtype CommittedMsg = CommittedMsg (NonEmpty Policy)
    deriving (Generic)

instance Buildable CommittedMsg where
    build (CommittedMsg p) = bprint ("Policies commission message "%listF ", " build) p
instance HasMessageShortcut CommittedMsg where
    messageShortcut = "commited"

declareMessage ''CommittedMsg


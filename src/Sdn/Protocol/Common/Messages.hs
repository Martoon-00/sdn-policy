{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Common for messages of various protocols.

module Sdn.Protocol.Common.Messages
    ( HasMessageShortcut (..)

    , CommittedMsg (..)
    ) where

import           Control.TimeWarp.Logging (LoggerName)
import           Data.MessagePack
import qualified Data.Text.Buildable
import           Formatting               (bprint, build, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra                (declareMessage, listF)

-- | Suffix used in logging for incoming messages of given type.
class HasMessageShortcut msg where
    messageShortcut :: LoggerName


-- | Message sent by learner to proposer in order to acknowledge that value has
-- been learned.
data CommittedMsg cstruct = CommittedMsg (NonEmpty (RawCmd cstruct))
    deriving (Generic)

instance Buildable (RawCmd cstruct) =>
         Buildable (CommittedMsg cstruct) where
    build (CommittedMsg p) = bprint ("Policies commission message "%listF ", " build) p

instance HasMessageShortcut (CommittedMsg cstruct) where
    messageShortcut = "commited"

instance MessagePack (RawCmd cstruct) => MessagePack (CommittedMsg cstruct)

declareMessage ''CommittedMsg

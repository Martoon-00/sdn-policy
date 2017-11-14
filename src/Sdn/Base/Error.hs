-- | Errors used in protocol

module Sdn.Base.Error where

import           Universum

import qualified Data.Text.Buildable
import           Formatting          (bprint, stext, (%))

newtype ProtocolError = ProtocolError Text
    deriving (Show)

instance Exception ProtocolError

instance Buildable ProtocolError where
    build (ProtocolError msg) = bprint ("Protocol error: "%stext) msg

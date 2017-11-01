-- | Errors used in protocol

module Sdn.Error where

import           Universum

newtype ProtocolError = ProtocolError Text
    deriving (Show)

instance Exception ProtocolError

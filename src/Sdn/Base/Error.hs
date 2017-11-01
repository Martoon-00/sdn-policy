-- | Errors used in protocol

module Sdn.Base.Error where

import           Universum

newtype ProtocolError = ProtocolError Text
    deriving (Show)

instance Exception ProtocolError

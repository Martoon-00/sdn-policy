{-# LANGUAGE GADTs #-}

-- | Errors used in protocol

module Sdn.Base.Error where

import           Universum
import qualified Text.Show

import qualified Data.Text.Buildable
import           Formatting          (bprint, stext, (%), string)

data ProtocolError = HasCallStack => ProtocolError Text

instance Show ProtocolError where
  show = toString . pretty

instance Exception ProtocolError

instance Buildable ProtocolError where
    build = \case
      ProtocolError msg ->
        bprint ("Protocol error: "%stext%"\n"%string) msg (prettyCallStack callStack)

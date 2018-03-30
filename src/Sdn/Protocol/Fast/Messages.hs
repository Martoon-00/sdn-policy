{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Special messages used in Fast Paxos.
--
-- Some messages from Classic Paxos are also used, but not mentioned here.

module Sdn.Protocol.Fast.Messages
    ( ProposalMsg (..)
    , Phase2bMsg (..)
    ) where

import           Data.MessagePack
import qualified Data.Text.Buildable
import           Formatting                   (bprint, build, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra
import           Sdn.Protocol.Common.Messages (HasMessageShortcut (..))

-- | Message sent by proposer to acceptors to propose a new values.
-- It carries multiple values for optimization purposes.
newtype ProposalMsg cstruct = ProposalMsg (NonEmpty (RawCmd cstruct))
    deriving (Generic)

instance Buildable (RawCmd cstruct) =>
         Buildable (ProposalMsg cstruct) where
    build (ProposalMsg p) = bprint ("Proposal message "%listF "," build) p

instance HasMessageShortcut (ProposalMsg cstruct) where
    messageShortcut = "rem" <> "f"

instance MessagePack (RawCmd cstruct) =>
         MessagePack (ProposalMsg cstruct)

declareMessage ''ProposalMsg


data Phase2bMsg cstruct = Phase2bMsg AcceptorId (NonEmpty $ Cmd cstruct)
    deriving (Generic)

instance Buildable (Cmd cstruct) =>
         Buildable (Phase2bMsg cstruct) where
    build (Phase2bMsg a c) =
        bprint ("Phase 2b message from "%build%": "%listF "," build) a c

instance HasMessageShortcut (Phase2bMsg cstruct) where
    messageShortcut = "f"

instance MessagePack (Cmd cstruct) => MessagePack (Phase2bMsg cstruct)

declareMessage ''Phase2bMsg


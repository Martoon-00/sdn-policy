{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Special messages used in Fast Paxos.
--
-- Some messages from Classic Paxos are also used, but not mentioned here.

module Sdn.Protocol.Fast.Messages
    ( ProposalMsg (..)
    , AcceptedMsg (..)
    ) where

import           Control.TimeWarp.Rpc         (RpcRequest (..))
import           Data.MessagePack
import qualified Data.Text.Buildable
import           Formatting                   (bprint, build, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra
import           Sdn.Protocol.Common.Messages (HasMessageShortcut (..))
import           Sdn.Protocol.Processes

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

declareMessage 21 ''ProposalMsg


-- | Message sent by acceptor to learners, notifying them about
-- new value.
-- This can be sent to leader as well. Thus, recipient is specified
-- in type, because networing enging doesn't allow listening
-- for same message from different places.
data AcceptedMsg recip cstruct = AcceptedMsg AcceptorId (NonEmpty $ Cmd cstruct)
    deriving (Generic)

instance Buildable (Cmd cstruct) =>
         Buildable (AcceptedMsg recip cstruct) where
    build (AcceptedMsg a c) =
        bprint ("Phase 2b message from "%build%": "%listF "," build) a c

instance HasMessageShortcut (AcceptedMsg recip cstruct) where
    messageShortcut = "f"

instance MessagePack (Cmd cstruct) => MessagePack (AcceptedMsg recip cstruct)

instance RpcRequest (AcceptedMsg Leader cstruct) where
    type Response (AcceptedMsg Leader cstruct) = ()
    messageId _ = 22

instance RpcRequest (AcceptedMsg Learner cstruct) where
    type Response (AcceptedMsg Learner cstruct) = ()
    messageId _ = 23



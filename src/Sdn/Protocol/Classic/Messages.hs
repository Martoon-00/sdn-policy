{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Messages used by Classic Paxos.

module Sdn.Protocol.Classic.Messages
    ( ProposalMsg (..)
    , Phase1aMsg (..)
    , Phase1bMsg (..)
    , Phase2aMsg (..)
    , Phase2bMsg (..)
    ) where

import           Data.MessagePack             (MessagePack)
import qualified Data.Text.Buildable
import           Formatting                   (bprint, build, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra                    (declareMessage)
import           Sdn.Protocol.Common.Messages (HasMessageShortcut (..))

-- | Message sent by proposer to leader in order to propose a new value.
newtype ProposalMsg cstruct = ProposalMsg (RawCmd cstruct)
    deriving (Generic)

instance Buildable (RawCmd cstruct) =>
         Buildable (ProposalMsg cstruct) where
    build (ProposalMsg p) = bprint ("Proposal message "%build) p

instance HasMessageShortcut (ProposalMsg cstruct) where
    messageShortcut = "1a"

instance MessagePack (RawCmd cstruct) =>
         MessagePack (ProposalMsg cstruct)

declareMessage ''ProposalMsg


-- | Message sent by leader to acceptors in order to initiate a new ballot and
-- request recent acceptors' state.
data Phase1aMsg = Phase1aMsg BallotId
    deriving (Generic)

instance Buildable Phase1aMsg where
    build (Phase1aMsg b) = bprint ("Phase 1a message "%build) b

instance HasMessageShortcut Phase1aMsg where
    messageShortcut = "1b"

instance MessagePack Phase1aMsg

declareMessage ''Phase1aMsg


-- | "Promise" message sent from acceptor to leader.
data Phase1bMsg cstruct = Phase1bMsg AcceptorId BallotId cstruct
    deriving (Generic)

instance Buildable cstruct =>
         Buildable (Phase1bMsg cstruct) where
    build (Phase1bMsg a b c) =
        bprint ("Phase 1b message from "%build%" "%build%" "%build) a b c

instance HasMessageShortcut (Phase1bMsg cstruct) where
    messageShortcut = "2a"

instance MessagePack cstruct => MessagePack (Phase1bMsg cstruct)

declareMessage ''Phase1bMsg


-- | Message sent by leader to acceptors to order them to accept a new cstruct.
data Phase2aMsg cstruct = Phase2aMsg BallotId cstruct
    deriving (Generic)

instance Buildable cstruct =>
         Buildable (Phase2aMsg cstruct) where
    build (Phase2aMsg b c) =
        bprint ("Phase 2a message "%build%" "%build) b c

instance HasMessageShortcut (Phase2aMsg cstruct) where
    messageShortcut = "2b"

instance MessagePack cstruct => MessagePack (Phase2aMsg cstruct)

declareMessage ''Phase2aMsg


-- | Message sent by acceptor to learner in order to make learner fixate new
-- cstruct.
data Phase2bMsg cstruct = Phase2bMsg AcceptorId cstruct
    deriving (Generic)

instance Buildable cstruct =>
         Buildable (Phase2bMsg cstruct) where
    build (Phase2bMsg a c) =
        bprint ("Phase 2b message from "%build%" "%build) a c

instance HasMessageShortcut (Phase2bMsg cstruct) where
    messageShortcut = mempty

instance MessagePack cstruct => MessagePack (Phase2bMsg cstruct)

declareMessage ''Phase2bMsg


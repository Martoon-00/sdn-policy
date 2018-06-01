{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Messages used by Classic Paxos.

module Sdn.Protocol.Classic.Messages
    ( ProposalMsg (..)
    , PrepareMsg (..)
    , PromiseMsg (..)
    , AcceptRequestMsg (..)
    , AcceptedMsg (..)
    ) where

import           Data.MessagePack             (MessagePack)
import qualified Data.Text.Buildable
import           Formatting                   (bprint, build, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra                    (declareMessage, listF)
import           Sdn.Protocol.Common.Messages (HasMessageShortcut (..))

-- | Message sent by proposer to leader in order to propose a new value.
newtype ProposalMsg cstruct = ProposalMsg (NonEmpty $ RawCmd cstruct)
    deriving (Generic)

instance Buildable (RawCmd cstruct) =>
         Buildable (ProposalMsg cstruct) where
    build (ProposalMsg p) = bprint ("Proposal message "%listF "," build) p

instance HasMessageShortcut (ProposalMsg cstruct) where
    messageShortcut = "propose"

instance MessagePack (RawCmd cstruct) =>
         MessagePack (ProposalMsg cstruct)

declareMessage 11 ''ProposalMsg


-- | Message sent by leader to acceptors in order to initiate a new ballot and
-- request recent acceptors' state.
data PrepareMsg = PrepareMsg BallotId
    deriving (Generic)

instance Buildable PrepareMsg where
    build (PrepareMsg b) = bprint ("Phase 1a message "%build) b

instance HasMessageShortcut PrepareMsg where
    messageShortcut = "prepare"

instance MessagePack PrepareMsg

declareMessage 12 ''PrepareMsg


-- | "Promise" message sent from acceptor to leader.
data PromiseMsg cstruct = PromiseMsg AcceptorId BallotId cstruct
    deriving (Generic)

instance Buildable cstruct =>
         Buildable (PromiseMsg cstruct) where
    build (PromiseMsg a b c) =
        bprint ("Phase 1b message from "%build%" "%build%" "%build) a b c

instance HasMessageShortcut (PromiseMsg cstruct) where
    messageShortcut = "promise"

instance MessagePack cstruct => MessagePack (PromiseMsg cstruct)

declareMessage 13 ''PromiseMsg


-- | Message sent by leader to acceptors to order them to accept a new cstruct.
data AcceptRequestMsg cstruct = AcceptRequestMsg BallotId cstruct
    deriving (Generic)

instance Buildable cstruct =>
         Buildable (AcceptRequestMsg cstruct) where
    build (AcceptRequestMsg b c) =
        bprint ("Phase 2a message "%build%" "%build) b c

instance HasMessageShortcut (AcceptRequestMsg cstruct) where
    messageShortcut = "accept"

instance MessagePack cstruct => MessagePack (AcceptRequestMsg cstruct)

declareMessage 14 ''AcceptRequestMsg


-- | Message sent by acceptor to learner in order to make learner fixate new
-- cstruct.
data AcceptedMsg cstruct = AcceptedMsg AcceptorId cstruct
    deriving (Generic)

instance Buildable cstruct =>
         Buildable (AcceptedMsg cstruct) where
    build (AcceptedMsg a c) =
        bprint ("Phase 2b message from "%build%" "%build) a c

instance HasMessageShortcut (AcceptedMsg cstruct) where
    messageShortcut = mempty

instance MessagePack cstruct => MessagePack (AcceptedMsg cstruct)

declareMessage 15 ''AcceptedMsg


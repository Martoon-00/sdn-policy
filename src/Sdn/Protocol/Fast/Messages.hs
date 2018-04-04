{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Special messages used in Fast Paxos.
--
-- Some messages from Classic Paxos are also used, but not mentioned here.

module Sdn.Protocol.Fast.Messages
    ( ProposalMsg (..)
    , InitBallotMsg (..)
    , Phase2bMsg (..)
    ) where

import           Data.MessagePack
import qualified Data.Text.Buildable
import           Formatting                   (bprint, build, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra
import           Sdn.Protocol.Common.Messages (HasMessageShortcut (..))

-- | Message sent by proposer to acceptors to propose a new value.
newtype ProposalMsg cstruct = ProposalMsg (RawCmd cstruct)
    deriving (Generic)

instance Buildable (RawCmd cstruct) =>
         Buildable (ProposalMsg cstruct) where
    build (ProposalMsg p) = bprint ("Proposal message "%build) p

instance HasMessageShortcut (ProposalMsg cstruct) where
    messageShortcut = "rem" <> "f"

instance MessagePack (RawCmd cstruct) =>
         MessagePack (ProposalMsg cstruct)

declareMessage ''ProposalMsg


-- | Message sent by leader to acceptors to initiate a ballot.
newtype InitBallotMsg = InitBallotMsg BallotId
    deriving (Generic)

instance Buildable InitBallotMsg where
    build (InitBallotMsg b) = bprint (build%" initiation") b

instance HasMessageShortcut InitBallotMsg where
    messageShortcut = "2b" <> "f"

instance MessagePack InitBallotMsg

declareMessage ''InitBallotMsg


-- | Message sent by acceptor to learner and leader in order to make them fixate
-- new cstruct.
data Phase2bMsg cstruct = Phase2bMsg BallotId AcceptorId cstruct
    deriving (Generic)

instance Buildable cstruct =>
         Buildable (Phase2bMsg cstruct) where
    build (Phase2bMsg b a c) =
        bprint ("Phase 2b message at "%build%" from "%build%" "%build) b a c

instance HasMessageShortcut (Phase2bMsg cstruct) where
    messageShortcut = "f"

instance MessagePack cstruct => MessagePack (Phase2bMsg cstruct)

declareMessage ''Phase2bMsg


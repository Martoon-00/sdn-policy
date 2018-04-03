{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Special messages used in Fast Paxos

module Sdn.Protocol.Fast.Messages
    ( ProposalMsg (..)
    , InitBallotMsg (..)
    , Phase2bMsg (..)
    ) where

import qualified Data.Text.Buildable
import           Formatting                   (bprint, build, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra
import           Sdn.Policy.Fake
import           Sdn.Protocol.Common.Messages (HasMessageShortcut (..))

-- Some messages from Classic Paxos are also used, but not mentioned here.

newtype ProposalMsg = ProposalMsg Policy
    deriving (Generic)

instance Buildable ProposalMsg where
    build (ProposalMsg p) = bprint (" proposal message "%build) p
instance HasMessageShortcut ProposalMsg where
    messageShortcut = "rem" <> "f"

declareMessage ''ProposalMsg


newtype InitBallotMsg = InitBallotMsg BallotId
    deriving (Generic)

instance Buildable InitBallotMsg where
    build (InitBallotMsg b) = bprint (build%" initiation") b
instance HasMessageShortcut InitBallotMsg where
    messageShortcut = "2b" <> "f"

declareMessage ''InitBallotMsg


data Phase2bMsg = Phase2bMsg BallotId AcceptorId Configuration
    deriving (Generic)

instance Buildable Phase2bMsg where
    build (Phase2bMsg b a c) =
        bprint ("Phase 2b message at "%build%" from "%build%" "%build) b a c
instance HasMessageShortcut Phase2bMsg where
    messageShortcut = "f"

declareMessage ''Phase2bMsg




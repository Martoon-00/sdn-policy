{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Messages used by Classic Paxos.

module Sdn.Protocol.Classic.Messages where

import qualified Data.Text.Buildable
import           Formatting                   (bprint, build, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra
import           Sdn.Protocol.Common.Messages (HasMessageShortcut (..))

newtype ProposalMsg = ProposalMsg Policy
    deriving (Generic)

instance Buildable ProposalMsg where
    build (ProposalMsg p) = bprint ("Proposal message "%build) p
instance HasMessageShortcut ProposalMsg where
    messageShortcut = "1a"

declareMessage ''ProposalMsg


data Phase1aMsg = Phase1aMsg BallotId
    deriving (Generic)

instance Buildable Phase1aMsg where
    build (Phase1aMsg b) = bprint ("Phase 1a message "%build) b
instance HasMessageShortcut Phase1aMsg where
    messageShortcut = "1b"

declareMessage ''Phase1aMsg


data Phase1bMsg = Phase1bMsg AcceptorId BallotId Configuration
    deriving (Generic)

instance Buildable Phase1bMsg where
    build (Phase1bMsg a b c) =
        bprint ("Phase 1b message from "%build%" "%build%" "%build) a b c
instance HasMessageShortcut Phase1bMsg where
    messageShortcut = "2a"

declareMessage ''Phase1bMsg


data Phase2aMsg = Phase2aMsg BallotId Configuration
    deriving (Generic)

instance Buildable Phase2aMsg where
    build (Phase2aMsg b c) =
        bprint ("Phase 2a message "%build%" "%build) b c
instance HasMessageShortcut Phase2aMsg where
    messageShortcut = "2b"

declareMessage ''Phase2aMsg


data Phase2bMsg = Phase2bMsg AcceptorId Configuration
    deriving (Generic)

instance Buildable Phase2bMsg where
    build (Phase2bMsg a c) =
        bprint ("Phase 2b message from "%build%" "%build) a c
instance HasMessageShortcut Phase2bMsg where
    messageShortcut = mempty

declareMessage ''Phase2bMsg


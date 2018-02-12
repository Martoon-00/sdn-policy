{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Special messages used in Fast Paxos

module Sdn.Protocol.Fast.Messages where

import qualified Data.Text.Buildable
import           Formatting                   (bprint, build, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra
import           Sdn.Protocol.Common.Messages (HasMessageShortcut (..))

-- Some messages from Classic Paxos are also used

newtype ProposalFastMsg = ProposalFastMsg Policy
    deriving (Generic)

instance Buildable ProposalFastMsg where
    build (ProposalFastMsg p) = bprint ("Fast proposal message "%build) p
instance HasMessageShortcut ProposalFastMsg where
    messageShortcut = "rem" <> "f"

declareMessage ''ProposalFastMsg


newtype InitFastBallotMsg = InitFastBallotMsg BallotId
    deriving (Generic)

instance Buildable InitFastBallotMsg where
    build (InitFastBallotMsg b) = bprint (build%" initiation") b
instance HasMessageShortcut InitFastBallotMsg where
    messageShortcut = "2b" <> "f"

declareMessage ''InitFastBallotMsg


data Phase2bFastMsg = Phase2bFastMsg BallotId AcceptorId Configuration
    deriving (Generic)

instance Buildable Phase2bFastMsg where
    build (Phase2bFastMsg b a c) =
        bprint ("Phase 2b message at "%build%" from "%build%" "%build) b a c
instance HasMessageShortcut Phase2bFastMsg where
    messageShortcut = "f"

declareMessage ''Phase2bFastMsg




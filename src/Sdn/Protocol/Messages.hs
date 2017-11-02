{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Messages different processes send to each other

module Sdn.Protocol.Messages where

import           Data.MessagePack    (MessagePack)
import qualified Data.Text.Buildable
import           Formatting          (bprint, build, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra

newtype ProposalMsg = ProposalMsg Policy
    deriving (Generic)

instance Buildable ProposalMsg where
    build (ProposalMsg p) = bprint ("Proposal message "%build) p

instance MessagePack ProposalMsg
declareMessage ''ProposalMsg


data Phase1aMsg = Phase1aMsg BallotId
    deriving (Generic)

instance Buildable Phase1aMsg where
    build (Phase1aMsg b) = bprint ("Phase 1a message "%build) b

instance MessagePack Phase1aMsg
declareMessage ''Phase1aMsg


data Phase1bMsg = Phase1bMsg AcceptorId BallotId Configuration
    deriving (Generic)

instance Buildable Phase1bMsg where
    build (Phase1bMsg (AcceptorId a) b c) =
        bprint ("Phase 1b message from acceptor #"%build%" "%build%" "%build) a b c

instance MessagePack Phase1bMsg
declareMessage ''Phase1bMsg


data Phase2aMsg = Phase2aMsg BallotId Configuration
    deriving (Generic)

instance Buildable Phase2aMsg where
    build (Phase2aMsg b c) =
        bprint ("Phase 2a message "%build%" "%build) b c

instance MessagePack Phase2aMsg
declareMessage ''Phase2aMsg


data Phase2bMsg = Phase2bMsg AcceptorId Configuration
    deriving (Generic)

instance Buildable Phase2bMsg where
    build (Phase2bMsg (AcceptorId a) c) =
        bprint ("Phase 2b message from acceptor #"%build%" "%build) a c

instance MessagePack Phase2bMsg
declareMessage ''Phase2bMsg

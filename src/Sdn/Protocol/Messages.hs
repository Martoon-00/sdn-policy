{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Messages different processes send to each other

module Sdn.Protocol.Messages where

import qualified Data.Text.Buildable
import           Formatting            (bprint, build, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra
import           Sdn.Protocol.Versions

-- * Classic

newtype ProposalMsg = ProposalMsg Policy
    deriving (Generic)

instance Buildable ProposalMsg where
    build (ProposalMsg p) = bprint ("Proposal message "%build) p

declareMessage ''ProposalMsg


data Phase1aMsg pv = Phase1aMsg (BallotId pv)
    deriving (Generic)

instance ProtocolVersion pv => Buildable (Phase1aMsg pv) where
    build (Phase1aMsg b) = bprint ("Phase 1a message "%build) b

declareMessagePV ''Phase1aMsg


data Phase1bMsg pv = Phase1bMsg AcceptorId (BallotId pv) Configuration
    deriving (Generic)

instance ProtocolVersion pv => Buildable (Phase1bMsg pv) where
    build (Phase1bMsg (AcceptorId a) b c) =
        bprint ("Phase 1b message from acceptor #"%build%" "%build%" "%build) a b c

declareMessagePV ''Phase1bMsg


data Phase2aMsg pv = Phase2aMsg (BallotId pv) Configuration
    deriving (Generic)

instance ProtocolVersion pv => Buildable (Phase2aMsg pv) where
    build (Phase2aMsg b c) =
        bprint ("Phase 2a message "%build%" "%build) b c

declareMessagePV ''Phase2aMsg


data Phase2bMsg = Phase2bMsg AcceptorId Configuration
    deriving (Generic)

instance Buildable Phase2bMsg where
    build (Phase2bMsg (AcceptorId a) c) =
        bprint ("Phase 2b message from acceptor #"%build%" "%build) a c

declareMessage ''Phase2bMsg

-- * Fast

newtype ProposalFastMsg = ProposalFastMsg Policy
    deriving (Generic)

instance Buildable ProposalFastMsg where
    build (ProposalFastMsg p) = bprint ("Fast proposal message "%build) p

declareMessage ''ProposalFastMsg


newtype InitFastBallotMsg pv = InitFastBallotMsg (BallotId pv)
    deriving (Generic)

instance ProtocolVersion pv => Buildable (InitFastBallotMsg pv) where
    build (InitFastBallotMsg b) = bprint ("Fast ballot "%build%" initiation") b

declareMessagePV ''InitFastBallotMsg



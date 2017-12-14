{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Messages different processes send to each other

module Sdn.Protocol.Messages where

import qualified Data.Text.Buildable
import           Formatting          (bprint, build, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra

-- * Classic

newtype ProposalMsg = ProposalMsg Policy
    deriving (Generic)

instance Buildable ProposalMsg where
    build (ProposalMsg p) = bprint ("Proposal message "%build) p

declareMessage ''ProposalMsg


data Phase1aMsg = Phase1aMsg BallotId
    deriving (Generic)

instance Buildable Phase1aMsg where
    build (Phase1aMsg b) = bprint ("Phase 1a message "%build) b

declareMessage ''Phase1aMsg


data Phase1bMsg = Phase1bMsg AcceptorId BallotId Configuration
    deriving (Generic)

instance Buildable Phase1bMsg where
    build (Phase1bMsg a b c) =
        bprint ("Phase 1b message from "%build%" "%build%" "%build) a b c

declareMessage ''Phase1bMsg


data Phase2aMsg = Phase2aMsg BallotId Configuration
    deriving (Generic)

instance Buildable Phase2aMsg where
    build (Phase2aMsg b c) =
        bprint ("Phase 2a message "%build%" "%build) b c

declareMessage ''Phase2aMsg


data Phase2bMsg = Phase2bMsg AcceptorId Configuration
    deriving (Generic)

instance Buildable Phase2bMsg where
    build (Phase2bMsg a c) =
        bprint ("Phase 2b message from "%build%" "%build) a c

declareMessage ''Phase2bMsg

-- * Fast

newtype ProposalFastMsg = ProposalFastMsg Policy
    deriving (Generic)

instance Buildable ProposalFastMsg where
    build (ProposalFastMsg p) = bprint ("Fast proposal message "%build) p

declareMessage ''ProposalFastMsg


newtype InitFastBallotMsg = InitFastBallotMsg BallotId
    deriving (Generic)

instance Buildable InitFastBallotMsg where
    build (InitFastBallotMsg b) = bprint (build%" initiation") b

declareMessage ''InitFastBallotMsg


data Phase2bFastMsg = Phase2bFastMsg BallotId AcceptorId Configuration
    deriving (Generic)

instance Buildable Phase2bFastMsg where
    build (Phase2bFastMsg b a c) =
        bprint ("Phase 2b message at "%build%" from "%build%" "%build) b a c

declareMessage ''Phase2bFastMsg



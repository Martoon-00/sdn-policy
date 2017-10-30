{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Messages different processes send to each other

module Sdn.Messages where

import           Data.MessagePack (MessagePack)
import           Universum

import           Sdn.Policy
import           Sdn.Roles
import           Sdn.Types
import           Sdn.Util


newtype ProposalMsg = ProposalMsg Policy
    deriving (Generic)

instance MessagePack ProposalMsg
declareMessage ''ProposalMsg


data Phase1aMsg = Phase1aMsg BallotId
    deriving (Generic)

instance MessagePack Phase1aMsg
declareMessage ''Phase1aMsg


data Phase1bMsg = Phase1bMsg AcceptorId BallotId Configuration
    deriving (Generic)

instance MessagePack Phase1bMsg
declareMessage ''Phase1bMsg


data Phase2aMsg = Phase2aMsg BallotId Configuration
    deriving (Generic)

instance MessagePack Phase2aMsg
declareMessage ''Phase2aMsg


data Phase2bMsg = Phase2bMsg AcceptorId Configuration
    deriving (Generic)

instance MessagePack Phase2bMsg
declareMessage ''Phase2bMsg

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Network addresses binded to processes.

module Sdn.Roles where

import           Control.TimeWarp.Rpc (NetworkAddress, localhost)
import           Data.MessagePack     (MessagePack)
import           Universum

-- * Topology

data Members = Members
    { acceptorsNum :: Int
    , learnersNum  :: Int
    }

leaderAddress :: NetworkAddress
leaderAddress = (localhost, 5000)

leaderAddresses :: [NetworkAddress]
leaderAddresses = one leaderAddress

acceptorsAddresses :: Members -> [NetworkAddress]
acceptorsAddresses members =
    [1 .. acceptorsNum members] <&> \i -> (localhost, 6000 + i)

learnersAddresses :: Members -> [NetworkAddress]
learnersAddresses members =
    [1 .. learnersNum members] <&> \i -> (localhost, 7000 + i)

-- * Ids

newtype AcceptorId = AcceptorId Int
    deriving (Eq, Ord, Show, Num, MessagePack)

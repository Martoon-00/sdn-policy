{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE ImplicitParams  #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Various protocol-wide settings.

module Sdn.Base.Settings where

import           Control.Lens         (makePrisms)
import           Control.TimeWarp.Rpc (NetworkAddress, Port, localhost)
import           Data.Default         (Default (..))
import           Data.Reflection
import qualified Data.Text.Buildable
import           Formatting           (bprint, build, (%))
import           Test.QuickCheck      (Arbitrary (..), getPositive)
import           Universum

import           Sdn.Base.Types

-- | Information about number of consensus participants.
data Members = Members
    { acceptorsNum :: Int
    , learnersNum  :: Int
    } deriving (Show, Generic)

instance Default Members where
    def = Members
        { acceptorsNum = 3
        , learnersNum = 1
        }

instance Buildable Members where
    build Members{..} =
        bprint ( "    acceptors: "%build%
               "\n    learners: "%build)
            acceptorsNum
            learnersNum

instance Arbitrary Members where
    arbitrary =
        Members
            <$> (getPositive <$> arbitrary)
            <*> (getPositive <$> arbitrary)

type HasMembers = Given Members

withMembers :: Members -> (HasMembers => a) -> a
withMembers = give

getMembers :: HasMembers => Members
getMembers = given


data ProposerAddrInfo a
    = ProposerAddrInfoFixed a
    | ProposerAddrInfoEvaled (ProcessId ProposerTag -> a)
    deriving (Functor)

makePrisms ''ProposerAddrInfo

-- | Addresses of members of protocol.
data MembersAddrInfo a = MembersAddrInfo
    { proposerAddrInfo   :: ProposerAddrInfo a  -- proposer may wish to left anonymous
    , leaderAddrInfo     :: a
    , acceptorsAddrInfos :: Int -> a
    , learnersAddrInfos  :: Int -> a
    } deriving (Functor)

type MembersAddresses = MembersAddrInfo NetworkAddress
type MembersPorts = MembersAddrInfo Port

instance Default MembersPorts where
    def =
        MembersAddrInfo
        { proposerAddrInfo = ProposerAddrInfoFixed 4000
        , leaderAddrInfo = 5000
        , acceptorsAddrInfos = (+ 6000)
        , learnersAddrInfos = (+ 7000)
        }

instance Default MembersAddresses where
    def = (localhost, ) <$> def


type HasMembersAddresses = Given MembersAddresses

withMembersAddresses :: MembersAddresses -> (HasMembersAddresses => a) -> a
withMembersAddresses = give

getMembersAddresses :: HasMembersAddresses => MembersAddresses
getMembersAddresses = given


-- | Carrying all information about members.
type HasMembersInfo =
    ( HasMembers
    , HasMembersAddresses
    )

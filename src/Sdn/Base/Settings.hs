{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE Rank2Types     #-}

-- | Various protocol-wide settings.

module Sdn.Base.Settings where

import           Data.Default        (Default (..))
import           Data.Reflection
import qualified Data.Text.Buildable
import           Formatting          (bprint, build, (%))
import           Test.QuickCheck     (Arbitrary (..), getPositive)
import           Universum

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

{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE Rank2Types     #-}

-- | Various protocol-wide settings.

module Sdn.Base.Settings where

import           Data.Default    (Default (..))
import           Data.Reflection
import           Test.QuickCheck (Arbitrary (..), CoArbitrary (..), getPositive, resize)
import           Universum

-- | Information about number of consensus participants.
data Members = Members
    { acceptorsNum :: Int
    , learnersNum  :: Int
    } deriving (Show)

instance Default Members where
    def = Members
        { acceptorsNum = 3
        , learnersNum = 1
        }

instance Arbitrary Members where
    arbitrary =
        Members
            <$> (getPositive <$> arbitrary)
            <*> (getPositive <$> arbitrary)

instance CoArbitrary Members where
    -- this is not very beautiful, but I'd hardly ever use more than one learner
    coarbitrary Members{..} = resize acceptorsNum


type HasMembers = Given Members

withMembers :: Members -> (HasMembers => a) -> a
withMembers = give

getMembers :: HasMembers => Members
getMembers = given

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Quorum-related stuff

module Sdn.Base.Quorum where

import           Control.Lens        (At (..), Index, IxValue, Ixed (..), Wrapped (..),
                                      iso)
import           Data.List           (subsequences)
import qualified Data.Map            as M
import           Data.Reflection     (Reifies (..))
import qualified Data.Text.Buildable
import           Formatting          (bprint)
import           GHC.Exts            (IsList (..))
import           Test.QuickCheck     (Arbitrary (..), sublistOf)
import           Universum           hiding (toList)
import qualified Universum           as U

import           Sdn.Base.Types
import           Sdn.Extra.Util

-- | For each acceptor - something proposed by him.
-- Phantom type @f@ stands for used quorum family, and decides whether given
-- number of votes forms quorum.
newtype Votes f a = Votes (M.Map AcceptorId a)
    deriving (Eq, Ord, Show, Monoid, Container, NontrivialContainer)

instance Wrapped (Votes t a) where
    type Unwrapped (Votes t a) = M.Map AcceptorId a
    _Wrapped' = iso (\(Votes v) -> v) Votes

instance IsList (Votes f a) where
    type Item (Votes f a) = (AcceptorId, a)
    toList (Votes v) = toList v
    fromList = Votes . fromList

type instance Index (Votes t a) = AcceptorId
type instance IxValue (Votes t a) = a

instance Ixed (Votes t a) where
    ix index = _Wrapped' . ix index

instance At (Votes t a) where
    at index = _Wrapped' . at index

instance Buildable a => Buildable (Votes t a) where
    build (Votes v) = bprint (buildList ", ") $ U.toList v

-- | All functions which work with 'Votes' can also be applied to
-- set of acceptors.
type AcceptorsSet f = Votes f ()

instance Arbitrary (AcceptorsSet f) where
    arbitrary =
        arbitrary >>= \n ->
            fmap fromList $ sublistOf $ map (, ()) $ map AcceptorId [1 .. n]

-- | Sometimes we don't care about used quorum family.
data UnknownQuorum

-- | Describes quorum family.
class QuorumFamily f where
    -- | Check whether votes belong to a quorum of acceptors.
    isQuorum :: Members -> Votes f a -> Bool

-- | Check whether votes belogs to a minimum for inclusion quorum.
isMinQuorum :: QuorumFamily f => Members -> Votes f a -> Bool
isMinQuorum members votes =
    let subVotes = votes & _Wrapped' . listL %~ drop 1
    in  isQuorum members votes && not (isQuorum members subVotes)

-- | Simple majority quorum.
data MajorityQuorum frac

instance Reifies frac Rational => QuorumFamily (MajorityQuorum frac) where
    isQuorum Members{..} votes =
        let frac = reflect @frac Proxy
        in  fromIntegral (length votes) > fromIntegral acceptorsNum * frac

data OneHalf
instance Reifies OneHalf Rational where
    reflect _ = 1/2

data ThreeQuarters
instance Reifies ThreeQuarters Rational where
    reflect _ = 3/4

type ClassicMajorityQuorum = MajorityQuorum OneHalf
type FastMajorityQuorum = MajorityQuorum ThreeQuarters

-- | Take all possible minimum for inclusion quorums.
allMinQuorums
    :: QuorumFamily f
    => Members -> Votes f a -> [Votes f a]
allMinQuorums members (Votes votes) =
    filter (isMinQuorum members) $
    map (Votes . M.fromList) $ subsequences $ M.toList votes

-- | Add vote to votes.
addVote :: AcceptorId -> a -> Votes f a -> Votes f a
addVote aid a (Votes m) = Votes $ M.insert aid a m

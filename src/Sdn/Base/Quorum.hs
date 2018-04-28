{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Quorum-related stuff

module Sdn.Base.Quorum where

import           Control.Lens        (At (..), Index, Iso', IxValue, Ixed (..),
                                      Wrapped (..), mapMOf, traverseOf)
import           Data.List           (groupBy, subsequences)
import qualified Data.Map            as M
import           Data.Reflection     (Reifies (..))
import qualified Data.Text.Buildable
import           Formatting          (bprint, build)
import           GHC.Exts            (IsList (..))
import           Test.QuickCheck     (Arbitrary (..), Gen, sublistOf)
import           Universum           hiding (toList)
import qualified Universum           as U
import           Unsafe              (unsafeFromJust, unsafeHead)

import           Sdn.Base.Settings
import           Sdn.Base.Types
import           Sdn.Extra.Util

-- | For each acceptor - something proposed by him.
-- Phantom type @f@ stands for used quorum family, and decides whether given
-- number of votes forms quorum.
newtype Votes f a = Votes { getVotes :: M.Map AcceptorId a }
    deriving (Eq, Ord, Show, Monoid, Functor, Foldable, Traversable,
              Container, NontrivialContainer, Generic)

instance Wrapped (Votes t a)

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

instance One (Votes f a) where
    type OneItem (Votes f a) = (AcceptorId, a)
    one = Votes . one

votesL :: Iso' (Votes f a) [(AcceptorId, a)]
votesL = _Wrapped' . listL

instance Buildable a => Buildable (Votes t a) where
    build (Votes v) = bprint (listF ", " build) $ U.toList v

instance (HasMembers, a ~ ()) => Bounded (Votes f a) where
    minBound = fromList []
    maxBound = fromList
        [ (fromIntegral accId, ())
        | accId <- [1 .. acceptorsNum getMembers]]

-- | All functions which work with 'Votes' can also be applied to
-- set of acceptors.
type AcceptorsSet f = Votes f ()

-- | Generates arbitrary votes using given value generator.
genVotes :: HasMembers => Gen a -> Gen (Votes f a)
genVotes genValue = do
    votes <- traverse (const genValue) maxBound
    mapMOf listL sublistOf votes
  where
    Members{..} = getMembers

instance (HasMembers, Arbitrary a) => Arbitrary (Votes f a) where
    arbitrary = genVotes arbitrary

-- | Sometimes we don't care about used quorum family.
data UnknownQuorum

-- | Change type family.
coerceVotes :: forall f' f a. Votes f a -> Votes f' a
coerceVotes (Votes v) = Votes v

-- | Remains votes for values which comply the predicate.
filterVotes :: (a -> Bool) -> Votes f a -> Votes f a
filterVotes p = listL %~ filter (p . snd)

-- | Remain only 'Just' values.
catMaybesVotes :: Votes f (Maybe a) -> Votes f a
catMaybesVotes = fmap unsafeFromJust . filterVotes isJust

-- | Add vote to votes.
addVote :: AcceptorId -> a -> Votes f a -> Votes f a
addVote aid a (Votes m) = Votes $ M.insert aid a m

-- | Intersection of two votes.
votesIntersection :: Votes f a -> Votes f () -> Votes f a
votesIntersection (Votes v1) (Votes v2) = Votes $ M.intersection v1 v2

votesNoDups :: [Votes f a] -> [Votes f a]
votesNoDups = map unsafeHead . groupBy ((==) `on` toKey) . sortWith toKey
  where
    toKey = M.keysSet . view _Wrapped'

allSubVotes :: Votes f a -> [Votes f a]
allSubVotes = traverseOf votesL subsequences

transposeVotes :: Ord a => Votes f a -> Map a (Votes f ())
transposeVotes (Votes votes) =
    M.fromListWith mappend $ map trans $ M.toList votes
  where
    trans (accId, v) = (v, one (accId, ()))

-- | Describes quorum family.
class QuorumFamily f where
    -- | Check whether votes belong to a quorum of acceptors.
    isQuorum :: HasMembers => Votes f a -> Bool

    -- | Check whether votes belogs to a minimum for inclusion quorum.
    isMinQuorum :: HasMembers => Votes f a -> Bool

    -- | Check whether not mentioned members can not form a quorum.
    excludesOtherQuorum :: HasMembers => Votes f a -> Bool

-- | Take all possible minimum for inclusion quorums from given votes.
allMinQuorumsOf
    :: (HasMembers, QuorumFamily f)
     => Votes f a -> [Votes f a]
allMinQuorumsOf = filter isMinQuorum . traverseOf votesL subsequences

-- | Take all possible quorums from given votes.
allQuorumsOf
    :: (HasMembers, QuorumFamily f)
     => Votes f a -> [Votes f a]
allQuorumsOf = filter isQuorum . allSubVotes

-- | Get all possible quorums.
allQuorums
    :: (HasMembers, QuorumFamily f)
    => [Votes f ()]
allQuorums = allQuorumsOf maxBound

-- * Quorum instances

-- | Simple majority quorum.
data MajorityQuorum frac

majorityQuorumSize :: forall frac. (HasMembers, Reifies frac Rational) => Int
majorityQuorumSize =
    let frac = reflect @frac Proxy
        Members {..} = getMembers
    in  floor $ fromIntegral acceptorsNum * frac + 1

instance Reifies frac Rational =>
         QuorumFamily (MajorityQuorum frac) where
    isQuorum votes = length votes >= majorityQuorumSize @frac

    isMinQuorum votes = length votes == majorityQuorumSize @frac

    excludesOtherQuorum votes =
        let freeAcceptors = acceptorsNum getMembers - length votes
        in  freeAcceptors < majorityQuorumSize @frac

data OneHalf
instance Reifies OneHalf Rational where
    reflect _ = 1/2

data ThreeQuarters
instance Reifies ThreeQuarters Rational where
    reflect _ = 3/4

type ClassicMajorityQuorum = MajorityQuorum OneHalf
type FastMajorityQuorum = MajorityQuorum ThreeQuarters


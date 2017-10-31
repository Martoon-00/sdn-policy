{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Quorum-related stuff

module Sdn.Quorum where

import           Control.Lens (At (..), Index, IxValue, Ixed (..), Wrapped (..), iso)
import           Data.List    (subsequences)
import qualified Data.Map     as M
import           Universum

import           Sdn.Types

-- | For each acceptor - something proposed by him.
newtype Votes a = Votes (M.Map AcceptorId a)
    deriving (Eq, Ord, Monoid, Container)

instance Wrapped (Votes a) where
    type Unwrapped (Votes a) = M.Map AcceptorId a
    _Wrapped' = iso (\(Votes v) -> v) Votes

type instance Index (Votes a) = AcceptorId
type instance IxValue (Votes a) = a

instance Ixed (Votes a) where
    ix index = _Wrapped' . ix index

instance At (Votes a) where
    at index = _Wrapped' . at index

-- | All functions which work with 'Votes' can also be applied to
-- set of acceptors.
type AcceptorsSet = Votes ()

-- | Check whether votes belong to a quorum of acceptors.
-- Here and further we assume that simple majority quorum is used.
isQuorum :: Members -> Votes a -> Bool
isQuorum Members{..} (Votes votes) = M.size votes * 2 > acceptorsNum

-- | Check whether votes belogs to a minimum for inclusion quorum.
isMinQuorum :: Members -> Votes a -> Bool
isMinQuorum Members{..} (Votes votes) =
    (acceptorsNum + 1) `div` 2 == M.size votes

-- | Take all possible minimum for inclusion quorums.
allMinQuorums :: Members -> Votes a -> [Votes a]
allMinQuorums members (Votes votes) =
    filter (isMinQuorum members) $
    map (Votes . M.fromList) $ subsequences $ M.toList votes

-- | Add vote to votes.
addVote :: AcceptorId -> a -> Votes a -> Votes a
addVote aid a (Votes m) = Votes $ M.insert aid a m

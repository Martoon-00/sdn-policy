{-# LANGUAGE TemplateHaskell #-}

-- | Various contexts of processes

module Sdn.Context where

import           Control.Lens (makeLenses)
import           Universum

import           Sdn.Policy   (PoliciesHeap)
import           Sdn.Types    (BallotId)


class HasLens s a where
    lensOf :: Lens' s a

accessing
    :: forall a s m.
       (MonadIO m, MonadReader (TVar s) m, HasLens s a)
    => (a -> a) -> m a
accessing modifier = do
    var <- ask
    liftIO . atomically $ do
        modifyTVar' var (lensOf %~ modifier)
        view lensOf <$> readTVar var


data LeaderContext = LeaderContext
    { _lcBallot  :: BallotId
    , _lcCstruct :: PoliciesHeap
    }

makeLenses ''LeaderContext

instance HasLens LeaderContext BallotId where
    lensOf = lcBallot
instance HasLens LeaderContext PoliciesHeap where
    lensOf = lcCstruct


data AcceptorContext = AcceptorContext
    { _acBallot  :: BallotId
    , _acCstruct :: PoliciesHeap
    }

makeLenses ''AcceptorContext

instance HasLens AcceptorContext BallotId where
    lensOf = acBallot
instance HasLens AcceptorContext PoliciesHeap where
    lensOf = acCstruct


data LearnerContext = LearnerContext
    { _lrcCstruct :: PoliciesHeap
    }

makeLenses ''LearnerContext

instance HasLens LearnerContext PoliciesHeap where
    lensOf = lrcCstruct

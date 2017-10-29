{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Some useful types.

module Sdn.ProposalStrategy
    ( GenSeed (..)
    , ProposalStrategy
    , mkStrategy

    -- * strategies
    , generating
    , simple

    -- * strategy combinators
    , limited
    , delayed
    ) where

import           Control.TimeWarp.Timed (MonadTimed, after, for, fork_, invoke, schedule,
                                         wait)
import           Data.Default           (Default (..))
import           Data.Time.Units        (Microsecond)
import           System.Random          (StdGen, mkStdGen, next, randomIO, split)
import           Test.QuickCheck.Gen    (Gen, unGen)
import           Test.QuickCheck.Random (mkQCGen)
import           Universum

newtype WhetherContinue = WhetherContinue Bool

instance Default WhetherContinue where
    def = WhetherContinue True


type MonadProposal m =
    ( MonadIO m
    , MonadTimed m
    )

data ProposalContext m p = ProposalContext
    { pcPush :: p -> m ()
    , pcCont :: TVar WhetherContinue
    , pcGen  :: StdGen
    }

newtype ProposalStrategy p =
    ProposalStrategy
    (forall m. MonadProposal m => ProposalContext m p -> m ())

data GenSeed
    = RandomSeed
    | FixedSeed Int

getGenSeed :: MonadIO m => GenSeed -> m Int
getGenSeed = \case
    RandomSeed -> liftIO randomIO
    FixedSeed s -> pure s

mkStrategy
    :: MonadProposal m
    => GenSeed -> ProposalStrategy p -> (p -> m ()) -> m ()
mkStrategy seed (ProposalStrategy strategy) consumer = do
    let pcPush = consumer
    pcGen <- mkStdGen <$> getGenSeed seed
    pcCont <- newTVarIO def
    strategy ProposalContext{..}

instance Monoid (ProposalStrategy p) where
    mempty = ProposalStrategy $ \_ -> pass
    ProposalStrategy strategy1 `mappend` ProposalStrategy strategy2 =
        ProposalStrategy $ \ctx -> do
            let (gen1, gen2) = split (pcGen ctx)
            fork_ $ strategy1 ctx{ pcGen = gen1 }
            fork_ $ strategy2 ctx{ pcGen = gen2 }


generating :: Microsecond -> Gen p -> ProposalStrategy p
generating period generator = do
    ProposalStrategy $ \ProposalContext{..} ->
        let loop gen = do
                wait (for period)
                WhetherContinue further <- readTVarIO pcCont
                when further $ do
                    let (seed, gen') = next gen
                    let value = unGen generator (mkQCGen seed) 30
                    pcPush value
                    loop gen'
        in  fork_ $ loop pcGen

simple :: Microsecond -> p -> ProposalStrategy p
simple period policy = generating period (pure policy)


limited :: Microsecond -> ProposalStrategy p -> ProposalStrategy p
limited duration (ProposalStrategy strategy) =
    ProposalStrategy $ \ctx -> do
        schedule (after duration) $
            atomically $ writeTVar (pcCont ctx) (WhetherContinue False)
        strategy ctx

delayed :: Microsecond -> ProposalStrategy p -> ProposalStrategy p
delayed duration (ProposalStrategy strategy) =
    ProposalStrategy $ invoke (after duration) . strategy

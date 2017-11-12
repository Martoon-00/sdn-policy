{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Allows to specify schedules in convenient way.

module Sdn.Schedule
    ( Schedule
    , MonadSchedule
    , runSchedule

    , GenSeed (..)
    , splitGenSeed

    -- * schedules
    , generating
    , simple
    , execute

    -- * schedule combinators
    , periodic
    , repeating
    , limited
    , delayed
    ) where

import           Control.Lens           (both)
import           Control.TimeWarp.Timed (MonadTimed, after, for, fork_, invoke, wait)
import           Data.Default           (Default (..))
import           Data.Time.Units        (Microsecond)
import           System.Random          (StdGen, mkStdGen, next, randomIO, split)
import           Test.QuickCheck.Gen    (Gen, unGen)
import           Test.QuickCheck.Random (mkQCGen)
import           Universum

-- | Whether executing job should be continued.
newtype WhetherContinue = WhetherContinue Bool

instance Default WhetherContinue where
    def = WhetherContinue True

-- | Constraints required for executing schedules.
type MonadSchedule m =
    ( MonadIO m
    , MonadTimed m
    )

data ScheduleContext m p = ScheduleContext
    { scPush :: p -> m ()
    , scCont :: TVar WhetherContinue
    , scGen  :: StdGen
    }

-- | Schedule allows to periodically execute some job,
-- providing it with data @p@ which may vary from time to time.
newtype Schedule p = Schedule
    (forall m. MonadSchedule m => ScheduleContext m p -> m ())

-- | Which seed to use for randomness.
data GenSeed
    = RandomSeed        -- ^ IO-provided seed
    | FixedSeed StdGen  -- ^ Specified seed

getGenSeed :: MonadIO m => GenSeed -> m StdGen
getGenSeed = \case
    RandomSeed -> mkStdGen <$> liftIO randomIO
    FixedSeed s -> pure s

-- | 'GenSeed' is not @instance RandomGen@, so custom split is needed.
splitGenSeed :: GenSeed -> (GenSeed, GenSeed)
splitGenSeed RandomSeed       = (RandomSeed, RandomSeed)
splitGenSeed (FixedSeed seed) = both %~ FixedSeed $ split seed

-- | Execute given job on schedule.
runSchedule
    :: MonadSchedule m
    => GenSeed -> Schedule p -> (p -> m ()) -> m ()
runSchedule seed (Schedule schedule) consumer = do
    let scPush = consumer
    scGen <- getGenSeed seed
    scCont <- newTVarIO def
    schedule ScheduleContext{..}

-- | Allows to execute schedules in parallel.
instance Monoid (Schedule p) where
    mempty = Schedule $ \_ -> pass
    Schedule strategy1 `mappend` Schedule strategy2 =
        Schedule $ \ctx -> do
            let (gen1, gen2) = split (scGen ctx)
            fork_ $ strategy1 ctx{ scGen = gen1 }
            fork_ $ strategy2 ctx{ scGen = gen2 }

instance Functor Schedule where
    fmap f (Schedule s) = Schedule $ \ctx ->
        s ctx{ scPush = scPush ctx . f }

instance Applicative Schedule where
    pure = return
    (<*>) = ap

instance Monad Schedule where
    return = simple
    Schedule s1 >>= f = Schedule $ \ctx ->
        let (gen1, gen2) = split (scGen ctx)
            push p = case f p of
                     Schedule s2 -> s2 ctx{ scGen = gen1 }
        in  s1 ctx{ scPush = push, scGen = gen2 }

-- | Just fires once, generating arbitrary job data.
--
-- Use combinators to define timing.
generating :: Gen p -> Schedule p
generating generator = do
    Schedule $ \ScheduleContext{..} ->
        let (seed, _) = next scGen
        in  scPush $ unGen generator (mkQCGen seed) 30

-- | Just fires once, using provided job data.
simple :: p -> Schedule p
simple = generating . pure

-- | Just fires once, for jobs without any data.
execute :: Schedule ()
execute = simple ()

periodicCounting :: Maybe Word -> Microsecond -> Schedule p -> Schedule p
periodicCounting mnum period (Schedule schedule) =
    Schedule $ \ctx@ScheduleContext{..} ->
        let loop (Just 0) _ = return ()
            loop mrem gen = do
                WhetherContinue further <- readTVarIO scCont
                when further $ do
                    let (gen1, gen2) = split gen
                        mrem' = fmap pred mrem
                    schedule ctx{ scGen = gen1 }
                    wait (for period)
                    loop mrem' gen2
        in  fork_ $ loop mnum scGen

-- | Execute with given period.
periodic :: Microsecond -> Schedule p -> Schedule p
periodic = periodicCounting Nothing

-- | Execute given number of times with specified delay.
repeating :: Word -> Microsecond -> Schedule p -> Schedule p
repeating = periodicCounting . Just

-- | Stop starting jobs after given amount of time.
limited :: Microsecond -> Schedule p -> Schedule p
limited duration (Schedule schedule) =
    Schedule $ \ctx -> do
        fork_ $ do
            wait (for duration)
            atomically $ writeTVar (scCont ctx) (WhetherContinue False)
        schedule ctx

-- | Postpone execution.
delayed :: Microsecond -> Schedule p -> Schedule p
delayed duration (Schedule schedule) =
    Schedule $ invoke (after duration) . schedule

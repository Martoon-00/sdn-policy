{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Allows to specify schedules in convenient way.

module Sdn.Extra.Schedule
    ( Schedule
    , MonadSchedule
    , runSchedule
    , runSchedule_

    , GenSeed (..)
    , getGenSeed
    , splitGenSeed

    -- * schedules
    , wrapped
    , generate
    , execute

    -- * schedule combinators
    , periodic
    , repeating
    , repeatingWhile
    , repeatingUntil
    , times
    , executeWhile
    , limited
    , delayed
    , maskExecutions
    ) where

import           Control.Lens           (both, ix)
import           Control.TimeWarp.Timed (MonadTimed, after, for, fork_, invoke,
                                         virtualTime, wait)
import           Data.Time.Units        (Microsecond)
import           System.Random          (StdGen, mkStdGen, next, randomIO, split)
import           Test.QuickCheck.Gen    (Gen, unGen)
import           Test.QuickCheck.Random (mkQCGen)
import           Universum

import           Sdn.Extra.Util         (atomicModifyIORefS)

-- | Whether executing job should be continued.
newtype WhetherContinue = WhetherContinue Bool

instance Monoid WhetherContinue where
    mempty = WhetherContinue True
    WhetherContinue b1 `mappend` WhetherContinue b2 = WhetherContinue (b1 && b2)

-- | Constraints required for executing schedules.
type MonadSchedule m =
    ( MonadIO m
    , MonadTimed m
    )

data ScheduleContext m p = ScheduleContext
    { scNext :: p -> m ()
    , scCont :: m WhetherContinue
    , scGen  :: StdGen
    }

scYield :: Monad m => ScheduleContext m p -> p -> m ()
scYield ScheduleContext{..} p = do
    WhetherContinue further <- scCont
    when further $ scNext p

-- | Schedule allows to periodically execute some job,
-- providing it with data @p@ which may vary from time to time.
newtype Schedule m p = Schedule (ScheduleContext m p -> m ())

-- | Which seed to use for randomness.
data GenSeed
    = RandomSeed        -- ^ IO-provided seed
    | FixedSeed StdGen  -- ^ Specified seed

getGenSeed :: MonadIO m => GenSeed -> m StdGen
getGenSeed = \case
    RandomSeed -> mkStdGen <$> liftIO randomIO
    FixedSeed s -> pure s

splitGenSeed :: GenSeed -> (GenSeed, GenSeed)
splitGenSeed RandomSeed       = (RandomSeed, RandomSeed)
splitGenSeed (FixedSeed seed) = both %~ FixedSeed $ split seed

-- | Execute given job on schedule.
runSchedule
    :: MonadSchedule m
    => StdGen -> Schedule m p -> (p -> m ()) -> m ()
runSchedule scGen (Schedule schedule) consumer = do
    let scNext = consumer
    let scCont = pure mempty
    fork_ $ schedule ScheduleContext{..}

-- | Execute schedule without any job passed.
runSchedule_
    :: MonadSchedule m
    => StdGen -> Schedule m () -> m ()
runSchedule_ seed schedule = runSchedule seed schedule $ \() -> pass


-- | Allows to execute schedules in parallel.
-- I prefered to have this logic in 'Monoid' rather than
-- in 'Alternative', because it's more convenient to use.
instance MonadTimed m => Monoid (Schedule m p) where
    mempty = Schedule $ \_ -> pass
    Schedule strategy1 `mappend` Schedule strategy2 =
        Schedule $ \ctx -> do
            let (gen1, gen2) = split (scGen ctx)
            fork_ $ strategy1 ctx{ scGen = gen1 }
            fork_ $ strategy2 ctx{ scGen = gen2 }

instance Functor (Schedule m) where
    fmap f (Schedule s) = Schedule $ \ctx ->
        s ctx{ scNext = scNext ctx . f }

instance MonadIO m => Applicative (Schedule m) where
    pure = return
    (<*>) = ap

instance MonadIO m => Monad (Schedule m) where
    return = generate . pure
    Schedule s1 >>= f = Schedule $ \ctx -> do
        let (gen1, gen2) = split (scGen ctx)
        genBox <- newIORef gen1
        let push p = do
               gen' <- atomicModifyIORef genBox split
               case f p of Schedule s2 -> s2 ctx{ scGen = gen' }
        s1 ctx{ scNext = push, scGen = gen2 }

instance MonadIO m => MonadIO (Schedule m) where
    liftIO = lift . liftIO

instance MonadTrans Schedule where
    lift job = Schedule $ \ctx -> job >>= scYield ctx


-- | Manually dump items.
wrapped :: Monad m => ((a -> m ()) -> m ()) -> Schedule m a
wrapped pusher = Schedule $ \ctx ->
    pusher $ \val -> scNext ctx val

-- | Just fires once, generating arbitrary job data.
--
-- Use combinators to define timing.
generate :: MonadIO m => Gen p -> Schedule m p
generate generator = do
    Schedule $ \ctx@ScheduleContext{..} -> do
        let (seed, _) = next scGen
        scYield ctx $ unGen generator (mkQCGen seed) 100

-- | Just fires once, for jobs without any data.
-- Synonym to @return ()@.
execute :: MonadIO m => Schedule m ()
execute = pass

-- | Execute action till condition holds, no more than given number of times,
-- with given period.
-- Action is not executed immediatelly, rather delay is awaited first.
-- Use @execute <> periodicCounting ...@ to execute action immediatelly as well.
periodicCounting
    :: MonadSchedule m
    => Maybe Word -> m Bool -> Microsecond -> Schedule m ()
periodicCounting mnum condM period =
    Schedule $ \ctx@ScheduleContext{..} ->
        let loop (Just 0) _ = return ()
            loop mrem gen = do
                further <- condM
                when further $ do
                    let mrem' = fmap pred mrem
                    scYield ctx ()
                    wait (for period)
                    loop mrem' gen
        in  fork_ $ loop mnum scGen

-- | Execute with given period indefinetely.
periodic
    :: MonadSchedule m
    => Microsecond -> Schedule m ()
periodic = periodicCounting Nothing (pure True)

-- | Execute given number of times with specified delay.
repeating
    :: MonadSchedule m
    => Word -> Microsecond -> Schedule m ()
repeating num = periodicCounting (Just num) (pure True)

-- | Execute with given delay while condition holds.
repeatingWhile :: MonadSchedule m => m Bool -> Microsecond -> Schedule m ()
repeatingWhile condM = periodicCounting Nothing condM

-- | Execute with given delay while condition doesn't hold.
repeatingUntil :: MonadSchedule m => m Bool -> Microsecond -> Schedule m ()
repeatingUntil condM = repeatingWhile (not <$> condM)

  -- | Perform schedule several times at once.
times :: MonadSchedule m => Word -> Schedule m ()
times k = repeating k 0

-- | Execute given schedule while continue holds.
executeWhile
    :: MonadSchedule m
    => m Bool -> Schedule m p -> Schedule m p
executeWhile condM (Schedule schedule) =
    Schedule $ \ctx ->
        schedule ctx{ scCont = (<>) <$> scCont ctx <*> (WhetherContinue <$> condM) }

-- | Stop starting jobs after given amount of time.
limited
    :: MonadSchedule m
    => Microsecond -> Schedule m p -> Schedule m p
limited duration schedule = do
    start <- lift virtualTime
    executeWhile (virtualTime <&> ( < start + duration)) schedule

-- | Postpone execution.
delayed
    :: MonadSchedule m
    => Microsecond -> Schedule m ()
delayed duration =
    Schedule $ \ctx ->
        invoke (after duration) $ scYield ctx ()

-- | Skip several executions. More preciesly
--
-- * 'True' corresponds to performing execution;
--
-- * 'False' - to skipping execution;
--
-- * End of list - to skipping all further executions.
maskExecutions :: (MonadSchedule m, MonadIO n) => [Bool] -> n (Schedule m ())
maskExecutions initMask = do
    st <- newIORef initMask
    return $ Schedule $ \ctx -> do
        whetherExecute <- atomicModifyIORefS st $ do
            m <- preuse $ ix 0
            modify $ drop 1
            return $ fromMaybe False m
        when whetherExecute $
            scYield ctx ()

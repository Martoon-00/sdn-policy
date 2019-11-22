{-# LANGUAGE EmptyCase                 #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Benchmark indented for a whole cluster performance measuring.

module Main where

import           Control.TimeWarp.Timed       (Microsecond, for, fork_, ms, runTimedIO, sec,
                                               sleepForever, wait, work)
import qualified Data.Time.Clock.POSIX        as Time
import           Test.QuickCheck              (generate)
import           Universum

import           Sdn.Base
import           Sdn.Extra.Util

import           Options
import           Sdn.Policy.OpenFlow
import           Sdn.Policy.PseudoConflicting
import           Sdn.Protocol.Node

type ConflictsFrac = TimeLimitMillis 50 (1 % 30)
type Policy' = PseudoConflicting ConflictsFrac Policy
type Configuration' = SimpleConfig Policy'

main :: IO ()
main = do
    BenchControllerOptions{..} <- getBenchControllerOptions

    proposedCounter <- newIORef (0 :: Int)
    installedCounter <- newIORef (0 :: Int)
    rejectedCounter <- newIORef (0 :: Int)

    protocolHandlers <-
        runProtocolNode
            @Configuration'
            protocolOptions curProcessId
            (protocolCallbacks installedCounter rejectedCounter)

    -- TODO:
    -- For time measuring - 1 second warmup and then count, printing time and
    -- configuration size each second.
    -- Define conflict relation so that wildcards work too

    mStartDelay <- forM mStartTime $ \startTime -> do
        time <- round @_ @Microsecond . (* 1000000) <$> Time.getPOSIXTime
        return (startTime - time)

    runTimedIO $ do
        -- give some time for server to be brought up
        wait (for 20 ms)
        putStrLn @Text "Consensus protocol initiated"

        case mStartDelay of
          Just startDelay -> do
            putStrLn @Text "Waiting for start"
            wait (for startDelay)
          Nothing ->
            wait (for proposalsStartDelay)
        putStrLn @Text "Proposing policies"

        fork_ . forever $ do
            proposed <- readIORef proposedCounter
            putText $ "Proposed " <> show proposed <> " policies"
            installed <- readIORef installedCounter
            rejected <- readIORef rejectedCounter
            let accepted = installed - rejected
            let acceptancePercent =
                  (round @Double @Int (fromIntegral accepted * 10000 / fromIntegral installed) `div` 100)
            putText $ "Installed " <> show installed <> " policies \
                      \(rejected " <> show rejected <>
                      " / accepted " <> show accepted <> " (" <> show acceptancePercent <> "%))"
            wait (for 1 sec)

        fork_ . work (for 10 sec) $
            submitEvenlyExact proposalsDelay $ \pretendedTime -> do
                liftIO $ do
                  policy <- PseudoConflicting @ConflictsFrac
                        <$> generate (genPolicy curProcessId pretendedTime)

                  -- putText $ "Proposing " <> show policy
                  protocolMakeProposal protocolHandlers policy
                  atomicModifyIORef' proposedCounter (\c -> (c + 1, ()))

    runTimedIO sleepForever
  where
    protocolCallbacks installedCounter rejectedCounter =
        ProtocolCallbacks
        { protocolOnLearned = \policyAccs -> do
              atomicModifyIORef' installedCounter (\c -> (c + length policyAccs, ()))

              forM_ policyAccs $ \policyAcc -> case policyAcc of
                  Rejected _ -> do
                    -- putText $ "Policy rejected " <> show p
                    atomicModifyIORef' rejectedCounter (\c -> (c + 1, ()))
                  Accepted _ -> pass

              -- putText $ "Learned " <> show _policyAccs
        }

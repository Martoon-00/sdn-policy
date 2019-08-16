{-# LANGUAGE EmptyCase                 #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Benchmark indented for a whole cluster performance measuring.

module Main where

import           Control.TimeWarp.Timed       (for, fork_, ms, runTimedIO, sec, sleepForever, wait,
                                               work)
import           Data.Coerce                  (coerce)
import qualified Network.Data.OpenFlow        as OF
import           Test.QuickCheck              (Gen, arbitrary, choose, generate)
import           Universum

import           Sdn.Base
import           Sdn.Extra.Util

import           Options
import           Sdn.Policy.OpenFlow
import           Sdn.Policy.PseudoConflicting
import           Sdn.Protocol.Node

type ConflictsFrac = 1 % 2

instance PracticalCStruct (PseudoConflicting ConflictsFrac Configuration)

main :: IO ()
main = do
    BenchControllerOptions{..} <- getBenchControllerOptions

    proposedCounter <- newIORef (0 :: Int)
    installedCounter <- newIORef (0 :: Int)

    protocolHandlers <-
        runProtocolNode
            @(PseudoConflicting ConflictsFrac Configuration)
            protocolOptions curProcessId
            (protocolCallbacks installedCounter)

    -- TODO:
    -- For time measuring - 1 second warmup and then count, printing time and
    -- configuration size each second.
    -- Define conflict relation so that wildcards work too

    runTimedIO $ do
        -- give some time for server to be brought up
        wait (for 20 ms)
        putStrLn @Text "Consensus protocol initiated"

        wait (for proposalsStartDelay)
        putStrLn @Text "Proposing policies"

        fork_ . forever $ do
            proposed <- readIORef proposedCounter
            putText $ "Proposed " <> show proposed <> " policies"
            installed <- readIORef installedCounter
            putText $ "Installed " <> show installed <> " policies"
            wait (for 1 sec)

        fork_ . work (for 10 sec) $
            submitEvenly proposalsDelay . liftIO $ do
                policy <- PseudoConflicting @ConflictsFrac
                      <$> generate (genPolicy curProcessId)

                -- putText $ "Proposing " <> show policy
                protocolMakeProposal protocolHandlers policy
                atomicModifyIORef' proposedCounter (\c -> (c + 1, ()))

    runTimedIO sleepForever
  where
    protocolCallbacks installedCounter =
        ProtocolCallbacks
        { protocolOnLearned = \policyAccs -> do
              atomicModifyIORef' installedCounter (\c -> (c + length policyAccs, ()))

              forM_ policyAccs $ \policyAcc -> case policyAcc of
                  Rejected p -> putText $ "Policy rejected " <> show p
                  Accepted _ -> pass

              -- putText $ "Learned " <> show _policyAccs
        }

genPolicy :: ProcessId p -> Gen Policy
genPolicy pid = do
    xid <- arbitrary
    sid <- choose (0, 100)
    return Policy
        { policyCoord = (xid, sid)
        , policyAction = OF.actionSequenceToList OF.flood
        , policyCreatorPid = coerce pid
        }

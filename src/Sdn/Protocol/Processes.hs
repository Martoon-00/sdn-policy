{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Network addresses binded to processes.

module Sdn.Protocol.Processes where

import           Control.Lens         (from)
import           Control.Monad.Reader (withReaderT)
import           Control.TimeWarp.Rpc (NetworkAddress, Port, localhost)
import           Data.Default         (Default (..))
import qualified System.Console.ANSI  as ANSI
import           System.Wlog          (LoggerName)
import           Universum

import           Sdn.Base
import           Sdn.Extra
import           Sdn.Protocol.Context

class Process p where
    type ProcessState p :: *

    processName :: p -> LoggerName
    processAddress :: p -> NetworkAddress
    processesNumber :: Members -> Int
    initProcessState :: p -> ProcessState p

inProcessCtx
    :: forall p m a.
       (MonadIO m, Process p)
    => p -> ReaderT (ProcessContext (ProcessState p)) m a
    -> ReaderT Members m a
inProcessCtx participant action = do
    var <- liftIO $ newTVarIO (initProcessState participant)
    withReaderT (ProcessContext var) action

processPort
    :: Process p
    => p -> Port
processPort = snd . processAddress

processesAddresses
    :: forall p i.
       (Process p, Integral i)
    => (i -> p)
    -> Members
    -> [NetworkAddress]
processesAddresses maker members =
    map processAddress $ processesOf maker members

processesOf
    :: forall p i.
       (Process p, Integral i)
    => (i -> p) -> Members -> [p]
processesOf maker members =
    let number = fromIntegral $ processesNumber @p members
    in  map maker [1 .. number]

-- * Instances

data Proposer = Proposer

instance Process Proposer where
    type ProcessState Proposer = ()

    processName _ = "proposer"
    processAddress Proposer = (localhost, 4000)
    processesNumber _ = 1
    initProcessState Proposer = def


data Leader = Leader

instance Process Leader where
    type ProcessState Leader = LeaderState

    processName _ =
        "leader"
            & loggerNameT %~ withColor ANSI.Vivid ANSI.Magenta
    processAddress Leader = (localhost, 5000)
    processesNumber _ = 1
    initProcessState Leader = def


data Acceptor = Acceptor AcceptorId

instance Process Acceptor where
    type ProcessState Acceptor = AcceptorState

    processName (Acceptor id) =
        "acceptor" <> (pretty id ^. from loggerNameT)
            & loggerNameT %~ withColor ANSI.Vivid ANSI.Yellow
    processAddress (Acceptor id) = (localhost, 6000 + fromIntegral id)
    processesNumber = acceptorsNum
    initProcessState (Acceptor id) = defAcceptorState id


data Learner = Learner Int

instance Process Learner where
    type ProcessState Learner = LearnerState

    processName (Learner id) =
        "learner" <> (pretty id ^. from loggerNameT)
            & loggerNameT %~ withColor ANSI.Vivid ANSI.Cyan
    processAddress (Learner id) = (localhost, 7000 + id)
    processesNumber = learnersNum
    initProcessState _ = def

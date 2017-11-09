{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Network addresses binded to processes.

module Sdn.Protocol.Processes where

import           Control.Lens             (from)
import           Control.Monad.Reader     (withReaderT)
import           Control.TimeWarp.Logging (LoggerName)
import           Control.TimeWarp.Rpc     (NetworkAddress, Port, localhost)
import           Data.Default             (Default (..))
import qualified System.Console.ANSI      as ANSI
import           Universum

import           Sdn.Base
import           Sdn.Extra
import           Sdn.Protocol.Context

-- | Unique features of each process.
class Process p where
    -- | State kept by the process
    type ProcessState p :: *

    -- | Name of the process, used in logging.
    processName :: p -> LoggerName

    -- | Address binded to given process.
    -- All processes have predefined determined addresses.
    processAddress :: p -> NetworkAddress

    -- | Number of processes of this kind.
    processesNumber :: Members -> Int

    -- | Initial state of the process.
    initProcessState :: p -> ProcessState p
    default initProcessState
        :: Default (ProcessState p)
        => p -> ProcessState p
    initProcessState _ = def

-- | Provide context for given process.
inProcessCtx
    :: forall p m a.
       (MonadIO m, Process p)
    => p
    -> ReaderT (ProcessContext (ProcessState p)) m a
    -> ReaderT Members m a
inProcessCtx participant action = do
    var <- liftIO $ newTVarIO (initProcessState participant)
    withReaderT (ProcessContext var) action

-- | Port binded to given process.
processPort
    :: Process p
    => p -> Port
processPort = snd . processAddress

-- | Get all the processes of this kind.
processesOf
    :: forall p i.
       (Process p, Integral i)
    => (i -> p) -> Members -> [p]
processesOf maker members =
    let number = fromIntegral $ processesNumber @p members
    in  map maker [1 .. number]

-- | Get all addresses of processes of this kind.
processesAddresses
    :: forall p i.
       (Process p, Integral i)
    => (i -> p)
    -> Members
    -> [NetworkAddress]
processesAddresses maker members =
    map processAddress $ processesOf maker members

-- * Instances of the processes

data Proposer = Proposer

instance Process Proposer where
    type ProcessState Proposer = ProposerState

    processName _ =
        "proposer"
            & loggerNameT %~ withColor ANSI.Dull ANSI.Magenta
    processAddress Proposer = (localhost, 4000)
    processesNumber _ = 1


data Leader = Leader

instance Process Leader where
    type ProcessState Leader = LeaderState

    processName _ =
        "leader"
            & loggerNameT %~ withColor ANSI.Vivid ANSI.Magenta
    processAddress Leader = (localhost, 5000)
    processesNumber _ = 1


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

{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Network addresses binded to processes.

module Sdn.Protocol.Processes where

import           Control.Lens             (from)
import           Control.TimeWarp.Logging (LoggerName)
import           Control.TimeWarp.Rpc     (NetworkAddress, Port, localhost)
import           Data.Default             (Default (..))
import qualified System.Console.ANSI      as ANSI
import           Universum

import           Sdn.Base
import           Sdn.Extra.Logging        (loggerNameT, withColor)
import           Sdn.Extra.MemStorage
import           Sdn.Protocol.Context
import           Sdn.Protocol.Versions

-- | Unique features of each process.
class Process p where
    -- | State kept by the process
    type ProcessState p :: * -> *

    -- | Name of the process, used in logging.
    processName :: p -> LoggerName

    -- | Color assigned to process (subjectively).
    processColor :: (ANSI.ColorIntensity, ANSI.Color)

    -- | Address binded to given process.
    -- All processes have predefined determined addresses.
    processAddress :: p -> NetworkAddress

    -- | Number of processes of this kind.
    processesNumber :: HasMembers => Int

    -- | Initial state of the process.
    initProcessState :: ProtocolVersion pv => p -> ProcessState p pv
    default initProcessState
        :: Default (ProcessState p pv)
        => p -> ProcessState p pv
    initProcessState _ = def

-- | Constraint for having context with specified mutable state.
type HasContext s m =
    ( MonadIO m
    , DeclaresMemStore m
    , MonadReader (ProcessContext (DeclaredMemStore m s)) m
    )

-- | Constraint for having context of specified type of process.
type HasContextOf p pv m = (HasContext (ProcessState p pv) m, ProtocolVersion pv)

-- | Port binded to given process.
processPort
    :: Process p
    => p -> Port
processPort = snd . processAddress

-- | Get all the processes of this kind.
processesOf
    :: forall p i.
       (HasMembers, Process p, Integral i)
    => (i -> p) -> [p]
processesOf maker =
    let number = fromIntegral $ processesNumber @p
    in  map maker [1 .. number]

-- | Get all addresses of processes of this kind.
processesAddresses
    :: forall p i.
       (HasMembers, Process p, Integral i)
    => (i -> p)
    -> [NetworkAddress]
processesAddresses maker =
    map processAddress $ processesOf maker

-- | Same as 'processesAddresses', but for single process, for compiance.
processAddresses
    :: forall p.
       Process p
    => p -> [NetworkAddress]
processAddresses p = one (processAddress p)

coloredProcessName
    :: forall p.
       Process p
    => p -> LoggerName
coloredProcessName p = processName p & loggerNameT %~ withColor (processColor @p)


-- * Instances of the processes

data Proposer = Proposer

instance Process Proposer where
    type ProcessState Proposer = ProposerState

    processName _ = "proposer"
    processColor = (ANSI.Dull, ANSI.Magenta)
    processAddress Proposer = (localhost, 4000)
    processesNumber = 1


data Leader = Leader

instance Process Leader where
    type ProcessState Leader = LeaderState

    processName _ = "leader"
    processColor = (ANSI.Vivid, ANSI.Magenta)
    processAddress Leader = (localhost, 5000)
    processesNumber = 1
    initProcessState Leader = def


data Acceptor = Acceptor AcceptorId

instance Process Acceptor where
    type ProcessState Acceptor = AcceptorState

    processName (Acceptor (AcceptorId id)) =
        "acceptor" <> (pretty id ^. from loggerNameT)
    processColor = (ANSI.Vivid, ANSI.Yellow)
    processAddress (Acceptor id) = (localhost, 6000 + fromIntegral id)
    processesNumber = acceptorsNum getMembers
    initProcessState (Acceptor id) = defAcceptorState id


data Learner = Learner Int

instance Process Learner where
    type ProcessState Learner = LearnerState

    processName (Learner id) =
        "learner" <> (pretty id ^. from loggerNameT)
    processColor = (ANSI.Vivid, ANSI.Cyan)
    processAddress (Learner id) = (localhost, 7000 + id)
    processesNumber = learnersNum getMembers

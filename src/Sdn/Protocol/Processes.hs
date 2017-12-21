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
import qualified Data.Tagged              as Tag
import qualified System.Console.ANSI      as ANSI
import           Universum

import           Sdn.Base
import           Sdn.Extra
import           Sdn.Protocol.Context
import           Sdn.Protocol.Versions

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
    processesNumber :: HasMembers => Int

    -- | Initial state of the process.
    initProcessState :: ProtocolVersion pv => Proxy pv -> p -> ProcessState p
    default initProcessState
        :: Default (ProcessState p)
        => Proxy pv -> p -> ProcessState p
    initProcessState _ _ = def

-- | Constraint for having context with specified mutable state.
type HasContext s m =
    ( MonadIO m
    , MonadReader (ProcessContext s) m
    )

-- | Constraint for having context of specified type of process.
type HasContextOf p m = HasContext (ProcessState p) m

-- | Provide context for given process.
inProcessCtx
    :: forall p pv m a.
       (MonadIO m, Process p, ProtocolVersion pv)
    => Proxy pv
    -> p
    -> ReaderT (ProcessContext (ProcessState p)) m a
    -> m a
inProcessCtx _ participant action = do
    var <- liftIO $ newTVarIO (initProcessState @p @pv Proxy participant)
    runReaderT action (ProcessContext var)

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


-- * Instances of the processes

data Proposer = Proposer

instance Process Proposer where
    type ProcessState Proposer = ProposerState

    processName _ =
        "proposer"
            & loggerNameT %~ withColor ANSI.Dull ANSI.Magenta
    processAddress Proposer = (localhost, 4000)
    processesNumber = 1


data Leader = Leader

instance Process Leader where
    type ProcessState Leader = LeaderState

    processName _ =
        "leader"
            & loggerNameT %~ withColor ANSI.Vivid ANSI.Magenta
    processAddress Leader = (localhost, 5000)
    processesNumber = 1
    initProcessState pv Leader = Tag.proxy def pv


data Acceptor = Acceptor AcceptorId

instance Process Acceptor where
    type ProcessState Acceptor = AcceptorState

    processName (Acceptor (AcceptorId id)) =
        "acceptor" <> (pretty id ^. from loggerNameT)
            & loggerNameT %~ withColor ANSI.Vivid ANSI.Yellow
    processAddress (Acceptor id) = (localhost, 6000 + fromIntegral id)
    processesNumber = acceptorsNum getMembers
    initProcessState _ (Acceptor id) = defAcceptorState id


data Learner = Learner Int

instance Process Learner where
    type ProcessState Learner = LearnerState

    processName (Learner id) =
        "learner" <> (pretty id ^. from loggerNameT)
            & loggerNameT %~ withColor ANSI.Vivid ANSI.Cyan
    processAddress (Learner id) = (localhost, 7000 + id)
    processesNumber = learnersNum getMembers

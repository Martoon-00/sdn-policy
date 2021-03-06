{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Network addresses binded to processes.

module Sdn.Protocol.Processes where

import           Control.Lens                (from, ix)
import           Control.TimeWarp.Logging    (LoggerName)
import           Control.TimeWarp.Rpc        (NetworkAddress, Port)
import           Data.Default                (Default (..))
import           GHC.Exts                    (fromList)
import qualified System.Console.ANSI         as ANSI
import           Universum

import           Sdn.Base
import           Sdn.Extra.Logging           (loggerNameT, withColor)
import           Sdn.Extra.MemStorage
import           Sdn.Protocol.Common.Context
import           Sdn.Protocol.Versions

-- | Unique features of each process.
class Functor (ProcessAddressContainer p) => Process p where
    -- | State kept by the process
    type ProcessState p :: * -> * -> *

    -- | Type of address assossiated with the process
    -- (single / optional / multiple...).
    type ProcessAddressContainer p :: * -> *
    type ProcessAddressContainer p = Identity

    -- | Name of the process, used in logging.
    processName :: p -> LoggerName

    -- | Color assigned to process (subjectively).
    processColor :: (ANSI.ColorIntensity, ANSI.Color)

    -- | Address binded to given process.
    -- All processes have predefined determined addresses.
    processAddress'
        :: HasMembersAddresses
        => p -> ProcessAddress p

    -- | Number of processes of this kind.
    processesNumber :: HasMembers => Int

    -- | Initial state of the process.
    initProcessState
        :: (ProtocolVersion pv, Default cstruct)
        => p -> ProcessState p pv cstruct
    default initProcessState
        :: Default (ProcessState p pv cstruct)
        => p -> ProcessState p pv cstruct
    initProcessState _ = def

    -- | Return all processes of this kind.
    takeAllProcesses :: HasMembers => NonEmpty p

type ProcessAddress p = ProcessAddressContainer p NetworkAddress
type ProcessPort p = ProcessAddressContainer p Port
type HasSimpleAddress p = ProcessAddressContainer p ~ Identity

-- | Constraint for having context with specified mutable state.
type HasContext s m =
    ( MonadIO m
    , DeclaresMemStore m
    , MonadReader (ProcessContext (DeclaredMemStore m s)) m
    )

-- | Constraint for having context of specified type of process.
type HasContextOf p pv m =
    ( HasContext (ProcessState p pv (DeclaredCStruct m)) m
    , ProtocolVersion pv
    )

processPort'
    :: (Process p, HasMembersAddresses)
    => p -> ProcessPort p
processPort' = fmap snd . processAddress'

processPort
    :: (Process p, HasMembersAddresses, HasSimpleAddress p)
    => p -> Port
processPort = runIdentity ... processPort'

-- | Get all the processes of this kind.
-- Unlike 'takeAllProcesses', this function accepts term-level identifier of
-- process kind.
processesOf
    :: forall p i.
       (HasMembers, Process p, Integral i)
    => (i -> p) -> [p]
processesOf maker =
    let number = fromIntegral $ processesNumber @p
    in  map maker [1 .. number]

-- | By given identifier, return some process, possibly the one specified by identitifer.
takeSomeProcess
    :: forall p i.
       (HasMembers, Process p, Integral i)
    => i -> p
takeSomeProcess (fromIntegral -> i) =
    let processes@(firstProcess :| _) = takeAllProcesses @p
    in  fromMaybe firstProcess (processes ^? ix (i - 1))

processAddress
    :: (HasMembersAddresses, Process p, HasSimpleAddress p)
    => p -> NetworkAddress
processAddress = runIdentity ... processAddress'

-- | Get all addresses of processes of this kind.
processesAddresses'
    :: forall p i.
       (HasMembersInfo, Process p, Integral i)
    => (i -> p)
    -> [ProcessAddress p]
processesAddresses' maker =
    map processAddress' $ processesOf maker

processesAddresses
    :: forall p i.
       (HasMembersInfo, Process p, Integral i, HasSimpleAddress p)
    => (i -> p)
    -> [NetworkAddress]
processesAddresses = map runIdentity ... processesAddresses'

-- | Same as 'processesAddresses', but for single process, for compiance.
processAddresses
    :: forall p.
       (Process p, HasMembers, HasMembersAddresses, HasSimpleAddress p)
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
    type ProcessAddressContainer Proposer = Maybe

    processName _ = "proposer"
    processColor = (ANSI.Dull, ANSI.Magenta)
    processAddress' Proposer =
        proposerAddrInfo getMembersAddresses ^? _ProposerAddrInfoFixed
    processesNumber = 1
    takeAllProcesses = one Proposer


data Leader = Leader

instance Process Leader where
    type ProcessState Leader = LeaderState

    processName _ = "leader"
    processColor = (ANSI.Vivid, ANSI.Magenta)
    processAddress' Leader =
        Identity $ leaderAddrInfo getMembersAddresses
    processesNumber = 1
    initProcessState Leader = def
    takeAllProcesses = one Leader


data Acceptor = Acceptor AcceptorId
    deriving (Eq, Ord, Show)

instance Process Acceptor where
    type ProcessState Acceptor = AcceptorState

    processName (Acceptor (ProcessId id)) =
        "acceptor" <> (pretty id ^. from loggerNameT)
    processColor = (ANSI.Vivid, ANSI.Yellow)
    processAddress' (Acceptor id) =
        Identity $ acceptorsAddrInfos getMembersAddresses $ fromIntegral id
    processesNumber = acceptorsNum getMembers
    initProcessState (Acceptor id) = defAcceptorState id
    takeAllProcesses =
        let n = fromIntegral $ acceptorsNum getMembers
        in  map Acceptor $ fromList [1..n]


data Learner = Learner Int
    deriving (Eq, Ord, Show)

instance Process Learner where
    type ProcessState Learner = LearnerState

    processName (Learner id) =
        "learner" <> (pretty id ^. from loggerNameT)
    processColor = (ANSI.Vivid, ANSI.Cyan)
    processAddress' (Learner id) =
        Identity $ learnersAddrInfos getMembersAddresses id
    processesNumber = learnersNum getMembers
    takeAllProcesses =
        let n = fromIntegral $ learnersNum getMembers
        in  map Learner $ fromList [1..n]

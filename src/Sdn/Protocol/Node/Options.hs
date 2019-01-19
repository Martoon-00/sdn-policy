{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE Rank2Types    #-}

module Sdn.Protocol.Node.Options
    ( NodeOptions (..)
    , nodeOptionsParser
    , processSequentialPort
    ) where

import           Control.TimeWarp.Rpc (Port)
import           Data.Time.Units      (Millisecond, convertUnit)
import qualified Options.Applicative  as Opt
import           Universum

import           Sdn.Base.Types
import           Sdn.Extra

-- | Options assosiated with policies compoition protocol.
data NodeOptions = NodeOptions
    { protocolPorts             :: forall pt. ProcessId pt -> Port
      -- ^ Port, with which given process which should participate in consensus protocol.
    , protocolTotalProcesses    :: Int
      -- ^ Overall number of controllers in network.
    , protocolLeaderId          :: ProcessId LeaderTag
      -- ^ Whether given controller is Paxos leader.
      -- For now this thing will be hardcoded.
    , protocolProposalsBatching :: BatchingSettings
    }

processSequentialPort :: Int -> ProcessId pt -> Port
processSequentialPort startPort processId =
    fromIntegral $ startPort + fromIntegral processId

nodeOptionsParser :: Opt.Parser NodeOptions
nodeOptionsParser = do
    startPort <- Opt.option Opt.auto $ mconcat
        [ Opt.long "protocol-start-port"
        , Opt.metavar "START-PORT"
        , Opt.help "Port, with which first process would participate in \
                    \consensus protocol. Other processes use sequentially next ports."
        ]

    protocolTotalProcesses <- Opt.option Opt.auto $ mconcat
        [ Opt.short 'n'
        , Opt.long "processes"
        , Opt.metavar "NUM"
        , Opt.help "How much controllers would participate."
        ]

    protocolLeaderId <- Opt.option (ProcessId <$> Opt.auto) $ mconcat
        [ Opt.short 'l'
        , Opt.long "leader"
        , Opt.metavar "PROCESS-ID"
        , Opt.help "Identifier of leader process. Should be manually \
                    \specified for now."
        , Opt.value 1
        ]

    protocolProposalsBatching <- do
        batchMaxSize <- Opt.option Opt.auto $ mconcat
            [ Opt.long "batch-size"
            , Opt.metavar "NUM"
            , Opt.help "How much proposals to collect before submitting \
                        \them to consensus protocol."
            , Opt.value 10
            ]

        batchMaxJitter <- fmap asMillis $ Opt.option Opt.auto $ mconcat
            [ Opt.long "batch-jitter"
            , Opt.metavar "MILLISECONDS"
            , Opt.help "Maximal awaitance duration before submitting \
                        \a batch of proposals to consensus protocol."
            , Opt.value 10
            ]

        return BatchingSettings{..}

    return NodeOptions
        { protocolPorts = \pid -> processSequentialPort startPort pid
        , ..
        }
  where
    asMillis = convertUnit @Millisecond @_ . fromInteger

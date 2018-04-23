{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- | Options for running a controller.

module Options
    ( ControllerOptions (..)
    , PlatformOptions (..)
    , ProtocolOptions (..)
    , ProcessId

    , getControllerOptions
    ) where

import           Control.TimeWarp.Rpc (Port)
import           Data.Time.Units
import qualified Options.Applicative  as Opt
import           Universum

import           Sdn.Extra.Batching
import           Sdn.Protocol.Common

-- | Options assosiated to SDN controller itself.
data PlatformOptions = PlatformOptions
    { platformPorts :: ProcessId -> Port
      -- ^ Port, at which given process should listen to messages from switches.
    }

-- | Overall set of options of controller.
data ControllerOptions = ControllerOptions
    { platformOptions :: PlatformOptions
      -- ^ Platform options.
    , protocolOptions :: ProtocolOptions
      -- ^ Protocol options
    , curProcessId    :: ProcessId
      -- ^ Identifier of given controller.
    }

processPort :: Int -> ProcessId -> Port
processPort startPort processId =
    fromIntegral $ startPort + fromIntegral processId

-- | Command-line parser of options
controllerOptionsParser :: Opt.Parser ControllerOptions
controllerOptionsParser = do
    platformOptions <- do
        platformPorts <- fmap processPort . Opt.option Opt.auto $ mconcat
            [ Opt.long "controllers-start-port"
            , Opt.metavar "START-PORT"
            , Opt.help "Port, at which first process would listen to messages \
                       \from switches. Other processes use sequentially next ports."
            ]

        return PlatformOptions{..}

    protocolOptions <- do
        protocolPorts <- fmap processPort . Opt.option Opt.auto $ mconcat
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

        return ProtocolOptions{..}

    curProcessId <- Opt.option (ProcessId <$> Opt.auto) $ mconcat
        [ Opt.short 'i'
        , Opt.long "process-id"
        , Opt.metavar "PROCESS_ID"
        , Opt.help "Identifier of this process. Numeration starts from 1."
        ]

    return ControllerOptions{..}
  where
    asMillis = convertUnit @Millisecond @_ . fromInteger

-- | Command line interraction, options fetching.
getControllerOptions :: IO ControllerOptions
getControllerOptions = Opt.execParser programInfo
  where
    programInfo =
        Opt.info (Opt.helper <*> versionOption <*> controllerOptionsParser) $
            Opt.fullDesc <>
            Opt.progDesc "Controller with concurrent SDN policies installation support" <>
            Opt.header "Prototype"

    versionOption = Opt.infoOption
        "sdn-policies-0.2.0-fast"
        (Opt.long "version" <> Opt.help "Show version.")


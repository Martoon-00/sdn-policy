{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- | Options for running a controller.

module OptionsController
    ( ControllerOptions (..)
    , PlatformOptions (..)
    , NodeOptions (..)
    , ProcessId

    , getControllerOptions
    ) where

import           Control.TimeWarp.Rpc (Port)
import qualified Options.Applicative  as Opt
import           Universum

import           Sdn.Base
import           Sdn.Protocol.Node

-- | Options assosiated to SDN controller itself.
data PlatformOptions = PlatformOptions
    { platformPorts :: GeneralProcessId -> Port
      -- ^ Port, at which given process should listen to messages from switches.
    }

-- | Overall set of options of controller.
data ControllerOptions = ControllerOptions
    { platformOptions :: PlatformOptions
      -- ^ Platform options.
    , protocolOptions :: NodeOptions
      -- ^ Protocol options
    , curProcessId    :: GeneralProcessId
      -- ^ Identifier of given controller.
    }

-- | Command-line parser of options
controllerOptionsParser :: Opt.Parser ControllerOptions
controllerOptionsParser = do
    platformOptions <- do
        platformPorts <- fmap processSequentialPort . Opt.option Opt.auto $ mconcat
            [ Opt.long "controllers-start-port"
            , Opt.metavar "START-PORT"
            , Opt.help "Port, at which first process would listen to messages \
                       \from switches. Other processes use sequentially next ports."
            ]

        return PlatformOptions{..}

    protocolOptions <- nodeOptionsParser

    curProcessId <- Opt.option (ProcessId <$> Opt.auto) $ mconcat
        [ Opt.short 'i'
        , Opt.long "process-id"
        , Opt.metavar "PROCESS_ID"
        , Opt.help "Identifier of this process. Numeration starts from 1."
        ]

    return ControllerOptions{..}

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

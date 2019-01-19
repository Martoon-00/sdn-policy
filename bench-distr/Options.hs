{-# LANGUAGE ApplicativeDo #-}

module Options
     ( BenchControllerOptions (..)
     , getBenchControllerOptions
     ) where

import           Data.Time.Units     (Microsecond, Second, convertUnit)
import qualified Options.Applicative as Opt
import           Universum

import           Sdn.Base
import           Sdn.Extra.Util
import           Sdn.Protocol.Node

data BenchControllerOptions = BenchControllerOptions
    { protocolOptions     :: NodeOptions
    , curProcessId        :: GeneralProcessId
    , proposalsStartDelay :: Second
    , proposalsDelay      :: Microsecond
    }

-- | Command-line parser of options
benchControllerOptionsParser :: Opt.Parser BenchControllerOptions
benchControllerOptionsParser = do
    protocolOptions <- nodeOptionsParser

    curProcessId <- Opt.option (ProcessId <$> Opt.auto) $ mconcat
        [ Opt.short 'i'
        , Opt.long "process-id"
        , Opt.metavar "PROCESS_ID"
        , Opt.help "Identifier of this process. Numeration starts from 1."
        ]

    proposalsStartDelay <- Opt.option (fromInteger <$> Opt.auto) $ mconcat
        [ Opt.long "start-delay"
        , Opt.metavar "SECONDS"
        , Opt.help "How long to wait before starting to propose policies, \
                   \allows initialize other controllers in time."
        , Opt.value 1
        ]

    proposalsDelay <- Opt.option (Opt.maybeReader readTime) $ mconcat
        [ Opt.long "proposals-delay"
        , Opt.metavar "TIME"
        , Opt.help "Time delay between consequent proposals."
        , Opt.value (convertUnit @Second 5)
        ]

    return BenchControllerOptions{..}

-- | Command line interraction, options fetching.
getBenchControllerOptions :: IO BenchControllerOptions
getBenchControllerOptions = Opt.execParser programInfo
  where
    programInfo =
        Opt.info (Opt.helper <*> versionOption <*> benchControllerOptionsParser) $
            Opt.fullDesc <>
            Opt.progDesc "Policy installing part of controller suitable for run in cluster" <>
            Opt.header "Prototype"

    versionOption = Opt.infoOption
        "sdn-policies-0.2.0-fast"
        (Opt.long "version" <> Opt.help "Show version.")

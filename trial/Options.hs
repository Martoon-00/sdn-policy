{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ApplicativeDo        #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Command line options for demo.

module Options where

import           Control.Lens         (Setter', makeLensesWith)
import           Control.Monad.Writer (WriterT (..))
import           Data.Default         (Default (..))
import qualified Data.Text.Buildable
import           Data.Time.Units      (Microsecond, Second, convertUnit)
import           Data.Yaml            (FromJSON (..), Value (..), decodeFileEither,
                                       withArray, withObject, withText, (.:), (.:?))
import qualified Data.Yaml            as Yaml
import           Formatting           (bprint, build, builder, sformat, stext, (%))
import qualified Options.Applicative  as Opt
import           Test.QuickCheck      (Gen, arbitrary, frequency)
import           Universum

import           Sdn.Base
import           Sdn.Extra            (WithDesc (..), combineWithDesc, getWithDesc,
                                       postfixLFields, rightSpaced, (?:))
import           Sdn.Protocol
import           Sdn.Schedule         (Schedule)
import qualified Sdn.Schedule         as S

-- * Helpers

optionalWithDef :: Alternative f => a -> f a -> f a
optionalWithDef defValue = fmap (fromMaybe defValue) . optional

applyChanges :: Applicative m => s -> WriterT (Endo s) m () -> m s
applyChanges init (WriterT writer) =
    writer <&> \((), Endo change) -> change init

optionalParserMod
    :: (Alternative f)
    => Setter' a b -> f b -> WriterT (Endo a) f ()
optionalParserMod lens' parser =
    WriterT $ optional parser <&> maybe mempty (pure . Endo . set lens')

parserMod
    :: Applicative f
    => Setter' a b -> f b -> WriterT (Endo a) f ()
parserMod lens' parser = WriterT $ parser <&> (pure . Endo . set lens')

choose :: Alternative f => [f a] -> f a
choose = foldr (<|>) empty

choose1 :: Alternative f => [a -> f b] -> a -> f b
choose1 l a = choose $ map ($ a) l

-- * Datatypes with options

data ProtocolType
    = ClassicProtocol
    | FastProtocol
    deriving (Eq, Show)

instance Buildable ProtocolType where
    build = \case
        ClassicProtocol -> "classic"
        FastProtocol -> "fast"

data ScheduleBuilder a
    = Execute Text (Gen a)
    | Periodically Microsecond (Maybe Word) (ScheduleBuilder a)
    | Delayed Microsecond (ScheduleBuilder a)
    | Times Word (ScheduleBuilder a)
    | InParallel [ScheduleBuilder a]
    deriving (Generic)

buildSchedule :: ScheduleBuilder a -> (forall m. S.MonadSchedule m => Schedule m a)
buildSchedule = \case
    Execute _ gen -> S.generate gen
    Periodically (convertUnit -> delay) times sb ->
        maybe S.periodic S.repeating times delay >> buildSchedule sb
    Delayed (convertUnit -> time) sb ->
        S.delayed time >> buildSchedule sb
    Times n sb ->
        S.times n >> buildSchedule sb
    InParallel sbs -> mconcat $ map buildSchedule sbs

instance Buildable (ScheduleBuilder a) where
    build = \case
        Execute desc _ ->
            bprint stext desc
        Periodically sec times sb ->
            bprint ("once per "%build%" "%rightSpaced builder%build)
                sec (maybe "" (bprint ("up to "%build%" times")) times) sb
        Delayed time sb ->
            bprint ("starting since "%build%" "%build) time sb
        Times n sb ->
            bprint (build%" times "%build) n sb
        InParallel sbs ->
            mconcat $ intersperse " + " $ map (bprint $ "("%build%")") sbs

data TopologySettingsBuilder = TopologySettingsBuilder
    { tsbMembers          :: Members
    , tsbProposalSchedule :: ScheduleBuilder Policy
    , tsbBallotsSchedule  :: ScheduleBuilder ()
    , tsbRecoveryDelay    :: Microsecond
    , tsbLifetime         :: Microsecond
    } deriving (Generic)

makeLensesWith postfixLFields ''TopologySettingsBuilder

instance Buildable TopologySettingsBuilder where
    build TopologySettingsBuilder{..} =
        bprint ( "  members: \n"%build%
               "\n  proposal schedule: "%build%
               "\n  ballots schedule: "%build%
               "\n  recovery delay: "%build%
               "\n  lifetime: "%build)
            tsbMembers
            tsbProposalSchedule
            tsbBallotsSchedule
            tsbRecoveryDelay
            tsbLifetime

instance Default TopologySettingsBuilder where
    def = TopologySettingsBuilder
        { tsbMembers = def
        , tsbProposalSchedule =
            Execute "single good policy" $ (GoodPolicy <$> arbitrary)
        , tsbBallotsSchedule = Execute "execute" pass
        , tsbRecoveryDelay = convertUnit @Second 3
        , tsbLifetime = convertUnit @Second 999999
        }

buildTopologySettings :: TopologySettingsBuilder -> TopologySettings
buildTopologySettings TopologySettingsBuilder{..} =
    TopologySettings
    { topologyMembers = tsbMembers
    , topologyProposalSchedule = buildSchedule tsbProposalSchedule
    , topologyBallotsSchedule = buildSchedule tsbBallotsSchedule
    , topologyRecoveryDelay = convertUnit tsbRecoveryDelay
    , topologySeed = S.RandomSeed  -- TODO: specify
    , topologyLifetime = convertUnit tsbLifetime
    }

data ProtocolOptions = ProtocolOptions
    { poTopologySettings :: TopologySettingsBuilder
    , poType             :: ProtocolType
    }

instance Buildable ProtocolOptions where
    build ProtocolOptions{..} =
        bprint ( "  protocol version: "%build%
               "\n  "%build)
        poType
        poTopologySettings

-- * Parser

instance FromJSON Members where
    parseJSON = withObject "members" $ \o -> do
        acceptorsNum <- o .: "acceptors"
        learnersNum <- o .: "learners"
        return Members{..}

instance FromJSON Microsecond where
    parseJSON = choose1
        [ withObject "time with measure" $ \o -> fromMcs <$> choose
            [ (* 1000000) <$> o .: "sec"
            , (* 1000) <$> o .: "ms"
            , o .: "mcs"
            ]
        , fmap (fromMcs . (* 1000000)) . parseJSON
        ]
      where
        fromMcs = fromInteger . round @Double

instance FromJSON (WithDesc (PolicyName -> Policy)) where
    parseJSON = \case
        String "good" -> return $ "good policy" ?: GoodPolicy
        String "bad" -> return $ "bad policy" ?: BadPolicy
        Object o -> do
            id <- o .: "moody"
            return $ ("moody policy #" <> pretty id) ?: MoodyPolicy id
        _ -> fail "Unknown policy"

instance FromJSON (WithDesc $ Gen ()) where
    parseJSON = \case
        Null -> ok
        String "execute" -> ok
        _ -> fail "Expected no value"
      where
        ok = pure $ WithDesc "execute" $ pure ()

instance FromJSON (WithDesc $ Gen Policy) where
    parseJSON = choose1
        [ weightenedArbitraryPolicies
        , \o -> (parseJSON o <&> (fmap $ \f -> f <$> arbitrary @PolicyName))
        ]
      where
        weightenedArbitraryPolicies :: Value -> Yaml.Parser $ WithDesc (Gen Policy)
        weightenedArbitraryPolicies = withArray "weightened policies" $ \a -> do
            fmap frequency . combineWithDesc . toList <$> mapM weightenedArbitraryPolicy a
        weightenedArbitraryPolicy :: Value -> Yaml.Parser $ WithDesc (Int, Gen Policy)
        weightenedArbitraryPolicy = withObject "weightened policy" $ \o -> do
            weight <- fromMaybe 1 <$> o .:? "weight"
            WithDesc policyDesc policy <- o .: "policy"
            pure (  sformat (build%"w "%stext) weight policyDesc
                 ?: (weight, policy)
                 )

instance FromJSON (WithDesc $ Gen a) => FromJSON (ScheduleBuilder a) where
    parseJSON = choose1
        [ periodicParser
        , delayedParser
        , timesParser
        , parallelParser
        , executeParser
        , onceParser
        ]
      where
        executeParser = withObject "solo schedule" $ \o -> do
            sb <- o .: "once"
            return . uncurry Execute $ getWithDesc sb
        onceParser v =
            uncurry Execute . getWithDesc <$> parseJSON v
        periodicParser = withObject "periodic schedule" $ \o -> do
            period <- o .: "period"
            times <- o .:? "repeat"
            sb <- o .: "schedule"
            pure $ Periodically period times sb
        delayedParser = withObject "delayed schedule" $ \o -> do
            delay <- o .: "delay"
            sb <- o .: "schedule"
            pure $ Delayed delay sb
        timesParser = withObject "times parser" $ \o -> do
            n <- o .: "times"
            sb <- o .: "schedule"
            pure $ Times n sb
        parallelParser = withArray "parallel schedule" $ \a -> do
            fmap InParallel $ mapM parseJSON (toList a)

instance FromJSON ProtocolType where
    parseJSON = withText "protocol type" $ \case
        "classic" -> pure ClassicProtocol
        "fast" -> pure FastProtocol
        _ -> fail "Unknown protocol type"

instance FromJSON TopologySettingsBuilder where
    parseJSON = withObject "topology settings" $ \o -> do
        tsbMembers <- o .: "members"
        tsbProposalSchedule <- o .: "proposals"
        tsbBallotsSchedule <- o .: "ballots"
        recovery <- o .: "recovery" -- TODO: make optional for classic ballots
        tsbRecoveryDelay <- recovery .: "delay"
        tsbLifetime <- o .: "lifetime"
        return TopologySettingsBuilder{..}

instance FromJSON ProtocolOptions where
    parseJSON v = do
        poTopologySettings <- parseJSON v
        poType <- parseJSON v >>=
            (withObject "protocol type" $ \o -> o .: "type")
        return ProtocolOptions{..}

configPathParser :: Opt.Parser FilePath
configPathParser = Opt.strOption $
    Opt.long "config" <>
    Opt.help "Path to file describing network topology configuration" <>
    Opt.metavar "FILEPATH to YAML" <>
    Opt.value "topology.yaml"

getProtocolOptions :: IO ProtocolOptions
getProtocolOptions = do
    configPath <- Opt.execParser programInfo
    either (error . show) identity <$> decodeFileEither configPath
  where
    programInfo =
        Opt.info (Opt.helper <*> versionOption <*> configPathParser) $
            Opt.fullDesc <>
            Opt.progDesc "SDN concurrent policies composition protocol" <>
            Opt.header "Demo version"

    versionOption = Opt.infoOption
        "sdn-policies-0.2.0-fast"
        (Opt.long "version" <> Opt.help "Show version.")


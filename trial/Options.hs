{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ApplicativeDo        #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Command line options for demo.

module Options where

import           Control.Lens         (Setter', makeLensesWith)
import           Control.Monad.Writer (WriterT (..))
import           Data.Default         (Default (..))
import qualified Data.Text.Buildable
import           Data.Time.Units      (Millisecond, Second, convertUnit)
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

class HasCustomParser a where
    customParser :: Opt.Parser a

optionalWithDef :: Alternative f => a -> f a -> f a
optionalWithDef defValue = fmap (fromMaybe defValue) . optional

timeOption
    :: Num t
    => Opt.Mod Opt.OptionFields Integer -> Opt.Parser t
timeOption mod' =
    fmap fromInteger $ Opt.option Opt.auto mod'

applyChanges :: Applicative m => s -> WriterT (Endo s) m () -> m s
applyChanges init (WriterT writer) =
    writer <&> \((), Endo change) -> change init

optionalParserMod :: Setter' a b -> Opt.Parser b -> WriterT (Endo a) Opt.Parser ()
optionalParserMod lens' parser =
    WriterT $ optional parser <&> maybe mempty (pure . Endo . set lens')

parserMod :: Setter' a b -> Opt.Parser b -> WriterT (Endo a) Opt.Parser ()
parserMod lens' parser = WriterT $ parser <&> (pure . Endo . set lens')

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
    | Periodically Second (Maybe Word) (ScheduleBuilder a)
    | Delayed Millisecond (ScheduleBuilder a)
    | InParallel [ScheduleBuilder a]

buildSchedule :: ScheduleBuilder a -> (forall m. S.MonadSchedule m => Schedule m a)
buildSchedule = \case
    Execute _ gen -> S.generate gen
    Periodically (convertUnit -> delay) times sb ->
        maybe S.periodic S.repeating times delay >> buildSchedule sb
    Delayed (convertUnit -> time) sb ->
        S.delayed time >> buildSchedule sb
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
        InParallel sbs ->
            mconcat $ intersperse " + " $ map (bprint build) sbs

data TopologySettingsBuilder = TopologySettingsBuilder
    { tsbMembers          :: Members
    , tsbProposalSchedule :: ScheduleBuilder Policy
    , tsbBallotsSchedule  :: ScheduleBuilder ()
    , tsbRecoveryDelay    :: Second
    , tsbLifetime         :: Second
    }

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
        , tsbRecoveryDelay = 3 :: Second
        , tsbLifetime = 999999 :: Second
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

instance HasCustomParser Members where
    customParser = do
        acceptorsNum <- Opt.option Opt.auto $
            Opt.short 'a' <>
            Opt.long  "acceptors" <>
            Opt.help  "Number of acceptors." <>
            Opt.metavar "NUM" <>
            Opt.value 3
        learnersNum <- Opt.option Opt.auto $
            Opt.short 'l' <>
            Opt.long  "learners" <>
            Opt.help  "Number of learners." <>
            Opt.metavar "NUM" <>
            Opt.value 1
        pure Members{..}

instance HasCustomParser (WithDesc $ Gen ()) where
    customParser = Opt.flag' ("nothing" ?: pure ()) $
        Opt.long "execute" <>
        Opt.help "Complete schedule which generates nothing"

instance HasCustomParser (WithDesc (PolicyName -> Policy)) where
    customParser = foldr1 (<|>)
        [ Opt.flag' ("good policy" ?: GoodPolicy) $
            Opt.short 'g' <>
            Opt.long "good" <>
            Opt.help "Policy which doesn't conflict with any other one."
        , Opt.flag' ("bad policy" ?: BadPolicy) $
            Opt.short 'b' <>
            Opt.long "bad" <>
            Opt.help "Policy which conflicts with any other one."
        , fmap (\id -> ("moody " <> pretty id ?: MoodyPolicy id)) . Opt.option Opt.auto $
            Opt.long "moody" <>
            Opt.help "Policy which conflicts only with policies with same id." <>
            Opt.metavar "NUM ID"
        ]

instance HasCustomParser (WithDesc $ Gen Policy) where
    customParser =
        weightenedArbitraryPolicies <|>
        (customParser <&> (fmap $ \f -> f <$> arbitrary @PolicyName))
      where
        weightenedArbitraryPolicies :: Opt.Parser $ WithDesc (Gen Policy)
        weightenedArbitraryPolicies = fmap (fmap frequency . combineWithDesc) . some $ do
            weight <- Opt.option Opt.auto $
                Opt.short 'w' <>
                Opt.long "weight" <>
                Opt.help "Probability of getting given policy."
            WithDesc policyDesc policy <- customParser
            pure (  sformat (build%"x "%stext) weight policyDesc
                 ?: (weight, policy)
                 )

instance HasCustomParser (WithDesc $ Gen a) => HasCustomParser (ScheduleBuilder a) where
    customParser = fmap InParallel . some $
        periodicParser <|>
        delayedParser <|>
        executeParser
      where
        executeParser = uncurry Execute . getWithDesc <$> customParser
        periodicParser = do
            period <- Opt.option Opt.auto $
                Opt.long "period" <>
                Opt.metavar "SECONDS" <>
                Opt.help "Execute once per given number of seconds."
            times <- optional . Opt.option Opt.auto $
                Opt.long "times" <>
                Opt.metavar "NUM" <>
                Opt.help "Execute only given number of times."
            sb <- customParser
            pure $ Periodically period times sb
        delayedParser = do
            delay <- Opt.option Opt.auto $
                Opt.long "delay" <>
                Opt.metavar "MILLISECONDS" <>
                Opt.help "Execute after given amount of time."
            sb <- customParser
            pure $ Delayed delay sb

instance HasCustomParser ProtocolType where
    customParser = foldr (<|>) (pure FastProtocol) $
        [ Opt.flag' ClassicProtocol $
            Opt.short 'c' <>
            Opt.long "classic" <>
            Opt.help "Use classic version of protocol."
        , Opt.flag' FastProtocol $
            Opt.short 'f' <>
            Opt.long "fast" <>
            Opt.help "Use fast version of protocol with classic as recovery."
        ]

instance HasCustomParser TopologySettingsBuilder where
    customParser = applyChanges def $ sequenceA_ $
        [ parserMod tsbMembersL customParser

        , optionalParserMod tsbProposalScheduleL customParser

        , optionalParserMod tsbBallotsScheduleL customParser

        , optionalParserMod tsbRecoveryDelayL . timeOption @Second $
            Opt.long "recovery-delay" <>
            Opt.help "How much time to wait before check the need in recovery \
                     \ballot. Used in fast version only" <>
            Opt.metavar "SECONDS"

        , optionalParserMod tsbLifetimeL . timeOption @Second $
            Opt.long "lifetime" <>
            Opt.help "Whole run duration" <>
            Opt.metavar "SECONDS"
        ]

instance HasCustomParser ProtocolOptions where
    customParser = do
        poTopologySettings <- customParser
        poType <- customParser
        return ProtocolOptions{..}

getProtocolOptions :: IO ProtocolOptions
getProtocolOptions = Opt.execParser programInfo
  where
    programInfo =
        Opt.info (Opt.helper <*> versionOption <*> customParser) $
            Opt.fullDesc <>
            Opt.progDesc "SDN concurrent policies composition protocol" <>
            Opt.header "Demo version"

    versionOption = Opt.infoOption
        "sdn-policies-0.2.0-fast"
        (Opt.long "version" <> Opt.help "Show version.")


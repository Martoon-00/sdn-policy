{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Various utility functions

module Sdn.Extra.Util where

import           Control.Lens            (Getting, Iso, Iso', LensRules, Wrapped (..),
                                          from, has, involuted, iso, lens, lensField,
                                          lensRules, makeLenses, mappingNamer, review)
import           Control.Monad.Random    (MonadRandom, getRandom)
import           Control.Monad.STM.Class (MonadSTM (..))
import           Control.TimeWarp.Rpc    (MonadRpc (..), NetworkAddress, RpcRequest (..),
                                          mkRequest)
import qualified Control.TimeWarp.Rpc    as Rpc
import           Control.TimeWarp.Timed  (Microsecond, MonadTimed (..))
import           Data.Coerce             (coerce)
import           Data.MessagePack        (MessagePack (..))
import qualified Data.Text.Buildable
import           Data.Text.Lazy.Builder  (Builder)
import           Data.Time.Units         (Millisecond, Second)
import           Formatting              (Format, bprint, build, formatToString, later,
                                          shortest, shown, string, (%))
import           Formatting.Internal     (Format (..))
import           GHC.Exts                (IsList (..))
import qualified GHC.Exts                as Exts
import qualified Language.Haskell.TH     as TH
import qualified System.Console.ANSI     as ANSI
import           Test.QuickCheck         (Gen, choose, suchThat)
import           Test.QuickCheck.Gen     (unGen)
import           Test.QuickCheck.Random  (mkQCGen)
import           Universum               hiding (toList)
import           Unsafe                  (unsafeFromJust)

-- | Declare instance for one-way message.
declareMessage :: TH.Name -> TH.Q [TH.Dec]
declareMessage msgType = do
    dec1 <- [d| instance MessagePack $getFullType |]
    dec2 <- mkRequest msgType ''()
    return $ dec2 <> dec1
  where
    getFullType = do
        (name, vars) <- TH.reify msgType >>= \case
            TH.TyConI (TH.NewtypeD _ nname typeVars _ _ _) -> pure (nname, typeVars)
            TH.TyConI (TH.DataD _ dname typeVars _ _ _) -> pure (dname, typeVars)
            TH.TyConI (TH.TySynD tname typeVars _) -> pure (tname, typeVars)
            _ -> fail $ formatToString ("Type "%shown%" not found") msgType
        typeArgs <- replicateM (length vars) $ TH.VarT <$> TH.newName "a"
        pure $ foldl TH.AppT (TH.ConT name) typeArgs

type RpcOptions = '[Rpc.RpcOptionMessagePack, Rpc.RpcOptionNoReturn]

type Message msg = (RpcRequest msg, Rpc.RpcConstraints RpcOptions msg)

-- | Alias for 'Rpc.send', restricted to messages satisfying 'Message' constraint.
submit
    :: (MonadCatch m, MonadTimed m, MonadRpc RpcOptions m, Message msg)
    => NetworkAddress -> msg -> m ()
submit = Rpc.send

-- | Builder for list.
listF
    :: IsList l
    => Builder -> Format Builder (Item l -> Builder) -> Format r (l -> r)
listF delim buildElem =
    later $ \(toList -> values) ->
    if null values
    then "[]"
    else mconcat $
         one "[ " <> (intersperse delim $ bprint buildElem <$> values) <> one " ]"

pairF
    :: Builder
    -> Format Builder (a -> Builder)
    -> Format Builder (b -> Builder)
    -> Format r ((a, b) -> r)
pairF delim buildA buildB =
    later $ \(a, b) -> bprint buildA a <> delim <> bprint buildB b

-- | Extended modifier for 'TVar'.
modifyTVarS :: (Monad m, MonadSTM m) => TVar s -> StateT s m a -> m a
modifyTVarS var modifier = do
    st <- liftSTM $ readTVar var
    (res, st') <- runStateT modifier st
    liftSTM $ writeTVar var st'
    return res

-- | Lens which looks inside the list-like structure
listL
    :: (Exts.IsList (t a), Exts.IsList (t b))
    => Iso (t a) (t b) [Exts.Item (t a)] [Exts.Item (t b)]
listL = iso Exts.toList Exts.fromList

-- | Try generating until getting 'Just'.
genJust :: Gen (Maybe a) -> Gen a
genJust gen = unsafeFromJust <$> gen `suchThat` isJust

-- | Rule to generate 'memeL' lenses for 'meme' field.
postfixLFields :: LensRules
postfixLFields = lensRules & lensField .~ mappingNamer (\s -> [s++"L"])

-- | Move from pure exception to monadic one.
throwOnFail
    :: (Exception e', MonadThrow m)
    => (e -> e') -> Either e a -> m a
throwOnFail mkException = either (throwM . mkException) pure

-- | Modify text produced by formatter.
mapfText :: (Builder -> Builder) -> Format a b -> Format a b
mapfText how (Format f) = Format $ f . \g t -> g $ how t

as :: forall b a. Coercible a b => Iso' a b
as = iso coerce coerce

-- | Add space at right if formatter returns non-empty text.
rightSpaced :: Format a b -> Format a b
rightSpaced = mapfText $ \x -> if x == "" then "" else x <> " "

-- | Colorizing formatter
coloredF :: (ANSI.ColorIntensity, ANSI.Color) -> Format a b -> Format a b
coloredF (int, color) = mapfText colorize
  where
    colorize text = mconcat
        [ bprint string $ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground int color]
        , text
        , bprint string $ ANSI.setSGRCode [ANSI.Reset]
        ]

gray :: (ANSI.ColorIntensity, ANSI.Color)
gray = (ANSI.Dull, ANSI.White)

instance Buildable Second where
    build = bprint (build%" sec") . toInteger

instance Buildable Millisecond where
    build = bprint (build%" ms") . toInteger

instance Buildable Microsecond where
    build (toInteger -> t)
        | t > 100000 = bprint (shortest%" sec") (deciRound 100000)
        | t > 100 = bprint (shortest%" ms") (deciRound 100)
        | otherwise = bprint (shortest%" mcs") t
      where
        deciRound d = fromIntegral @Int @Double (round (fromIntegral @_ @Double t / d)) / 10

-- | Something with description.
data WithDesc a = WithDesc
    { _descText :: Text
    , dropDesc  :: a
    } deriving (Functor)

makeLenses ''WithDesc

instance Monoid a => Monoid (WithDesc a) where
    mempty = WithDesc "" mempty
    WithDesc t1 a1 `mappend` WithDesc t2 a2 = WithDesc (t1 <> "; " <> t2) (a1 <> a2)

instance Applicative WithDesc where
    pure = WithDesc ""
    WithDesc t1 f <*> WithDesc t2 a = WithDesc (t1 <> " " <> t2) (f a)

-- | Alias for 'WithDesc'.
(?:) :: Text -> a -> WithDesc a
(?:) = WithDesc
infix 1 ?:

-- | Unwrap 'WithDesc'
getWithDesc :: WithDesc a -> (Text, a)
getWithDesc (WithDesc t a) = (t, a)

-- | Run QuickCheck generator taking seed from monad context.
generateM :: MonadRandom m => Gen a -> m a
generateM generator = do
    seed <- getRandom
    return $ unGen generator (mkQCGen seed) 30

-- | Examples: "qurek", "manek".
genSoundWord :: IsString s => Int -> Gen s
genSoundWord k = fromString <$> doGen
  where
    doGen = forM [1..k] $ \i ->
        choose ('a', 'z') `suchThat` (\c -> even i == isVowel c)
    isVowel c = any (== c) ['a', 'y', 'o', 'e', 'i', 'u']

-- | 'zoom' specified to 'MonadState'.
zoom :: MonadState s m => Lens' s a -> StateT a m r -> m r
zoom l st = do
    s <- get
    let a = s ^. l
    (r, a') <- runStateT st a
    modify (l .~ a')
    return r

-- | 'MonadState'-ic version of 'has'.
exists :: Monad m => Getting Any s a -> StateT s m Bool
exists l = has l <$> get

-- | Whether value is present.
-- Helper for accessing elements in 'Set'.
presence :: Lens' (Maybe ()) Bool
presence = lens (maybe False (\() -> True))
                (\_ b -> if b then Just () else Nothing)

-- | Whether value is absent.
-- Helper for accessing elements in 'Set'.
absence :: Lens' (Maybe ()) Bool
absence = presence . involuted not

instance MessagePack a => MessagePack (NonEmpty a) where
    toObject = toObject . toList
    fromObject = maybe (fail "MessagePack NonEmpty: got empty list") pure
             <=< fmap nonEmpty . fromObject

-- | Alias for 'mzero'.
exit :: MonadPlus m => m a
exit = mzero

-- | Modify under newtype.
underneath :: Wrapped s => (Unwrapped s -> Unwrapped s) -> s -> s
underneath = over _Wrapped'

-- | Similar to 'over', but for functions which accepts 2 arguments.
over2 :: Iso' s a -> (a -> a -> a) -> s -> s -> s
over2 l f a b = f (a ^. l) (b ^. l) ^. from l

-- | Apply function of 2 arguments under newtype.
underneath2
    :: Wrapped s
    => (Unwrapped s -> Unwrapped s -> Unwrapped s) -> s -> s -> s
underneath2 = over2 _Wrapped'

-- | Like @pack@ for @Newtype@, but in terms of 'Wrapped'.
pack :: Wrapped s => Unwrapped s -> s
pack = review _Wrapped'

-- | Like @unpack@ for @Newtype@, but in terms of 'Wrapped'.
unpack :: Wrapped s => s -> Unwrapped s
unpack = view _Wrapped'

{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Sdn.Policy.PseudoConflicting
  ( PseudoConflicting (..)
  ) where

import           Control.Lens     (Wrapped (..))
import           Data.Coerce      (coerce)
import           Data.Default     (Default (..))
import           Data.Hashable    (Hashable (..))
import           Data.MessagePack (MessagePack)
import           Universum

import           Sdn.Base

-- | This wrapper entirely modifies semantics of 'Conflict' instance:
-- in addition to existing conflict conditions, two items are considered
-- conflicting arbitrarily with probability @f@.
--
-- Use '(%)' datatype to construct the first type parameter.
-- There might be another datatype passed there, then it's semantics should be
-- specified via 'Conflict' instance.
newtype PseudoConflicting f a = PseudoConflicting { unPseudoConflicting :: a }
    deriving (Show, Eq, Ord, Buildable, Generic, MessagePack, MayHaveProposerId)

instance Wrapped (PseudoConflicting f a)

instance ( f ~ f1
         , Conflict (PseudoConflicting f a) (PseudoConflicting f1 b)
         ) =>
         Conflict (PseudoConflicting f a) (Acceptance (PseudoConflicting f1 b)) where
  conflictReason p1 = \case
    Rejected _ -> pass
    Accepted p2 -> conflictReason p1 p2

instance ( f ~ f1
         , Conflict (PseudoConflicting f a) (PseudoConflicting f1 b)
         ) =>
         Conflict (Acceptance (PseudoConflicting f1 b)) (PseudoConflicting f a) where
  conflictReason = flip conflictReason

instance Default a => Default (PseudoConflicting f a) where
    def = PseudoConflicting def

instance (AtCmd cfg, Cmd cfg ~ Acceptance rawCmd) =>
         AtCmd (PseudoConflicting f cfg) where
  atCmd rawCmd = _Wrapped' . atCmd @cfg (unPseudoConflicting rawCmd)

instance ( CStruct cfg, Hashable rawCmd
         , Cmd cfg ~ Acceptance rawCmd
         , Eq rawCmd
         , Conflict (PseudoConflicting f rawCmd)
                    (PseudoConflicting f rawCmd)
         , Conflict (PseudoConflicting f cfg)
                    (PseudoConflicting f rawCmd)
         , Conflict (PseudoConflicting f cfg)
                    (PseudoConflicting f cfg)
         ) =>
         CStruct (PseudoConflicting f cfg) where
    type Cmd (PseudoConflicting f cfg) =
      Acceptance $ PseudoConflicting f $ RawCmd cfg

    addCommand p'@(fmap unPseudoConflicting -> p) c'@(PseudoConflicting c) =
      conflictReason p' c' *>
      (PseudoConflicting <$> addCommand p c)
    glb c1'@(PseudoConflicting c1) c2'@(PseudoConflicting c2) =
        conflictReason c1' c2' *> (PseudoConflicting <$> glb c1 c2)
    lub (PseudoConflicting c1) (PseudoConflicting c2) =
        PseudoConflicting $ lub c1 c2
    extends (PseudoConflicting c1) (PseudoConflicting c2) =
        extends c1 c2
    difference (PseudoConflicting c1) (PseudoConflicting c2) =
        map (fmap PseudoConflicting) $ difference c1 c2
    combination votes =
        PseudoConflicting <$> combination (coerce <$> votes)

-- data DummyConfig a = DummyConfig

-- instance Default (DummyConfig a) where
--   def = DummyConfig

-- instance Buildable (DummyConfig a) where
--   build DummyConfig = "<dummy config>"

-- instance Hashable (DummyConfig a) where
--   hashWithSalt _ DummyConfig = 0

-- instance ( Hashable p
--          , f ~ (k % n), KnownNat k, KnownNat n
--          ) => CStruct (PseudoConflicting f (DummyConfig p)) where
--   type Cmd (PseudoConflicting f (DummyConfig p)) = PseudoConflicting f p
--   addCommand = checkingAgreement $
--     \_ (PseudoConflicting DummyConfig) -> PseudoConflicting DummyConfig

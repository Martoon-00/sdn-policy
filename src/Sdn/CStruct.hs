{-# LANGUAGE FunctionalDependencies #-}

-- | Interface for commands and cstructs.

module Sdn.CStruct where

import           Data.Default     (Default)
import           Data.MessagePack (MessagePack)
import           Universum

-- * Conflict

-- | "Conflict" relationship between two entities.
-- It's enough to define one of 'conflict' and 'agree' functions.
class Conflict a b where
    -- | Whether entities conflict.
    conflicts :: a -> b -> Bool
    conflicts a b = not (agrees a b)

    -- | The opposite to 'conflict'.
    agrees :: a -> b -> Bool
    agrees a b = not (conflicts a b)

type SuperConflict a b =
    ( Conflict a a
    , Conflict a b
    , Conflict b a
    , Conflict b b
    )

-- | Command can be either accepted or denied.
data Acceptance cmd
    = Accepted cmd
    | Rejected cmd
    deriving (Eq, Ord, Generic)

-- | Command rejection doesn't conflict with any other command.
instance Conflict a a => Conflict (Acceptance a) (Acceptance a) where
    Accepted cmd1 `conflicts` Accepted cmd2 = conflicts cmd1 cmd2
    _ `conflicts` _ = False

instance MessagePack p => MessagePack (Acceptance p)

-- * Commands & cstructs

-- | Defines basic operations with commands and cstructs.
-- Requires "conflict" relationship to be defined for them,
-- and "bottom" cstruct to exist.
class (SuperConflict cmd cstruct, Default cstruct) =>
      Command cmd cstruct | cstruct -> cmd, cmd -> cstruct where

    -- | Add command to CStruct, if no conflict arise.
    addCommand :: cmd -> cstruct -> Maybe cstruct

    -- | Calculate Greatest Lower Bound of two cstructs.
    -- Fails, if two cstructs have conflicting commands.
    glb :: cstruct -> cstruct -> Maybe cstruct

    -- | Calculate Least Upper Bound of two cstructs.
    -- This function is always defined.
    lub :: cstruct -> cstruct -> cstruct

    -- | @extends c1 c2@ is true iff @glb c c2 = c1@ for some @c@.
    extends :: cstruct -> cstruct -> Bool

-- | Utility function, which unsures that arguments being combined does not
-- conflict.
checkingAgreement :: Conflict a b => (a -> b -> c) -> a -> b -> Maybe c
checkingAgreement f a b = guard (agrees a b) $> f a b

-- | Try to add command to cstruct; on fail add denial of that command.
acceptOrRejectCommand
    :: Command (Acceptance cmd) cstruct
    => cmd -> cstruct -> cstruct
acceptOrRejectCommand cmd cstruct =
    fromMaybe (error "failed to add command rejection") $
        addCommand (Accepted cmd) cstruct
    <|> addCommand (Rejected cmd) cstruct
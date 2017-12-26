{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Interface for commands and cstructs.

module Sdn.Base.CStruct where

import           Control.Lens         (makePrisms)
import           Control.Monad.Except (MonadError, throwError)
import           Data.Default         (Default (..))
import           Data.MessagePack     (MessagePack)
import qualified Data.Text.Buildable
import           Formatting           (bprint, build, sformat, (%))
import           Test.QuickCheck      (Arbitrary (..), elements)
import           Universum

import           Sdn.Base.Quorum
import           Sdn.Base.Settings
import           Sdn.Extra.Util

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

-- | Whether cstruct conflicts with itself.
contradictive :: Conflict a a => a -> Bool
contradictive = join conflicts

-- | Opposite to `contradictive`.
consistent :: Conflict a a => a -> Bool
consistent = join agrees

type SuperConflict a b =
    ( Conflict a a
    , Conflict a b
    , Conflict b a
    , Conflict b b
    )

-- | Command can be either accepted or denied.
--
-- Note: its @instance Ord@ implies that 'AcceptedT' is smaller
-- than 'RejectedT' because one may want to try it first.
data AcceptanceType
    = AcceptedT
    | RejectedT
    deriving (Eq, Ord, Enum)

-- | Acception or denial of command.
data Acceptance cmd
    = Accepted cmd
    | Rejected cmd
    deriving (Eq, Ord, Show, Generic)

makePrisms ''Acceptance

acceptanceCmd :: Acceptance cmd -> cmd
acceptanceCmd = \case
    Accepted cmd -> cmd
    Rejected cmd -> cmd

acceptanceType :: Acceptance cmd -> AcceptanceType
acceptanceType = \case
    Accepted _ -> AcceptedT
    Rejected _ -> RejectedT

-- | Command rejection doesn't conflict with any other command.
instance (Conflict a a, Eq a) => Conflict (Acceptance a) (Acceptance a) where
    Accepted cmd1 `conflicts` Accepted cmd2 = conflicts cmd1 cmd2
    Accepted cmd1 `conflicts` Rejected cmd2 = cmd1 == cmd2
    Rejected cmd1 `conflicts` Accepted cmd2 = cmd1 == cmd2
    Rejected _ `conflicts` Rejected _ = False

instance Buildable p => Buildable (Acceptance p) where
    build = \case
        Accepted p -> bprint ("+ "%build) p
        Rejected p -> bprint ("xx "%build) p

instance Arbitrary a => Arbitrary (Acceptance a) where
    arbitrary = elements [Accepted, Rejected] <*> arbitrary

instance MessagePack p => MessagePack (Acceptance p)

-- * Commands & cstructs

-- | Defines basic operations with commands and cstructs.
-- Requires "conflict" relationship to be defined for them,
-- and "bottom" cstruct to exist.
class (SuperConflict cmd cstruct, Default cstruct, Buildable cstruct) =>
      Command cstruct cmd | cstruct -> cmd, cmd -> cstruct where

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

    -- | Returns cstruct with all commands, which are present in votes
    -- from all acceptors of some quorum.
    -- Fails if resulting cstruct is contradictory.
    combination
        :: (HasMembers, QuorumFamily qf)
        => Votes qf cstruct -> Either Text cstruct
    combination = combinationDefault

    -- | Returns cstruct with all commands, which are present in votes
    -- of all acceptors of intersection of given quorum with some other quorum.
    -- Fails if resulting cstruct is contradictory.
    intersectingCombination
        :: (HasMembers, QuorumIntersectionFamily qf)
        => Votes qf cstruct -> Either Text cstruct
    intersectingCombination = intersectingCombinationDefault


-- | Construct cstruct from single command.
liftCommand
    :: Command cstruct (Acceptance cmd)
    => Acceptance cmd -> cstruct
liftCommand cmd =
    fromMaybe (error "Can't make up cstruct from single command") $
    addCommand cmd def

-- | Whether sctruct contains command, accepted or rejected.
contains :: Command cstruct (Acceptance cmd) => cstruct -> cmd -> Bool
contains cstruct cmd =
    any (\acc -> cstruct `extends` liftCommand (acc cmd))
    [Accepted, Rejected]

-- | Utility function, which unsures that arguments being combined does not
-- conflict.
checkingAgreement :: Conflict a b => (a -> b -> c) -> a -> b -> Maybe c
checkingAgreement f a b = guard (agrees a b) $> f a b

checkingConsistency
    :: (Conflict a a, Buildable a, MonadError Text m)
    => a -> m a
checkingConsistency x
    | contradictive x = throwError $ "got contradictive cstruct: " <> pretty x
    | otherwise       = pure x

-- | Try to add command to cstruct; on fail add denial of that command.
acceptOrRejectCommand
    :: Command cstruct (Acceptance cmd)
    => cmd -> cstruct -> cstruct
acceptOrRejectCommand cmd cstruct =
    fromMaybe (error "failed to add command rejection") $
        addCommand (Accepted cmd) cstruct
    <|> addCommand (Rejected cmd) cstruct

-- | Take list of lists of cstructs, 'lub's inner lists and then 'gdb's results.
-- None of given 'cstruct's should be empty.
mergeCStructs
    :: (Container cstructs, cstruct ~ Element cstructs, Command cstruct cmd)
    => [cstructs] -> Either Text cstruct
mergeCStructs cstructs =
    let gamma = map (foldr1 lub . toList) cstructs
        combined = foldrM glb def gamma
    in  maybe (errorContradictory gamma) pure combined
  where
    errorContradictory gamma =
        throwError $
        sformat ("mergeCStructs: got contradictory Gamma: "%listF "\n  ," build)
            gamma

-- | Takes first argument only if it is extension of second one.
maxOrSecond :: Command cstruct cmd => cstruct -> cstruct -> cstruct
maxOrSecond c1 c2
    | c1 `extends` c2 = c1
    | otherwise       = c2

-- | This is straightforward and very inefficient implementation of
-- 'combination'.
combinationDefault
    :: (HasMembers, Command cstruct cmd, QuorumFamily qf)
    => Votes qf cstruct -> Either Text cstruct
combinationDefault votes =
    mergeCStructs $ allMinQuorumsOf votes

-- | This is straightforward and very inefficient implementation of
-- 'intersectingCombination'.
intersectingCombinationDefault
    :: (HasMembers, Command cstruct cmd, QuorumIntersectionFamily qf)
    => Votes qf cstruct -> Either Text cstruct
intersectingCombinationDefault =
    mergeCStructs . filter (not . null) . getQuorumsSubIntersections

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Interface for commands and cstructs.

module Sdn.Base.CStruct where

import           Control.Lens         (Index, Ixed, makePrisms)
import           Control.Monad.Except (MonadError, throwError)
import           Data.Default         (Default (..))
import           Data.MessagePack     (MessagePack)
import qualified Data.Text.Buildable
import           Formatting           (bprint, build, sformat, (%))
import           Test.QuickCheck      (Arbitrary (..), elements)
import           Universum

import           Sdn.Base.Quorum
import           Sdn.Base.Settings
import           Sdn.Extra.Util       (DeclaredMark, Decomposable (..), MonadicMark (..),
                                       listF)

-- * Conflict

-- | "Conflict" relationship between two entities.
-- It's enough to define one of 'conflict' and 'agree' functions.
class Conflict a b where
    -- | Whether entities conflict, with reason
    conflictReason :: a -> b -> Either Text ()
    conflictReason a b =
        if a `conflicts` b
        then Left "some conflict"
        else Right ()

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
    deriving (Eq, Show, Ord, Enum)

instance Buildable AcceptanceType where
    build = \case
        AcceptedT -> "accepted"
        RejectedT -> "rejected"

-- | Acception or denial of command.
data Acceptance cmd
    = Accepted cmd
    | Rejected cmd
    deriving (Eq, Ord, Show, Generic)

makePrisms ''Acceptance

instance Decomposable (Acceptance cmd) (AcceptanceType, cmd) where
    decompose = \case
        Accepted cmd -> (AcceptedT, cmd)
        Rejected cmd -> (RejectedT, cmd)

    compose (acc, cmd) = case acc of
        AcceptedT -> Accepted cmd
        RejectedT -> Rejected cmd

acceptanceCmd :: Acceptance cmd -> cmd
acceptanceCmd = snd . decompose

acceptanceType :: Acceptance cmd -> AcceptanceType
acceptanceType = fst . decompose

type family UnAcceptance cmd where
    UnAcceptance (Acceptance a) = a

-- | Takes raw command.
-- E.g. when cstruct is network configuration, then true command is
-- @Acceptance Policy@ (because @Configuration@ consists from them),
-- while form in which policies are proposed (@Policy@) is "raw" command.
type RawCmd cstruct = UnAcceptance (Cmd cstruct)

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
class ( SuperConflict (Cmd cstruct) cstruct
      , Default cstruct
      , Buildable cstruct
      ) => CStruct cstruct where

    -- | Type of commands, cstruct is assembled from.
    type Cmd cstruct :: *

    -- | Add command to CStruct, if no conflict arise.
    addCommand :: Cmd cstruct -> cstruct -> Either Text cstruct

    -- | Calculate Greatest Lower Bound of two cstructs.
    -- Fails, if two cstructs have conflicting commands.
    glb :: cstruct -> cstruct -> Either Text cstruct

    -- | Calculate Least Upper Bound of two cstructs.
    -- This function is always defined.
    lub :: cstruct -> cstruct -> cstruct

    -- | @extends c1 c2@ is true iff @glb c c2 = c1@ for some @c@.
    extends :: cstruct -> cstruct -> Bool

    -- | @difference c1 c2@ returns all commands in @c1@ which are
    -- not present in @c2@.
    difference :: cstruct -> cstruct -> [Cmd cstruct]

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


-- | 'CStruct', where commands are 'Acceptance's.
type CStructA cstruct cmd = (CStruct cstruct, Cmd cstruct ~ Acceptance cmd)

-- | Construct cstruct from single command.
liftCommand
    :: (CStruct cstruct, Cmd cstruct ~ Acceptance cmd)
    => Acceptance cmd -> cstruct
liftCommand cmd =
    fromRight (error "Can't make up cstruct from single command") $
    addCommand cmd def

-- | Whether sctruct contains command, accepted or rejected.
contains :: CStructA cstruct cmd => cstruct -> cmd -> Bool
contains cstruct cmd =
    any (\acc -> cstruct `extends` liftCommand (acc cmd))
    [Accepted, Rejected]

-- | Utility function, which unsures that arguments being combined does not
-- conflict.
checkingAgreement :: Conflict a b => (a -> b -> c) -> a -> b -> Either Text c
checkingAgreement f a b = conflictReason a b $> f a b

checkingConsistency
    :: (Conflict a a, Buildable a, MonadError Text m)
    => a -> m a
checkingConsistency x
    | contradictive x = throwError $ "got contradictive cstruct: " <> pretty x
    | otherwise       = pure x

-- | Try to add command to cstruct; on fail add denial of that command.
-- Returns acceptance/denial of command which fit and new cstruct.
acceptOrRejectCommand
    :: CStructA cstruct cmd
    => cmd -> cstruct -> (Acceptance cmd, cstruct)
acceptOrRejectCommand cmd cstruct =
    case try Accepted of
        Right x -> x
        Left _ -> case try Rejected of
            Right x -> x
            Left _  -> error "failed to add command rejection"
  where
    try acceptance =
        let acmd = acceptance cmd
        in  (acmd, ) <$> addCommand acmd cstruct

-- | 'State' version of 'acceptOrRejectCommand'.
acceptOrRejectCommandS
    :: (Monad m, CStructA cstruct cmd)
    => cmd -> StateT cstruct m (Acceptance cmd)
acceptOrRejectCommandS = state . acceptOrRejectCommand

-- | Take list of lists of cstructs, 'lub's inner lists and then 'gdb's results.
-- None of given 'cstruct's should be empty.
mergeCStructs
    :: (Container cstructs, cstruct ~ Element cstructs, CStruct cstruct)
    => [cstructs] -> Either Text cstruct
mergeCStructs cstructs =
    let gamma = map (foldr1 lub . toList) cstructs
        combined = foldrM glb def gamma
    in  either (errorContradictory gamma) pure combined
  where
    errorContradictory gamma err =
        throwError $
        sformat ("mergeCStructs: got contradictory Gamma: "%listF "\n  ," build
                %"\n  : "%build)
            gamma err

-- | Takes first argument only if it is extension of second one.
maxOrSecond :: CStruct cstruct => cstruct -> cstruct -> cstruct
maxOrSecond c1 c2
    | c1 `extends` c2 = c1
    | otherwise       = c2

-- | This is straightforward and very inefficient implementation of
-- 'combination'.
combinationDefault
    :: (HasMembers, CStruct cstruct, QuorumFamily qf)
    => Votes qf cstruct -> Either Text cstruct
combinationDefault votes =
    mergeCStructs $ allMinQuorumsOf votes

-- | This is straightforward and very inefficient implementation of
-- 'intersectingCombination'.
intersectingCombinationDefault
    :: (HasMembers, CStruct cstruct, QuorumIntersectionFamily qf)
    => Votes qf cstruct -> Either Text cstruct
intersectingCombinationDefault =
    mergeCStructs . filter (not . null) . getQuorumsSubIntersections


-- | Declares that implementation of cstruct has many other practically useful
-- instances.
-- Generally, all this constraints are needed for distributed protocol.
class ( CStruct cstruct
      , Buildable cstruct
      , Buildable (RawCmd cstruct)
      , Ord (RawCmd cstruct)
      , Eq cstruct  -- TODO: remove?
      , MessagePack cstruct
      , MessagePack (RawCmd cstruct)
      , Acceptance (RawCmd cstruct) ~ Cmd cstruct
      , Ixed cstruct
      , Index cstruct ~ Cmd cstruct
      ) =>
      PracticalCStruct cstruct


-- | Contains type of cstruct, used to be passed to 'MonadicMark'.
data CStructType cstruct

-- | Monad transformer to bind cstruct type to monadic stack.
type CStructDecl store = MonadicMark (CStructType store)

-- | Get type of cstruct in given monad.
type DeclaredCStruct m = DeclaredMark CStructType m

-- | Get type of command in given monad.
type DeclaredCmd m = Cmd (DeclaredCStruct m)

-- | Get type of raw command in given monad.
type DeclaredRawCmd m = RawCmd (DeclaredCStruct m)

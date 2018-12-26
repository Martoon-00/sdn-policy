{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Various types - helpers for node states.

module Sdn.Protocol.Common.Context.Data
    ( ForBothRoundTypes (..)
    , forClassic
    , forFast
    , bothRoundsF

    , PerBallot
    , PerBallots
    , ballotMapF

    , ProposedCommands (..)
    , ballotProposedCommands
    , pendingProposedCommands
    , rememberProposedCommandsAt
    , dumpProposedCommands

    , PerCmdVotes

    , CStructStore (..)
    , coreCStruct
    , totalCStruct
    , addUnstableCmd
    , addUnstableCmds
    , extendCoreCStruct
    , acceptOrRejectIntoStoreS
    ) where

import           Control.Exception      (assert)
import           Control.Lens           (At (..), Index, IxValue, Ixed, makeLenses, (.=), (<<.=))
import           Data.Default           (Default (..))
import qualified Data.Set               as S
import qualified Data.Text.Buildable
import           Data.Text.Lazy.Builder (Builder)
import           Formatting             (Format, bprint, build, later, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra.Util         (listF, pairF, presence)

-- | Keeps instances for classic and fast versions of algorithm separately.
data ForBothRoundTypes a = ForBothRoundTypes
    { _forClassic :: a
    , _forFast    :: a
    }

makeLenses ''ForBothRoundTypes

instance Monoid a => Monoid (ForBothRoundTypes a) where
    mempty = ForBothRoundTypes mempty mempty
    ForBothRoundTypes a1 b1 `mappend` ForBothRoundTypes a2 b2
        = ForBothRoundTypes (a1 <> a2) (b1 <> b2)

instance Default a => Default (ForBothRoundTypes a) where
    def = ForBothRoundTypes def def

bothRoundsF :: Format Builder (a -> Builder) -> Format r (ForBothRoundTypes a -> r)
bothRoundsF fmt = later $ \ForBothRoundTypes{..} ->
    bprint ("_fast_: "%fmt) _forClassic <>
    bprint ("\n  _classic_: "%fmt) _forFast

type PerBallot a = Map BallotId a
type PerBallots a = ForBothRoundTypes $ PerBallot a

-- | Formatter for ballot id map
ballotMapF
    :: Buildable BallotId
    => Format Builder (a -> Builder) -> Format r (PerBallot a -> r)
ballotMapF f = listF "\n  " (pairF (build%": "%f))


-- | This is where commands from proposer are stored.
-- We need to cache all policies proposed upon specified ballot.
data ProposedCommands a = ProposedCommands
    { _ballotProposedCommands  :: PerBallot [a]
      -- ^ Commands proposed upon given ballots.
    , _pendingProposedCommands :: S.Set a
      -- ^ Proposed commands to attach to next ballot.
    }

makeLenses ''ProposedCommands

instance Default (ProposedCommands a) where
    def = ProposedCommands mempty def

instance (Buildable a, Ord a) => Buildable (ProposedCommands a) where
    build ProposedCommands{..} =
        bprint ("pending: "%listF ", " build%"\n"
               %"fixed: "%ballotMapF (listF ", " build))
            _pendingProposedCommands
            _ballotProposedCommands

instance Ixed (ProposedCommands a) where
instance At (ProposedCommands a) where
    at i = ballotProposedCommands . at i
type instance Index (ProposedCommands a) = Index (PerBallot [a])
type instance IxValue (ProposedCommands a) = IxValue (PerBallot [a])

rememberProposedCommandsAt :: BallotId -> ProposedCommands a -> ProposedCommands a
rememberProposedCommandsAt ballotId = execState $ do
    pending <- pendingProposedCommands <<.= def
    ballotProposedCommands . at ballotId .= Just (toList pending)

dumpProposedCommands :: MonadState (ProposedCommands a) m => BallotId -> m (S.Set a)
dumpProposedCommands ballotId =
    use pendingProposedCommands <* modify (rememberProposedCommandsAt ballotId)


-- | For each policy, keeps votes on its acceptance or not.
type PerCmdVotes qf cstruct = Map (RawCmd cstruct) $ Votes qf AcceptanceType


-- | Storage for cstruct with several level of application "unstableness".
data CStructStore cstruct = CStructStore
    { _storedCoreCStruct  :: cstruct
      -- ^ Core cstruct contains whose policies which are proved to be fixated
      -- (accepted by a quorum of acceptors).
      -- Currently it is updated by leader only, since it does the work of
      -- collecting cstructs of acceptors and judging about conflicting/fixated
      -- policies.

    , _storedUnstableCmds :: Set (Cmd cstruct)
      -- ^ These are commands which have been proposed and were somehow applied
      -- by local node. Still, majority may make another decision, thus
      -- this set of policies is called "unstable".
      -- Unstable and core sets are always disjoint, and
      -- in all cases core set has priority to preserve a policy in it.
      --
      -- Other nodes always see sum of core and unstable sets stored at acceptor,
      -- see 'totalCStruct'. Generally, these sets are separated in order to,
      -- when leader extends cstruct, find and remove conficting policies.

    , _storedTotalCStruct :: cstruct
      -- ^ Exact union of '_storedCoreCStruct' and '_storedUnstableCmds',
      -- for performance purposes.
    }

makeLenses ''CStructStore

deriving instance (Eq cstruct, Eq (Cmd cstruct)) => Eq (CStructStore cstruct)
deriving instance (Show cstruct, Show (Cmd cstruct)) => Show (CStructStore cstruct)

instance (Buildable cstruct, Buildable (Cmd cstruct), Ord (Cmd cstruct)) =>
         Buildable (CStructStore cstruct) where
    build CStructStore {..} =
        bprint
            ("core: " %build % "; " %
             "unstable:" %listF ", " build)
            _storedCoreCStruct
            _storedUnstableCmds

instance Default cstruct => Default (CStructStore cstruct) where
    def = CStructStore def def def

-- | Safe getter for core.
coreCStruct :: CStructStore cstruct -> cstruct
coreCStruct = _storedCoreCStruct

-- | Construct sum of core and unstable sets.
totalCStruct :: PracticalCStruct cstruct => CStructStore cstruct -> cstruct
totalCStruct = _storedTotalCStruct
    -- either (error "core and unstable sets are contradictory!") identity $
    -- foldlM (flip addCommand) _storedCoreCStruct (S.toList _storedUnstableCmds)

-- | Recalculate cached total cstruct after one of other fields change.
reevalTotalCStruct
    :: (CStruct cstruct, HasCallStack)
    => CStructStore cstruct -> CStructStore cstruct
reevalTotalCStruct store@CStructStore{..} =
    store{ _storedTotalCStruct = foldl' attachUnsafe _storedCoreCStruct _storedUnstableCmds  }
  where
    attachUnsafe cstruct cmd = either error identity $ addCommand cmd cstruct

-- | Add a policy to unstable set.
-- If core policy already has this policy, with same acceptance or not,
-- nothing happens.
addUnstableCmd
    :: PracticalCStruct cstruct
    => Cmd cstruct -> CStructStore cstruct -> (Bool, CStructStore cstruct)
addUnstableCmd cmd store =
    if totalCStruct store `conflicts` cmd
        then (False, store)
        else (True, store & storedUnstableCmds . at cmd . presence .~ True
                          & storedTotalCStruct %~ either error identity . addCommand cmd)

-- | Multiple-version of 'addUnstableCmd'. Returns list of successfully added commands.
addUnstableCmds
    :: PracticalCStruct cstruct
    => [Cmd cstruct] -> CStructStore cstruct -> ([Cmd cstruct], CStructStore cstruct)
addUnstableCmds cmds initStore = foldl' attach ([], initStore) cmds
  where
    attach (!applied, !store) cmd =
        let (added, store') = addUnstableCmd cmd store
        in  (if added then cmd : applied else applied, store')

-- | Replaces core cstruct. It's only valid to extend core cstruct,
-- otherwise error happens.
-- All added policies, if were present in unstable set, will be removed from there.
extendCoreCStruct
    :: PracticalCStruct cstruct
    => cstruct -> CStructStore cstruct -> Either Text (CStructStore cstruct)
extendCoreCStruct newCStruct store = do
    unless (newCStruct `extends` _storedCoreCStruct store) $
        Left "Cannot replace core, specified cstruct doesn't extend current one"

    let diff = newCStruct `difference` _storedCoreCStruct store
    return $ store
           & storedCoreCStruct .~ newCStruct
           & storedUnstableCmds %~ S.filter
               (\cmd -> all (agrees cmd) diff && all (/= cmd) diff)
           & reevalTotalCStruct

-- | Apply policy to store somehow, and return how it was applied.
-- Policy will be added to unstable set.
--
-- If decision on policy was already present, it is returned
-- (it may be better to return Nothing in this case, but let it be for now).
acceptOrRejectIntoStoreS
    :: (PracticalCStruct cstruct, MonadState (CStructStore cstruct) m)
    => RawCmd cstruct -> m (Cmd cstruct)
acceptOrRejectIntoStoreS policy = do
    total <- gets totalCStruct
    let (policyAcceptance, _) = acceptOrRejectCommand policy total
    modify $ \store ->
        let (ok, store') = addUnstableCmd policyAcceptance store
        in assert ok store'
    storedUnstableCmds . at policyAcceptance . presence .= True
    return policyAcceptance

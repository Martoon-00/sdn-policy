{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Various types - helpers for node states.

module Sdn.Protocol.Common.Context.Types where

import           Control.Lens           (At (..), Index, IxValue, Ixed, makeLenses, (.=),
                                         (<<.=))
import           Data.Default           (Default (..))
import qualified Data.Text.Buildable
import           Data.Text.Lazy.Builder (Builder)
import           Formatting             (Format, bprint, build, later, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra.Util         (listF, pairF)

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
    , _pendingProposedCommands :: [a]
      -- ^ Proposed commands to attach to next ballot.
    }

makeLenses ''ProposedCommands

instance Default (ProposedCommands a) where
    def = ProposedCommands mempty mempty

instance Buildable a => Buildable (ProposedCommands a) where
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
    pending <- pendingProposedCommands <<.= []
    ballotProposedCommands . at ballotId .= Just pending

dumpProposedCommands :: MonadState (ProposedCommands a) m => BallotId -> m [a]
dumpProposedCommands ballotId =
    use pendingProposedCommands <* modify (rememberProposedCommandsAt ballotId)


-- | For each policy, keeps votes on its acceptance or not.
type PerCmdVotes qf cstruct = Map (RawCmd cstruct) $ Votes qf AcceptanceType


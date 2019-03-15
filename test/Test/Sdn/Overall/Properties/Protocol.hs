{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

-- | Various useful properties for the protocol.

module Test.Sdn.Overall.Properties.Protocol where

import           Control.Lens                     (Prism', has, traversed)
import           Control.Monad.Error.Class        (throwError)
import qualified Data.Map                         as M
import qualified Data.Set                         as S
import           Formatting                       (build, sformat, text, (%))
import           Universum

import           Sdn.Base
import           Sdn.Extra                        (listF, viewListOf)
import           Sdn.Policy.Fake
import           Sdn.Protocol
import           Test.Sdn.Overall.Properties.Util


-- * Property primitives

proposedPoliciesWereLearned :: PropertyChecker pv Configuration
proposedPoliciesWereLearned = do
    proposed's <- view $ proposerState . proposerProposedPolicies
    learned's <- viewListOf $ learnersStates . traversed . learnerLearned

    forM_ proposed's $ \p ->
        forM_ (zip [1..] learned's) $ \(learnerId, learned) ->
            unless (learned `contains` p) $
               failProp p learnerId
  where
    failProp p (li :: Int) =
        throwError $
        sformat ("Proposed "%build%" wasn't leart by learner "%build) p li

learnedPoliciesWereProposed :: PropertyChecker pv Configuration
learnedPoliciesWereProposed = do
    proposed's <- view $ proposerState . proposerProposedPolicies
    let validOutcomes = S.fromList $ [Accepted, Rejected] <*> proposed's
    learned's <- viewListOf $ learnersStates . traversed . learnerLearned
    forM_ (zip [1..] learned's) $ \(learnerId, learned) ->
        forM_ learned $ \l ->
            unless (l `S.member` validOutcomes) $ failProp l learnerId
  where
    failProp p (li :: Int) =
        throwError $
        sformat ("Learned "%build%" by "%build%" was never proposed") p li

learnersAgree :: PropertyChecker pv Configuration
learnersAgree = do
    learned <- viewListOf $ learnersStates . traversed . learnerLearned
    l :| ls <- maybe (throwError "No learners") pure $ nonEmpty learned
    forM_ ls $ \l' ->
        when (l /= l') $ throwError "learners disagree"

-- | Checks that number of learned policies matches predicate.
numberOfLearnedPolicies :: (Prism' (Decision Policy) a)
                        -> (Word -> Bool)
                        -> PropertyChecker pv Configuration
numberOfLearnedPolicies predicate cmp = do
    learned's <- viewListOf $ learnersStates . traversed . learnerLearned
    forM_ (zip [1..] learned's) $ \(learnerId, learned) -> do
        let fit = filter (has predicate) $ toList learned
            ok = cmp $ fromIntegral (length fit)
        unless ok $ failProp learnerId learned
  where
    failProp (li :: Int) l =
        throwError $
        sformat ("Unexpected number of learned policies for learner "%build
                %", but "%build%" are present:"%build) li (length l) l

recoveryWasUsed :: PracticalCStruct cstruct => Bool -> PropertyChecker pv cstruct
recoveryWasUsed shouldBeUsed = do
    -- in Fast version all proposals which leader receives occur due to recovery
    recoveries <- view $ leaderState . leaderProposedPolicies . ballotProposedCommands
    let recoveryOccured = not $ all null recoveries
    unless (recoveryOccured == shouldBeUsed) $
        failProp recoveries
  where
    failProp recoveries =
        throwError $
        sformat ("Got recoveries at ballots "%listF ", " build
                %", despite they were "%text%"expected")
            (M.keys recoveries) (if shouldBeUsed then "" else "un")


-- * Properties groups

basicProperties
    :: (MonadIO m, DeclaredCStruct m ~ Configuration)
    => [ProtocolProperty pv m]
basicProperties =
    [ eventually proposedPoliciesWereLearned
    , invariant learnedPoliciesWereProposed
    , eventually learnersAgree
    ]

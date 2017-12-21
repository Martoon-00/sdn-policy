{-# LANGUAGE Rank2Types #-}

-- | Various useful properties for the protocol.

module Test.Sdn.Overall.Properties.Protocol where

import           Control.Lens                     (Prism', has)
import           Control.Monad.Error.Class        (throwError)
import qualified Data.Set                         as S
import           Formatting                       (build, sformat, (%))
import           Universum

import           Sdn.Base
import           Sdn.Protocol
import           Test.Sdn.Overall.Properties.Util


-- * Property primitives

proposedPoliciesWereLearned :: PropertyChecker pv
proposedPoliciesWereLearned AllStates{..} = do
    let proposed's = _proposerProposedPolicies proposerState
    let learned's = _learnerLearned <$> learnersStates

    forM_ proposed's $ \p ->
        forM_ (zip [1..] learned's) $ \(learnerId, learned) ->
            unless (learned `contains` p) $
               failProp p learnerId
  where
    failProp p (li :: Int) =
        throwError $
        sformat ("Proposed "%build%" wasn't leart by learner "%build) p li

learnedPoliciesWereProposed :: PropertyChecker pv
learnedPoliciesWereProposed AllStates{..} = do
    let proposed's = _proposerProposedPolicies proposerState
    let validOutcomes = S.fromList $ [Accepted, Rejected] <*> proposed's
    let learned's = _learnerLearned <$> learnersStates
    forM_ (zip [1..] learned's) $ \(learnerId, learned) ->
        forM_ learned $ \l ->
            unless (l `S.member` validOutcomes) $ failProp l learnerId
  where
    failProp p (li :: Int) =
        throwError $
        sformat ("Learned "%build%" by "%build%" was never proposed") p li

learnersAgree :: PropertyChecker pv
learnersAgree AllStates{..} = do
    let learned = _learnerLearned <$> learnersStates
    l :| ls <- maybe (Left "No learners") Right $ nonEmpty learned
    forM_ ls $ \l' ->
        when (l /= l') $ Left "learners disagree"

-- | Checks that number of learned policies matches predicate.
numberOfLearnedPolicies :: (Prism' (Acceptance Policy) a)
                        -> (Word -> Bool)
                        -> PropertyChecker pv
numberOfLearnedPolicies predicate cmp AllStates{..} = do
    let learned's = _learnerLearned <$> learnersStates
    forM_ (zip [1..] learned's) $ \(learnerId, learned) -> do
        let fit = filter (has predicate) $ toList learned
            ok = cmp $ fromIntegral (length fit)
        unless ok $ failProp learnerId learned
  where
    failProp (li :: Int) l =
        throwError $
        sformat ("Unexpected number of learned policies for learner "%build
                %", but "%build%" are present:"%build) li (length l) l


-- * Properties groups

basicProperties :: MonadIO m => [ProtocolProperty pv m]
basicProperties =
    [ eventually proposedPoliciesWereLearned
    , invariant learnedPoliciesWereProposed
    , eventually learnersAgree
    ]

{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Fake policies with various conflicting behaviour.

module Sdn.Policy.Fake
    ( Policy (..)
    , PolicyName
    , policyName

    , Configuration
    , mkConfig
    ) where

import           Data.MessagePack      (MessagePack (..))
import qualified Data.Text.Buildable
import           Formatting            (bprint, build, (%))
import           Test.QuickCheck       (Arbitrary (..), getNonNegative, oneof, resize, suchThat)
import           Universum

import           Sdn.Base.CStruct
import           Sdn.Base.SimpleConfig

newtype PolicyName = PolicyName Int
    deriving (Eq, Ord, Show, Num, Buildable, MessagePack)

instance Arbitrary PolicyName where
    arbitrary = PolicyName <$> arbitrary `suchThat` (>= 0)

-- | Abstract SDN policy.
data Policy
    = GoodPolicy PolicyName       -- ^ Agrees with any other one
    | BadPolicy PolicyName        -- ^ Conflicts with any other one
    | MoodyPolicy Int PolicyName  -- ^ Conflicts if group ids are equal
    deriving (Eq, Ord, Show, Generic)

instance Buildable Policy where
    build = \case
        GoodPolicy name -> bprint ("Good \""%build%"\"") name
        BadPolicy name -> bprint ("Bad \""%build%"\"") name
        MoodyPolicy id name -> bprint ("Moody #"%build%" \""%build%"\"") id name

policyName :: Policy -> PolicyName
policyName = \case
    GoodPolicy name    -> name
    BadPolicy name     -> name
    MoodyPolicy _ name -> name

instance Conflict Policy Policy where
    agrees a b                                     | a == b                            = True
    agrees GoodPolicy{} _                          = True
    agrees _ GoodPolicy{}                          = True
    agrees BadPolicy{} _                           = False
    agrees _ BadPolicy{}                           = False
    agrees (MoodyPolicy id1 _) (MoodyPolicy id2 _) = id1 /= id2

instance Arbitrary Policy where
    arbitrary =
        oneof
        [ pure GoodPolicy
        , pure BadPolicy
        , MoodyPolicy <$> resize 5 (getNonNegative <$> arbitrary)
        ]
        <*>
        resize 5 arbitrary

instance MessagePack Policy

instance MayHaveProposerId Policy where
    cmdProposerId _ = Nothing

type Configuration = SimpleConfig Policy

mkConfig :: [Acceptance Policy] -> Maybe Configuration
mkConfig = mkSimpleConfig

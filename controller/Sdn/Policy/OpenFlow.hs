{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Network policies and configuration for OpenFlow controllers.

module Sdn.Policy.OpenFlow
    ( Policy (..)
    , Configuration
    ) where

import           Control.Lens          (at, makeLenses, non)
import qualified Data.Binary           as Bin
import           Data.Default          (Default (..))
import qualified Data.Map              as M
import           Data.MessagePack      (MessagePack (..))
import qualified Data.Set              as S
import qualified Data.Text.Buildable
import           Formatting            (bprint, build, sformat, shown, (%))
import qualified Network.Data.OpenFlow as OF
import           Universum

import           Sdn.Base
import           Sdn.Extra.Util        (listF, pairF, presence)

data Policy = Policy
    { policyXid    :: OF.TransactionID
    , policyAction :: [OF.Action]
    } deriving (Eq, Ord, Show, Generic)

instance Buildable Policy where
    build = bprint shown

instance Conflict Policy Policy where
    conflictReason (Policy xid1 action1) (Policy xid2 action2) =
        let pack action = M.fromList $ map (\a -> (OF.actionToType a, a)) action
            [at1, at2] = map pack [action1, action2]
            actionsOfSameTypeAreSame = M.intersectionWith (==) at1 at2
        in  if xid1 == xid2 && and actionsOfSameTypeAreSame
            then Right ()
            else Left $ sformat ("Policies "%shown%" & "%shown%" conflict!")
                        action1 action2

instance MessagePack Policy where
    toObject (Policy xid action) = toObject $ Bin.encode (xid, action)
    fromObject o = do
        bin <- fromObject o
        (action, xid) <- case Bin.decodeOrFail bin of
            Left _          -> Nothing
            Right (_, _, x) -> Just x
        return (Policy action xid)


data Configuration = Configuration
    { _configEntry    :: M.Map OF.TransactionID $ S.Set Policy
    , _configRejected :: S.Set Policy
    } deriving (Eq, Generic)

makeLenses ''Configuration

instance Buildable Configuration where
    build Configuration{..} =
        bprint
            (  "Accepted: "%listF "\n  " (pairF ("for #"%build%": "%listF ", " build))%
            "\n Rejected: "%listF ", " build)
          _configEntry _configRejected

instance Default Configuration where
    def = Configuration mempty mempty

instance MessagePack Configuration

instance Conflict (Acceptance Policy) Configuration where
    conflictReason (Rejected _) _ = Right ()
    conflictReason (Accepted policy) Configuration{..} =
        case M.lookup (policyXid policy) _configEntry of
            Nothing          -> Right ()
            Just oldPolicies -> mapM_ (conflictReason policy) oldPolicies

instance Conflict Configuration (Acceptance Policy) where
    conflictReason = flip conflictReason


addCommandUnsafe :: Acceptance Policy -> Configuration -> Configuration
addCommandUnsafe (Rejected policy) = configRejected . at policy . presence .~ True
addCommandUnsafe (Accepted policy) =
    configEntry . at (policyXid policy) . non mempty . at policy . presence .~ True

instance CStruct Configuration where
    type Cmd Configuration = Acceptance Policy
    addCommand = checkingAgreement addCommandUnsafe
    glb = error "glb undefined"
    lub c1 c2 =
        Configuration
        { _configEntry = M.intersectionWith (S.intersection) (_configEntry c1) (_configEntry c2)
        , _configRejected = S.intersection (_configRejected c1) (_configRejected c2)
        }
    extends c1 c2 = and
        [ M.isSubmapOfBy (S.isSubsetOf) (_configEntry c2) (_configEntry c1)
        , S.isSubsetOf (_configRejected c2) (_configRejected c1)
        ]
    difference c1 c2 = mconcat
        [ map Accepted . toList $ fold (_configEntry c1) `S.difference` fold (_configEntry c2)
        , map Rejected . toList $ _configRejected c1 `S.difference` _configRejected c2
        ]


instance PracticalCStruct Configuration
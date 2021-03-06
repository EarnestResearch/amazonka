{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.DeploymentStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.DeploymentStatus (
  DeploymentStatus (
    ..
    , Completed
    , Eligible
    , InProgress
    , NotEligible
    , PendingUpdate
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DeploymentStatus = DeploymentStatus' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern Completed :: DeploymentStatus
pattern Completed = DeploymentStatus' "COMPLETED"

pattern Eligible :: DeploymentStatus
pattern Eligible = DeploymentStatus' "ELIGIBLE"

pattern InProgress :: DeploymentStatus
pattern InProgress = DeploymentStatus' "IN_PROGRESS"

pattern NotEligible :: DeploymentStatus
pattern NotEligible = DeploymentStatus' "NOT_ELIGIBLE"

pattern PendingUpdate :: DeploymentStatus
pattern PendingUpdate = DeploymentStatus' "PENDING_UPDATE"

{-# COMPLETE
  Completed,
  Eligible,
  InProgress,
  NotEligible,
  PendingUpdate,
  DeploymentStatus' #-}

instance FromText DeploymentStatus where
    parser = (DeploymentStatus' . mk) <$> takeText

instance ToText DeploymentStatus where
    toText (DeploymentStatus' ci) = original ci

-- | Represents an enum of /known/ $DeploymentStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeploymentStatus where
    toEnum i = case i of
        0 -> Completed
        1 -> Eligible
        2 -> InProgress
        3 -> NotEligible
        4 -> PendingUpdate
        _ -> (error . showText) $ "Unknown index for DeploymentStatus: " <> toText i
    fromEnum x = case x of
        Completed -> 0
        Eligible -> 1
        InProgress -> 2
        NotEligible -> 3
        PendingUpdate -> 4
        DeploymentStatus' name -> (error . showText) $ "Unknown DeploymentStatus: " <> original name

-- | Represents the bounds of /known/ $DeploymentStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeploymentStatus where
    minBound = Completed
    maxBound = PendingUpdate

instance Hashable     DeploymentStatus
instance NFData       DeploymentStatus
instance ToByteString DeploymentStatus
instance ToQuery      DeploymentStatus
instance ToHeader     DeploymentStatus

instance FromJSON DeploymentStatus where
    parseJSON = parseJSONText "DeploymentStatus"

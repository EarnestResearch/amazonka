{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.StackStatus (
  StackStatus (
    ..
    , SSCreateComplete
    , SSCreateFailed
    , SSCreateInProgress
    , SSDeleteComplete
    , SSDeleteFailed
    , SSDeleteInProgress
    , SSReviewInProgress
    , SSRollbackComplete
    , SSRollbackFailed
    , SSRollbackInProgress
    , SSUpdateComplete
    , SSUpdateCompleteCleanupInProgress
    , SSUpdateInProgress
    , SSUpdateRollbackComplete
    , SSUpdateRollbackCompleteCleanupInProgress
    , SSUpdateRollbackFailed
    , SSUpdateRollbackInProgress
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StackStatus = StackStatus' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern SSCreateComplete :: StackStatus
pattern SSCreateComplete = StackStatus' "CREATE_COMPLETE"

pattern SSCreateFailed :: StackStatus
pattern SSCreateFailed = StackStatus' "CREATE_FAILED"

pattern SSCreateInProgress :: StackStatus
pattern SSCreateInProgress = StackStatus' "CREATE_IN_PROGRESS"

pattern SSDeleteComplete :: StackStatus
pattern SSDeleteComplete = StackStatus' "DELETE_COMPLETE"

pattern SSDeleteFailed :: StackStatus
pattern SSDeleteFailed = StackStatus' "DELETE_FAILED"

pattern SSDeleteInProgress :: StackStatus
pattern SSDeleteInProgress = StackStatus' "DELETE_IN_PROGRESS"

pattern SSReviewInProgress :: StackStatus
pattern SSReviewInProgress = StackStatus' "REVIEW_IN_PROGRESS"

pattern SSRollbackComplete :: StackStatus
pattern SSRollbackComplete = StackStatus' "ROLLBACK_COMPLETE"

pattern SSRollbackFailed :: StackStatus
pattern SSRollbackFailed = StackStatus' "ROLLBACK_FAILED"

pattern SSRollbackInProgress :: StackStatus
pattern SSRollbackInProgress = StackStatus' "ROLLBACK_IN_PROGRESS"

pattern SSUpdateComplete :: StackStatus
pattern SSUpdateComplete = StackStatus' "UPDATE_COMPLETE"

pattern SSUpdateCompleteCleanupInProgress :: StackStatus
pattern SSUpdateCompleteCleanupInProgress = StackStatus' "UPDATE_COMPLETE_CLEANUP_IN_PROGRESS"

pattern SSUpdateInProgress :: StackStatus
pattern SSUpdateInProgress = StackStatus' "UPDATE_IN_PROGRESS"

pattern SSUpdateRollbackComplete :: StackStatus
pattern SSUpdateRollbackComplete = StackStatus' "UPDATE_ROLLBACK_COMPLETE"

pattern SSUpdateRollbackCompleteCleanupInProgress :: StackStatus
pattern SSUpdateRollbackCompleteCleanupInProgress = StackStatus' "UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS"

pattern SSUpdateRollbackFailed :: StackStatus
pattern SSUpdateRollbackFailed = StackStatus' "UPDATE_ROLLBACK_FAILED"

pattern SSUpdateRollbackInProgress :: StackStatus
pattern SSUpdateRollbackInProgress = StackStatus' "UPDATE_ROLLBACK_IN_PROGRESS"

{-# COMPLETE
  SSCreateComplete,
  SSCreateFailed,
  SSCreateInProgress,
  SSDeleteComplete,
  SSDeleteFailed,
  SSDeleteInProgress,
  SSReviewInProgress,
  SSRollbackComplete,
  SSRollbackFailed,
  SSRollbackInProgress,
  SSUpdateComplete,
  SSUpdateCompleteCleanupInProgress,
  SSUpdateInProgress,
  SSUpdateRollbackComplete,
  SSUpdateRollbackCompleteCleanupInProgress,
  SSUpdateRollbackFailed,
  SSUpdateRollbackInProgress,
  StackStatus' #-}

instance FromText StackStatus where
    parser = (StackStatus' . mk) <$> takeText

instance ToText StackStatus where
    toText (StackStatus' ci) = original ci

-- | Represents an enum of /known/ $StackStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StackStatus where
    toEnum i = case i of
        0 -> SSCreateComplete
        1 -> SSCreateFailed
        2 -> SSCreateInProgress
        3 -> SSDeleteComplete
        4 -> SSDeleteFailed
        5 -> SSDeleteInProgress
        6 -> SSReviewInProgress
        7 -> SSRollbackComplete
        8 -> SSRollbackFailed
        9 -> SSRollbackInProgress
        10 -> SSUpdateComplete
        11 -> SSUpdateCompleteCleanupInProgress
        12 -> SSUpdateInProgress
        13 -> SSUpdateRollbackComplete
        14 -> SSUpdateRollbackCompleteCleanupInProgress
        15 -> SSUpdateRollbackFailed
        16 -> SSUpdateRollbackInProgress
        _ -> (error . showText) $ "Unknown index for StackStatus: " <> toText i
    fromEnum x = case x of
        SSCreateComplete -> 0
        SSCreateFailed -> 1
        SSCreateInProgress -> 2
        SSDeleteComplete -> 3
        SSDeleteFailed -> 4
        SSDeleteInProgress -> 5
        SSReviewInProgress -> 6
        SSRollbackComplete -> 7
        SSRollbackFailed -> 8
        SSRollbackInProgress -> 9
        SSUpdateComplete -> 10
        SSUpdateCompleteCleanupInProgress -> 11
        SSUpdateInProgress -> 12
        SSUpdateRollbackComplete -> 13
        SSUpdateRollbackCompleteCleanupInProgress -> 14
        SSUpdateRollbackFailed -> 15
        SSUpdateRollbackInProgress -> 16
        StackStatus' name -> (error . showText) $ "Unknown StackStatus: " <> original name

-- | Represents the bounds of /known/ $StackStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StackStatus where
    minBound = SSCreateComplete
    maxBound = SSUpdateRollbackInProgress

instance Hashable     StackStatus
instance NFData       StackStatus
instance ToByteString StackStatus
instance ToQuery      StackStatus
instance ToHeader     StackStatus

instance FromXML StackStatus where
    parseXML = parseXMLText "StackStatus"

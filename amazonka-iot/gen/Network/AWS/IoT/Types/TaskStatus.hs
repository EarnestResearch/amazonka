{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TaskStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.TaskStatus (
  TaskStatus (
    ..
    , TSCancelled
    , TSCancelling
    , TSCompleted
    , TSFailed
    , TSInProgress
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TaskStatus = TaskStatus' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern TSCancelled :: TaskStatus
pattern TSCancelled = TaskStatus' "Cancelled"

pattern TSCancelling :: TaskStatus
pattern TSCancelling = TaskStatus' "Cancelling"

pattern TSCompleted :: TaskStatus
pattern TSCompleted = TaskStatus' "Completed"

pattern TSFailed :: TaskStatus
pattern TSFailed = TaskStatus' "Failed"

pattern TSInProgress :: TaskStatus
pattern TSInProgress = TaskStatus' "InProgress"

{-# COMPLETE
  TSCancelled,
  TSCancelling,
  TSCompleted,
  TSFailed,
  TSInProgress,
  TaskStatus' #-}

instance FromText TaskStatus where
    parser = (TaskStatus' . mk) <$> takeText

instance ToText TaskStatus where
    toText (TaskStatus' ci) = original ci

-- | Represents an enum of /known/ $TaskStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TaskStatus where
    toEnum i = case i of
        0 -> TSCancelled
        1 -> TSCancelling
        2 -> TSCompleted
        3 -> TSFailed
        4 -> TSInProgress
        _ -> (error . showText) $ "Unknown index for TaskStatus: " <> toText i
    fromEnum x = case x of
        TSCancelled -> 0
        TSCancelling -> 1
        TSCompleted -> 2
        TSFailed -> 3
        TSInProgress -> 4
        TaskStatus' name -> (error . showText) $ "Unknown TaskStatus: " <> original name

-- | Represents the bounds of /known/ $TaskStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TaskStatus where
    minBound = TSCancelled
    maxBound = TSInProgress

instance Hashable     TaskStatus
instance NFData       TaskStatus
instance ToByteString TaskStatus
instance ToQuery      TaskStatus
instance ToHeader     TaskStatus

instance ToJSON TaskStatus where
    toJSON = toJSONText

instance FromJSON TaskStatus where
    parseJSON = parseJSONText "TaskStatus"

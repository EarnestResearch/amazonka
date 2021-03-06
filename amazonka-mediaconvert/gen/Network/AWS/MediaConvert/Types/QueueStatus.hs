{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.QueueStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.QueueStatus (
  QueueStatus (
    ..
    , QSActive
    , QSPaused
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Queues can be ACTIVE or PAUSED. If you pause a queue, jobs in that queue won't begin. Jobs that are running when you pause a queue continue to run until they finish or result in an error.
data QueueStatus = QueueStatus' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern QSActive :: QueueStatus
pattern QSActive = QueueStatus' "ACTIVE"

pattern QSPaused :: QueueStatus
pattern QSPaused = QueueStatus' "PAUSED"

{-# COMPLETE
  QSActive,
  QSPaused,
  QueueStatus' #-}

instance FromText QueueStatus where
    parser = (QueueStatus' . mk) <$> takeText

instance ToText QueueStatus where
    toText (QueueStatus' ci) = original ci

-- | Represents an enum of /known/ $QueueStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum QueueStatus where
    toEnum i = case i of
        0 -> QSActive
        1 -> QSPaused
        _ -> (error . showText) $ "Unknown index for QueueStatus: " <> toText i
    fromEnum x = case x of
        QSActive -> 0
        QSPaused -> 1
        QueueStatus' name -> (error . showText) $ "Unknown QueueStatus: " <> original name

-- | Represents the bounds of /known/ $QueueStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded QueueStatus where
    minBound = QSActive
    maxBound = QSPaused

instance Hashable     QueueStatus
instance NFData       QueueStatus
instance ToByteString QueueStatus
instance ToQuery      QueueStatus
instance ToHeader     QueueStatus

instance ToJSON QueueStatus where
    toJSON = toJSONText

instance FromJSON QueueStatus where
    parseJSON = parseJSONText "QueueStatus"

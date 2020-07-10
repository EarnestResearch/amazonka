{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ExportTaskState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ExportTaskState (
  ExportTaskState (
    ..
    , ETSActive
    , ETSCancelled
    , ETSCancelling
    , ETSCompleted
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ExportTaskState = ExportTaskState' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern ETSActive :: ExportTaskState
pattern ETSActive = ExportTaskState' "active"

pattern ETSCancelled :: ExportTaskState
pattern ETSCancelled = ExportTaskState' "cancelled"

pattern ETSCancelling :: ExportTaskState
pattern ETSCancelling = ExportTaskState' "cancelling"

pattern ETSCompleted :: ExportTaskState
pattern ETSCompleted = ExportTaskState' "completed"

{-# COMPLETE
  ETSActive,
  ETSCancelled,
  ETSCancelling,
  ETSCompleted,
  ExportTaskState' #-}

instance FromText ExportTaskState where
    parser = (ExportTaskState' . mk) <$> takeText

instance ToText ExportTaskState where
    toText (ExportTaskState' ci) = original ci

-- | Represents an enum of /known/ $ExportTaskState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ExportTaskState where
    toEnum i = case i of
        0 -> ETSActive
        1 -> ETSCancelled
        2 -> ETSCancelling
        3 -> ETSCompleted
        _ -> (error . showText) $ "Unknown index for ExportTaskState: " <> toText i
    fromEnum x = case x of
        ETSActive -> 0
        ETSCancelled -> 1
        ETSCancelling -> 2
        ETSCompleted -> 3
        ExportTaskState' name -> (error . showText) $ "Unknown ExportTaskState: " <> original name

-- | Represents the bounds of /known/ $ExportTaskState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ExportTaskState where
    minBound = ETSActive
    maxBound = ETSCompleted

instance Hashable     ExportTaskState
instance NFData       ExportTaskState
instance ToByteString ExportTaskState
instance ToQuery      ExportTaskState
instance ToHeader     ExportTaskState

instance FromXML ExportTaskState where
    parseXML = parseXMLText "ExportTaskState"

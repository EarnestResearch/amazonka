{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AutoMLJobStatus (
  AutoMLJobStatus (
    ..
    , AMLJSCompleted
    , AMLJSFailed
    , AMLJSInProgress
    , AMLJSStopped
    , AMLJSStopping
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AutoMLJobStatus = AutoMLJobStatus' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern AMLJSCompleted :: AutoMLJobStatus
pattern AMLJSCompleted = AutoMLJobStatus' "Completed"

pattern AMLJSFailed :: AutoMLJobStatus
pattern AMLJSFailed = AutoMLJobStatus' "Failed"

pattern AMLJSInProgress :: AutoMLJobStatus
pattern AMLJSInProgress = AutoMLJobStatus' "InProgress"

pattern AMLJSStopped :: AutoMLJobStatus
pattern AMLJSStopped = AutoMLJobStatus' "Stopped"

pattern AMLJSStopping :: AutoMLJobStatus
pattern AMLJSStopping = AutoMLJobStatus' "Stopping"

{-# COMPLETE
  AMLJSCompleted,
  AMLJSFailed,
  AMLJSInProgress,
  AMLJSStopped,
  AMLJSStopping,
  AutoMLJobStatus' #-}

instance FromText AutoMLJobStatus where
    parser = (AutoMLJobStatus' . mk) <$> takeText

instance ToText AutoMLJobStatus where
    toText (AutoMLJobStatus' ci) = original ci

-- | Represents an enum of /known/ $AutoMLJobStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AutoMLJobStatus where
    toEnum i = case i of
        0 -> AMLJSCompleted
        1 -> AMLJSFailed
        2 -> AMLJSInProgress
        3 -> AMLJSStopped
        4 -> AMLJSStopping
        _ -> (error . showText) $ "Unknown index for AutoMLJobStatus: " <> toText i
    fromEnum x = case x of
        AMLJSCompleted -> 0
        AMLJSFailed -> 1
        AMLJSInProgress -> 2
        AMLJSStopped -> 3
        AMLJSStopping -> 4
        AutoMLJobStatus' name -> (error . showText) $ "Unknown AutoMLJobStatus: " <> original name

-- | Represents the bounds of /known/ $AutoMLJobStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AutoMLJobStatus where
    minBound = AMLJSCompleted
    maxBound = AMLJSStopping

instance Hashable     AutoMLJobStatus
instance NFData       AutoMLJobStatus
instance ToByteString AutoMLJobStatus
instance ToQuery      AutoMLJobStatus
instance ToHeader     AutoMLJobStatus

instance ToJSON AutoMLJobStatus where
    toJSON = toJSONText

instance FromJSON AutoMLJobStatus where
    parseJSON = parseJSONText "AutoMLJobStatus"

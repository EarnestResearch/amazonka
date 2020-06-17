{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TelemetryStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TelemetryStatus (
  TelemetryStatus (
    ..
    , Down
    , UP
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TelemetryStatus = TelemetryStatus' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Down :: TelemetryStatus
pattern Down = TelemetryStatus' "DOWN"

pattern UP :: TelemetryStatus
pattern UP = TelemetryStatus' "UP"

{-# COMPLETE
  Down,
  UP,
  TelemetryStatus' #-}

instance FromText TelemetryStatus where
    parser = (TelemetryStatus' . mk) <$> takeText

instance ToText TelemetryStatus where
    toText (TelemetryStatus' ci) = original ci

-- | Represents an enum of /known/ $TelemetryStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TelemetryStatus where
    toEnum i = case i of
        0 -> Down
        1 -> UP
        _ -> (error . showText) $ "Unknown index for TelemetryStatus: " <> toText i
    fromEnum x = case x of
        Down -> 0
        UP -> 1
        TelemetryStatus' name -> (error . showText) $ "Unknown TelemetryStatus: " <> original name

-- | Represents the bounds of /known/ $TelemetryStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TelemetryStatus where
    minBound = Down
    maxBound = UP

instance Hashable     TelemetryStatus
instance NFData       TelemetryStatus
instance ToByteString TelemetryStatus
instance ToQuery      TelemetryStatus
instance ToHeader     TelemetryStatus

instance FromXML TelemetryStatus where
    parseXML = parseXMLText "TelemetryStatus"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264LookAheadRateControl
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H264LookAheadRateControl (
  H264LookAheadRateControl (
    ..
    , HLARCHigh
    , HLARCLow
    , HLARCMedium
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Placeholder documentation for H264LookAheadRateControl
data H264LookAheadRateControl = H264LookAheadRateControl' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern HLARCHigh :: H264LookAheadRateControl
pattern HLARCHigh = H264LookAheadRateControl' "HIGH"

pattern HLARCLow :: H264LookAheadRateControl
pattern HLARCLow = H264LookAheadRateControl' "LOW"

pattern HLARCMedium :: H264LookAheadRateControl
pattern HLARCMedium = H264LookAheadRateControl' "MEDIUM"

{-# COMPLETE
  HLARCHigh,
  HLARCLow,
  HLARCMedium,
  H264LookAheadRateControl' #-}

instance FromText H264LookAheadRateControl where
    parser = (H264LookAheadRateControl' . mk) <$> takeText

instance ToText H264LookAheadRateControl where
    toText (H264LookAheadRateControl' ci) = original ci

-- | Represents an enum of /known/ $H264LookAheadRateControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H264LookAheadRateControl where
    toEnum i = case i of
        0 -> HLARCHigh
        1 -> HLARCLow
        2 -> HLARCMedium
        _ -> (error . showText) $ "Unknown index for H264LookAheadRateControl: " <> toText i
    fromEnum x = case x of
        HLARCHigh -> 0
        HLARCLow -> 1
        HLARCMedium -> 2
        H264LookAheadRateControl' name -> (error . showText) $ "Unknown H264LookAheadRateControl: " <> original name

-- | Represents the bounds of /known/ $H264LookAheadRateControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264LookAheadRateControl where
    minBound = HLARCHigh
    maxBound = HLARCMedium

instance Hashable     H264LookAheadRateControl
instance NFData       H264LookAheadRateControl
instance ToByteString H264LookAheadRateControl
instance ToQuery      H264LookAheadRateControl
instance ToHeader     H264LookAheadRateControl

instance ToJSON H264LookAheadRateControl where
    toJSON = toJSONText

instance FromJSON H264LookAheadRateControl where
    parseJSON = parseJSONText "H264LookAheadRateControl"

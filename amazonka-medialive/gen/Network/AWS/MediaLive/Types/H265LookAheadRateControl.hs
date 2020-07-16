{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265LookAheadRateControl
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H265LookAheadRateControl (
  H265LookAheadRateControl (
    ..
    , High
    , Low
    , Medium
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | H265 Look Ahead Rate Control
data H265LookAheadRateControl = H265LookAheadRateControl' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern High :: H265LookAheadRateControl
pattern High = H265LookAheadRateControl' "HIGH"

pattern Low :: H265LookAheadRateControl
pattern Low = H265LookAheadRateControl' "LOW"

pattern Medium :: H265LookAheadRateControl
pattern Medium = H265LookAheadRateControl' "MEDIUM"

{-# COMPLETE
  High,
  Low,
  Medium,
  H265LookAheadRateControl' #-}

instance FromText H265LookAheadRateControl where
    parser = (H265LookAheadRateControl' . mk) <$> takeText

instance ToText H265LookAheadRateControl where
    toText (H265LookAheadRateControl' ci) = original ci

-- | Represents an enum of /known/ $H265LookAheadRateControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H265LookAheadRateControl where
    toEnum i = case i of
        0 -> High
        1 -> Low
        2 -> Medium
        _ -> (error . showText) $ "Unknown index for H265LookAheadRateControl: " <> toText i
    fromEnum x = case x of
        High -> 0
        Low -> 1
        Medium -> 2
        H265LookAheadRateControl' name -> (error . showText) $ "Unknown H265LookAheadRateControl: " <> original name

-- | Represents the bounds of /known/ $H265LookAheadRateControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H265LookAheadRateControl where
    minBound = High
    maxBound = Medium

instance Hashable     H265LookAheadRateControl
instance NFData       H265LookAheadRateControl
instance ToByteString H265LookAheadRateControl
instance ToQuery      H265LookAheadRateControl
instance ToHeader     H265LookAheadRateControl

instance ToJSON H265LookAheadRateControl where
    toJSON = toJSONText

instance FromJSON H265LookAheadRateControl where
    parseJSON = parseJSONText "H265LookAheadRateControl"

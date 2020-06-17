{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264AdaptiveQuantization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H264AdaptiveQuantization (
  H264AdaptiveQuantization (
    ..
    , HAQHigh
    , HAQHigher
    , HAQLow
    , HAQMax
    , HAQMedium
    , HAQOff
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
data H264AdaptiveQuantization = H264AdaptiveQuantization' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern HAQHigh :: H264AdaptiveQuantization
pattern HAQHigh = H264AdaptiveQuantization' "HIGH"

pattern HAQHigher :: H264AdaptiveQuantization
pattern HAQHigher = H264AdaptiveQuantization' "HIGHER"

pattern HAQLow :: H264AdaptiveQuantization
pattern HAQLow = H264AdaptiveQuantization' "LOW"

pattern HAQMax :: H264AdaptiveQuantization
pattern HAQMax = H264AdaptiveQuantization' "MAX"

pattern HAQMedium :: H264AdaptiveQuantization
pattern HAQMedium = H264AdaptiveQuantization' "MEDIUM"

pattern HAQOff :: H264AdaptiveQuantization
pattern HAQOff = H264AdaptiveQuantization' "OFF"

{-# COMPLETE
  HAQHigh,
  HAQHigher,
  HAQLow,
  HAQMax,
  HAQMedium,
  HAQOff,
  H264AdaptiveQuantization' #-}

instance FromText H264AdaptiveQuantization where
    parser = (H264AdaptiveQuantization' . mk) <$> takeText

instance ToText H264AdaptiveQuantization where
    toText (H264AdaptiveQuantization' ci) = original ci

-- | Represents an enum of /known/ $H264AdaptiveQuantization.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H264AdaptiveQuantization where
    toEnum i = case i of
        0 -> HAQHigh
        1 -> HAQHigher
        2 -> HAQLow
        3 -> HAQMax
        4 -> HAQMedium
        5 -> HAQOff
        _ -> (error . showText) $ "Unknown index for H264AdaptiveQuantization: " <> toText i
    fromEnum x = case x of
        HAQHigh -> 0
        HAQHigher -> 1
        HAQLow -> 2
        HAQMax -> 3
        HAQMedium -> 4
        HAQOff -> 5
        H264AdaptiveQuantization' name -> (error . showText) $ "Unknown H264AdaptiveQuantization: " <> original name

-- | Represents the bounds of /known/ $H264AdaptiveQuantization.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264AdaptiveQuantization where
    minBound = HAQHigh
    maxBound = HAQOff

instance Hashable     H264AdaptiveQuantization
instance NFData       H264AdaptiveQuantization
instance ToByteString H264AdaptiveQuantization
instance ToQuery      H264AdaptiveQuantization
instance ToHeader     H264AdaptiveQuantization

instance ToJSON H264AdaptiveQuantization where
    toJSON = toJSONText

instance FromJSON H264AdaptiveQuantization where
    parseJSON = parseJSONText "H264AdaptiveQuantization"

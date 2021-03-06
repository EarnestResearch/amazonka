{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DeinterlaceAlgorithm
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.DeinterlaceAlgorithm (
  DeinterlaceAlgorithm (
    ..
    , DABlend
    , DABlendTicker
    , DAInterpolate
    , DAInterpolateTicker
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Only applies when you set Deinterlacer (DeinterlaceMode) to Deinterlace (DEINTERLACE) or Adaptive (ADAPTIVE). Motion adaptive interpolate (INTERPOLATE) produces sharper pictures, while blend (BLEND) produces smoother motion. Use (INTERPOLATE_TICKER) OR (BLEND_TICKER) if your source file includes a ticker, such as a scrolling headline at the bottom of the frame.
data DeinterlaceAlgorithm = DeinterlaceAlgorithm' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern DABlend :: DeinterlaceAlgorithm
pattern DABlend = DeinterlaceAlgorithm' "BLEND"

pattern DABlendTicker :: DeinterlaceAlgorithm
pattern DABlendTicker = DeinterlaceAlgorithm' "BLEND_TICKER"

pattern DAInterpolate :: DeinterlaceAlgorithm
pattern DAInterpolate = DeinterlaceAlgorithm' "INTERPOLATE"

pattern DAInterpolateTicker :: DeinterlaceAlgorithm
pattern DAInterpolateTicker = DeinterlaceAlgorithm' "INTERPOLATE_TICKER"

{-# COMPLETE
  DABlend,
  DABlendTicker,
  DAInterpolate,
  DAInterpolateTicker,
  DeinterlaceAlgorithm' #-}

instance FromText DeinterlaceAlgorithm where
    parser = (DeinterlaceAlgorithm' . mk) <$> takeText

instance ToText DeinterlaceAlgorithm where
    toText (DeinterlaceAlgorithm' ci) = original ci

-- | Represents an enum of /known/ $DeinterlaceAlgorithm.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeinterlaceAlgorithm where
    toEnum i = case i of
        0 -> DABlend
        1 -> DABlendTicker
        2 -> DAInterpolate
        3 -> DAInterpolateTicker
        _ -> (error . showText) $ "Unknown index for DeinterlaceAlgorithm: " <> toText i
    fromEnum x = case x of
        DABlend -> 0
        DABlendTicker -> 1
        DAInterpolate -> 2
        DAInterpolateTicker -> 3
        DeinterlaceAlgorithm' name -> (error . showText) $ "Unknown DeinterlaceAlgorithm: " <> original name

-- | Represents the bounds of /known/ $DeinterlaceAlgorithm.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeinterlaceAlgorithm where
    minBound = DABlend
    maxBound = DAInterpolateTicker

instance Hashable     DeinterlaceAlgorithm
instance NFData       DeinterlaceAlgorithm
instance ToByteString DeinterlaceAlgorithm
instance ToQuery      DeinterlaceAlgorithm
instance ToHeader     DeinterlaceAlgorithm

instance ToJSON DeinterlaceAlgorithm where
    toJSON = toJSONText

instance FromJSON DeinterlaceAlgorithm where
    parseJSON = parseJSONText "DeinterlaceAlgorithm"

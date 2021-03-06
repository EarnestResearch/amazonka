{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264QualityTuningLevel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H264QualityTuningLevel (
  H264QualityTuningLevel (
    ..
    , HQTLMultiPassHq
    , HQTLSinglePass
    , HQTLSinglePassHq
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Use Quality tuning level (H264QualityTuningLevel) to specifiy whether to use fast single-pass, high-quality singlepass, or high-quality multipass video encoding.
data H264QualityTuningLevel = H264QualityTuningLevel' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern HQTLMultiPassHq :: H264QualityTuningLevel
pattern HQTLMultiPassHq = H264QualityTuningLevel' "MULTI_PASS_HQ"

pattern HQTLSinglePass :: H264QualityTuningLevel
pattern HQTLSinglePass = H264QualityTuningLevel' "SINGLE_PASS"

pattern HQTLSinglePassHq :: H264QualityTuningLevel
pattern HQTLSinglePassHq = H264QualityTuningLevel' "SINGLE_PASS_HQ"

{-# COMPLETE
  HQTLMultiPassHq,
  HQTLSinglePass,
  HQTLSinglePassHq,
  H264QualityTuningLevel' #-}

instance FromText H264QualityTuningLevel where
    parser = (H264QualityTuningLevel' . mk) <$> takeText

instance ToText H264QualityTuningLevel where
    toText (H264QualityTuningLevel' ci) = original ci

-- | Represents an enum of /known/ $H264QualityTuningLevel.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H264QualityTuningLevel where
    toEnum i = case i of
        0 -> HQTLMultiPassHq
        1 -> HQTLSinglePass
        2 -> HQTLSinglePassHq
        _ -> (error . showText) $ "Unknown index for H264QualityTuningLevel: " <> toText i
    fromEnum x = case x of
        HQTLMultiPassHq -> 0
        HQTLSinglePass -> 1
        HQTLSinglePassHq -> 2
        H264QualityTuningLevel' name -> (error . showText) $ "Unknown H264QualityTuningLevel: " <> original name

-- | Represents the bounds of /known/ $H264QualityTuningLevel.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264QualityTuningLevel where
    minBound = HQTLMultiPassHq
    maxBound = HQTLSinglePassHq

instance Hashable     H264QualityTuningLevel
instance NFData       H264QualityTuningLevel
instance ToByteString H264QualityTuningLevel
instance ToQuery      H264QualityTuningLevel
instance ToHeader     H264QualityTuningLevel

instance ToJSON H264QualityTuningLevel where
    toJSON = toJSONText

instance FromJSON H264QualityTuningLevel where
    parseJSON = parseJSONText "H264QualityTuningLevel"

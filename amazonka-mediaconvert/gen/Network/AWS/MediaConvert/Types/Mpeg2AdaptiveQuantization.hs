{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2AdaptiveQuantization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Mpeg2AdaptiveQuantization where

import Network.AWS.Prelude
  
-- | Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
data Mpeg2AdaptiveQuantization = MAQHigh
                               | MAQLow
                               | MAQMedium
                               | MAQOff
                                   deriving (Eq, Ord, Read, Show, Enum, Bounded,
                                             Data, Typeable, Generic)

instance FromText Mpeg2AdaptiveQuantization where
    parser = takeLowerText >>= \case
        "high" -> pure MAQHigh
        "low" -> pure MAQLow
        "medium" -> pure MAQMedium
        "off" -> pure MAQOff
        e -> fromTextError $ "Failure parsing Mpeg2AdaptiveQuantization from value: '" <> e
           <> "'. Accepted values: high, low, medium, off"

instance ToText Mpeg2AdaptiveQuantization where
    toText = \case
        MAQHigh -> "HIGH"
        MAQLow -> "LOW"
        MAQMedium -> "MEDIUM"
        MAQOff -> "OFF"

instance Hashable     Mpeg2AdaptiveQuantization
instance NFData       Mpeg2AdaptiveQuantization
instance ToByteString Mpeg2AdaptiveQuantization
instance ToQuery      Mpeg2AdaptiveQuantization
instance ToHeader     Mpeg2AdaptiveQuantization

instance ToJSON Mpeg2AdaptiveQuantization where
    toJSON = toJSONText

instance FromJSON Mpeg2AdaptiveQuantization where
    parseJSON = parseJSONText "Mpeg2AdaptiveQuantization"

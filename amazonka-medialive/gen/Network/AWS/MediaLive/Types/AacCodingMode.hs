{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AacCodingMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.AacCodingMode where

import Network.AWS.Prelude
  
-- | Placeholder documentation for AacCodingMode
data AacCodingMode = ACMAdReceiverMix
                   | ACMCodingMode10
                   | ACMCodingMode11
                   | ACMCodingMode20
                   | ACMCodingMode51
                       deriving (Eq, Ord, Read, Show, Enum, Bounded, Data,
                                 Typeable, Generic)

instance FromText AacCodingMode where
    parser = takeLowerText >>= \case
        "ad_receiver_mix" -> pure ACMAdReceiverMix
        "coding_mode_1_0" -> pure ACMCodingMode10
        "coding_mode_1_1" -> pure ACMCodingMode11
        "coding_mode_2_0" -> pure ACMCodingMode20
        "coding_mode_5_1" -> pure ACMCodingMode51
        e -> fromTextError $ "Failure parsing AacCodingMode from value: '" <> e
           <> "'. Accepted values: ad_receiver_mix, coding_mode_1_0, coding_mode_1_1, coding_mode_2_0, coding_mode_5_1"

instance ToText AacCodingMode where
    toText = \case
        ACMAdReceiverMix -> "AD_RECEIVER_MIX"
        ACMCodingMode10 -> "CODING_MODE_1_0"
        ACMCodingMode11 -> "CODING_MODE_1_1"
        ACMCodingMode20 -> "CODING_MODE_2_0"
        ACMCodingMode51 -> "CODING_MODE_5_1"

instance Hashable     AacCodingMode
instance NFData       AacCodingMode
instance ToByteString AacCodingMode
instance ToQuery      AacCodingMode
instance ToHeader     AacCodingMode

instance ToJSON AacCodingMode where
    toJSON = toJSONText

instance FromJSON AacCodingMode where
    parseJSON = parseJSONText "AacCodingMode"

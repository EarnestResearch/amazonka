{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AfdSignaling
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AfdSignaling where

import Network.AWS.Prelude
  
-- | This setting only applies to H.264 and MPEG2 outputs. Use Insert AFD signaling (AfdSignaling) to whether there are AFD values in the output video data and what those values are. * Choose None to remove all AFD values from this output. * Choose Fixed to ignore input AFD values and instead encode the value specified in the job. * Choose Auto to calculate output AFD values based on the input AFD scaler data.
data AfdSignaling = ASAuto
                  | ASFixed
                  | ASNone
                      deriving (Eq, Ord, Read, Show, Enum, Bounded, Data,
                                Typeable, Generic)

instance FromText AfdSignaling where
    parser = takeLowerText >>= \case
        "auto" -> pure ASAuto
        "fixed" -> pure ASFixed
        "none" -> pure ASNone
        e -> fromTextError $ "Failure parsing AfdSignaling from value: '" <> e
           <> "'. Accepted values: auto, fixed, none"

instance ToText AfdSignaling where
    toText = \case
        ASAuto -> "AUTO"
        ASFixed -> "FIXED"
        ASNone -> "NONE"

instance Hashable     AfdSignaling
instance NFData       AfdSignaling
instance ToByteString AfdSignaling
instance ToQuery      AfdSignaling
instance ToHeader     AfdSignaling

instance ToJSON AfdSignaling where
    toJSON = toJSONText

instance FromJSON AfdSignaling where
    parseJSON = parseJSONText "AfdSignaling"

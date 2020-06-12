{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264SceneChangeDetect
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H264SceneChangeDetect where

import Network.AWS.Prelude
  
-- | Scene change detection (inserts I-frames on scene changes).
data H264SceneChangeDetect = HSCDSDisabled
                           | HSCDSEnabled
                               deriving (Eq, Ord, Read, Show, Enum, Bounded,
                                         Data, Typeable, Generic)

instance FromText H264SceneChangeDetect where
    parser = takeLowerText >>= \case
        "disabled" -> pure HSCDSDisabled
        "enabled" -> pure HSCDSEnabled
        e -> fromTextError $ "Failure parsing H264SceneChangeDetect from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText H264SceneChangeDetect where
    toText = \case
        HSCDSDisabled -> "DISABLED"
        HSCDSEnabled -> "ENABLED"

instance Hashable     H264SceneChangeDetect
instance NFData       H264SceneChangeDetect
instance ToByteString H264SceneChangeDetect
instance ToQuery      H264SceneChangeDetect
instance ToHeader     H264SceneChangeDetect

instance ToJSON H264SceneChangeDetect where
    toJSON = toJSONText

instance FromJSON H264SceneChangeDetect where
    parseJSON = parseJSONText "H264SceneChangeDetect"

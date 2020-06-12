{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264GopSizeUnits
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H264GopSizeUnits where

import Network.AWS.Prelude
  
-- | Placeholder documentation for H264GopSizeUnits
data H264GopSizeUnits = Frames
                      | Seconds
                          deriving (Eq, Ord, Read, Show, Enum, Bounded, Data,
                                    Typeable, Generic)

instance FromText H264GopSizeUnits where
    parser = takeLowerText >>= \case
        "frames" -> pure Frames
        "seconds" -> pure Seconds
        e -> fromTextError $ "Failure parsing H264GopSizeUnits from value: '" <> e
           <> "'. Accepted values: frames, seconds"

instance ToText H264GopSizeUnits where
    toText = \case
        Frames -> "FRAMES"
        Seconds -> "SECONDS"

instance Hashable     H264GopSizeUnits
instance NFData       H264GopSizeUnits
instance ToByteString H264GopSizeUnits
instance ToQuery      H264GopSizeUnits
instance ToHeader     H264GopSizeUnits

instance ToJSON H264GopSizeUnits where
    toJSON = toJSONText

instance FromJSON H264GopSizeUnits where
    parseJSON = parseJSONText "H264GopSizeUnits"

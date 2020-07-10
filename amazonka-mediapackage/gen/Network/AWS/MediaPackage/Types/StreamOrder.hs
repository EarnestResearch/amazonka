{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.StreamOrder
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaPackage.Types.StreamOrder (
  StreamOrder (
    ..
    , Original
    , VideoBitrateAscending
    , VideoBitrateDescending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StreamOrder = StreamOrder' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Original :: StreamOrder
pattern Original = StreamOrder' "ORIGINAL"

pattern VideoBitrateAscending :: StreamOrder
pattern VideoBitrateAscending = StreamOrder' "VIDEO_BITRATE_ASCENDING"

pattern VideoBitrateDescending :: StreamOrder
pattern VideoBitrateDescending = StreamOrder' "VIDEO_BITRATE_DESCENDING"

{-# COMPLETE
  Original,
  VideoBitrateAscending,
  VideoBitrateDescending,
  StreamOrder' #-}

instance FromText StreamOrder where
    parser = (StreamOrder' . mk) <$> takeText

instance ToText StreamOrder where
    toText (StreamOrder' ci) = original ci

-- | Represents an enum of /known/ $StreamOrder.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StreamOrder where
    toEnum i = case i of
        0 -> Original
        1 -> VideoBitrateAscending
        2 -> VideoBitrateDescending
        _ -> (error . showText) $ "Unknown index for StreamOrder: " <> toText i
    fromEnum x = case x of
        Original -> 0
        VideoBitrateAscending -> 1
        VideoBitrateDescending -> 2
        StreamOrder' name -> (error . showText) $ "Unknown StreamOrder: " <> original name

-- | Represents the bounds of /known/ $StreamOrder.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StreamOrder where
    minBound = Original
    maxBound = VideoBitrateDescending

instance Hashable     StreamOrder
instance NFData       StreamOrder
instance ToByteString StreamOrder
instance ToQuery      StreamOrder
instance ToHeader     StreamOrder

instance ToJSON StreamOrder where
    toJSON = toJSONText

instance FromJSON StreamOrder where
    parseJSON = parseJSONText "StreamOrder"

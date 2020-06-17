{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

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
module Network.AWS.MediaLive.Types.H264GopSizeUnits (
  H264GopSizeUnits (
    ..
    , Frames
    , Seconds
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Placeholder documentation for H264GopSizeUnits
data H264GopSizeUnits = H264GopSizeUnits' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern Frames :: H264GopSizeUnits
pattern Frames = H264GopSizeUnits' "FRAMES"

pattern Seconds :: H264GopSizeUnits
pattern Seconds = H264GopSizeUnits' "SECONDS"

{-# COMPLETE
  Frames,
  Seconds,
  H264GopSizeUnits' #-}

instance FromText H264GopSizeUnits where
    parser = (H264GopSizeUnits' . mk) <$> takeText

instance ToText H264GopSizeUnits where
    toText (H264GopSizeUnits' ci) = original ci

-- | Represents an enum of /known/ $H264GopSizeUnits.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H264GopSizeUnits where
    toEnum i = case i of
        0 -> Frames
        1 -> Seconds
        _ -> (error . showText) $ "Unknown index for H264GopSizeUnits: " <> toText i
    fromEnum x = case x of
        Frames -> 0
        Seconds -> 1
        H264GopSizeUnits' name -> (error . showText) $ "Unknown H264GopSizeUnits: " <> original name

-- | Represents the bounds of /known/ $H264GopSizeUnits.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264GopSizeUnits where
    minBound = Frames
    maxBound = Seconds

instance Hashable     H264GopSizeUnits
instance NFData       H264GopSizeUnits
instance ToByteString H264GopSizeUnits
instance ToQuery      H264GopSizeUnits
instance ToHeader     H264GopSizeUnits

instance ToJSON H264GopSizeUnits where
    toJSON = toJSONText

instance FromJSON H264GopSizeUnits where
    parseJSON = parseJSONText "H264GopSizeUnits"

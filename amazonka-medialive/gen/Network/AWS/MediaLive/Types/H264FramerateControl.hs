{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264FramerateControl
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H264FramerateControl (
  H264FramerateControl (
    ..
    , HFCInitializeFromSource
    , HFCSpecified
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | H264 Framerate Control
data H264FramerateControl = H264FramerateControl' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern HFCInitializeFromSource :: H264FramerateControl
pattern HFCInitializeFromSource = H264FramerateControl' "INITIALIZE_FROM_SOURCE"

pattern HFCSpecified :: H264FramerateControl
pattern HFCSpecified = H264FramerateControl' "SPECIFIED"

{-# COMPLETE
  HFCInitializeFromSource,
  HFCSpecified,
  H264FramerateControl' #-}

instance FromText H264FramerateControl where
    parser = (H264FramerateControl' . mk) <$> takeText

instance ToText H264FramerateControl where
    toText (H264FramerateControl' ci) = original ci

-- | Represents an enum of /known/ $H264FramerateControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H264FramerateControl where
    toEnum i = case i of
        0 -> HFCInitializeFromSource
        1 -> HFCSpecified
        _ -> (error . showText) $ "Unknown index for H264FramerateControl: " <> toText i
    fromEnum x = case x of
        HFCInitializeFromSource -> 0
        HFCSpecified -> 1
        H264FramerateControl' name -> (error . showText) $ "Unknown H264FramerateControl: " <> original name

-- | Represents the bounds of /known/ $H264FramerateControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264FramerateControl where
    minBound = HFCInitializeFromSource
    maxBound = HFCSpecified

instance Hashable     H264FramerateControl
instance NFData       H264FramerateControl
instance ToByteString H264FramerateControl
instance ToQuery      H264FramerateControl
instance ToHeader     H264FramerateControl

instance ToJSON H264FramerateControl where
    toJSON = toJSONText

instance FromJSON H264FramerateControl where
    parseJSON = parseJSONText "H264FramerateControl"

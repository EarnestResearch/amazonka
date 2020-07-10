{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FrameCaptureIntervalUnit
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.FrameCaptureIntervalUnit (
  FrameCaptureIntervalUnit (
    ..
    , Milliseconds
    , Seconds
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Frame Capture Interval Unit
data FrameCaptureIntervalUnit = FrameCaptureIntervalUnit' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern Milliseconds :: FrameCaptureIntervalUnit
pattern Milliseconds = FrameCaptureIntervalUnit' "MILLISECONDS"

pattern Seconds :: FrameCaptureIntervalUnit
pattern Seconds = FrameCaptureIntervalUnit' "SECONDS"

{-# COMPLETE
  Milliseconds,
  Seconds,
  FrameCaptureIntervalUnit' #-}

instance FromText FrameCaptureIntervalUnit where
    parser = (FrameCaptureIntervalUnit' . mk) <$> takeText

instance ToText FrameCaptureIntervalUnit where
    toText (FrameCaptureIntervalUnit' ci) = original ci

-- | Represents an enum of /known/ $FrameCaptureIntervalUnit.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FrameCaptureIntervalUnit where
    toEnum i = case i of
        0 -> Milliseconds
        1 -> Seconds
        _ -> (error . showText) $ "Unknown index for FrameCaptureIntervalUnit: " <> toText i
    fromEnum x = case x of
        Milliseconds -> 0
        Seconds -> 1
        FrameCaptureIntervalUnit' name -> (error . showText) $ "Unknown FrameCaptureIntervalUnit: " <> original name

-- | Represents the bounds of /known/ $FrameCaptureIntervalUnit.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FrameCaptureIntervalUnit where
    minBound = Milliseconds
    maxBound = Seconds

instance Hashable     FrameCaptureIntervalUnit
instance NFData       FrameCaptureIntervalUnit
instance ToByteString FrameCaptureIntervalUnit
instance ToQuery      FrameCaptureIntervalUnit
instance ToHeader     FrameCaptureIntervalUnit

instance ToJSON FrameCaptureIntervalUnit where
    toJSON = toJSONText

instance FromJSON FrameCaptureIntervalUnit where
    parseJSON = parseJSONText "FrameCaptureIntervalUnit"

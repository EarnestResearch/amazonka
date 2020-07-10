{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsId3SegmentTaggingState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.HlsId3SegmentTaggingState (
  HlsId3SegmentTaggingState (
    ..
    , HISTSDisabled
    , HISTSEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | State of HLS ID3 Segment Tagging
data HlsId3SegmentTaggingState = HlsId3SegmentTaggingState' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern HISTSDisabled :: HlsId3SegmentTaggingState
pattern HISTSDisabled = HlsId3SegmentTaggingState' "DISABLED"

pattern HISTSEnabled :: HlsId3SegmentTaggingState
pattern HISTSEnabled = HlsId3SegmentTaggingState' "ENABLED"

{-# COMPLETE
  HISTSDisabled,
  HISTSEnabled,
  HlsId3SegmentTaggingState' #-}

instance FromText HlsId3SegmentTaggingState where
    parser = (HlsId3SegmentTaggingState' . mk) <$> takeText

instance ToText HlsId3SegmentTaggingState where
    toText (HlsId3SegmentTaggingState' ci) = original ci

-- | Represents an enum of /known/ $HlsId3SegmentTaggingState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HlsId3SegmentTaggingState where
    toEnum i = case i of
        0 -> HISTSDisabled
        1 -> HISTSEnabled
        _ -> (error . showText) $ "Unknown index for HlsId3SegmentTaggingState: " <> toText i
    fromEnum x = case x of
        HISTSDisabled -> 0
        HISTSEnabled -> 1
        HlsId3SegmentTaggingState' name -> (error . showText) $ "Unknown HlsId3SegmentTaggingState: " <> original name

-- | Represents the bounds of /known/ $HlsId3SegmentTaggingState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HlsId3SegmentTaggingState where
    minBound = HISTSDisabled
    maxBound = HISTSEnabled

instance Hashable     HlsId3SegmentTaggingState
instance NFData       HlsId3SegmentTaggingState
instance ToByteString HlsId3SegmentTaggingState
instance ToQuery      HlsId3SegmentTaggingState
instance ToHeader     HlsId3SegmentTaggingState

instance ToJSON HlsId3SegmentTaggingState where
    toJSON = toJSONText

instance FromJSON HlsId3SegmentTaggingState where
    parseJSON = parseJSONText "HlsId3SegmentTaggingState"

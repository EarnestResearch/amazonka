{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafWriteSegmentTimelineInRepresentation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.CmafWriteSegmentTimelineInRepresentation (
  CmafWriteSegmentTimelineInRepresentation (
    ..
    , CWSTIRDisabled
    , CWSTIREnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | When you enable Precise segment duration in DASH manifests (writeSegmentTimelineInRepresentation), your DASH manifest shows precise segment durations. The segment duration information appears inside the SegmentTimeline element, inside SegmentTemplate at the Representation level. When this feature isn't enabled, the segment durations in your DASH manifest are approximate. The segment duration information appears in the duration attribute of the SegmentTemplate element.
data CmafWriteSegmentTimelineInRepresentation = CmafWriteSegmentTimelineInRepresentation' (CI
                                                                                             Text)
                                                  deriving (Eq, Ord, Read, Show,
                                                            Data, Typeable,
                                                            Generic)

pattern CWSTIRDisabled :: CmafWriteSegmentTimelineInRepresentation
pattern CWSTIRDisabled = CmafWriteSegmentTimelineInRepresentation' "DISABLED"

pattern CWSTIREnabled :: CmafWriteSegmentTimelineInRepresentation
pattern CWSTIREnabled = CmafWriteSegmentTimelineInRepresentation' "ENABLED"

{-# COMPLETE
  CWSTIRDisabled,
  CWSTIREnabled,
  CmafWriteSegmentTimelineInRepresentation' #-}

instance FromText CmafWriteSegmentTimelineInRepresentation where
    parser = (CmafWriteSegmentTimelineInRepresentation' . mk) <$> takeText

instance ToText CmafWriteSegmentTimelineInRepresentation where
    toText (CmafWriteSegmentTimelineInRepresentation' ci) = original ci

-- | Represents an enum of /known/ $CmafWriteSegmentTimelineInRepresentation.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CmafWriteSegmentTimelineInRepresentation where
    toEnum i = case i of
        0 -> CWSTIRDisabled
        1 -> CWSTIREnabled
        _ -> (error . showText) $ "Unknown index for CmafWriteSegmentTimelineInRepresentation: " <> toText i
    fromEnum x = case x of
        CWSTIRDisabled -> 0
        CWSTIREnabled -> 1
        CmafWriteSegmentTimelineInRepresentation' name -> (error . showText) $ "Unknown CmafWriteSegmentTimelineInRepresentation: " <> original name

-- | Represents the bounds of /known/ $CmafWriteSegmentTimelineInRepresentation.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CmafWriteSegmentTimelineInRepresentation where
    minBound = CWSTIRDisabled
    maxBound = CWSTIREnabled

instance Hashable     CmafWriteSegmentTimelineInRepresentation
instance NFData       CmafWriteSegmentTimelineInRepresentation
instance ToByteString CmafWriteSegmentTimelineInRepresentation
instance ToQuery      CmafWriteSegmentTimelineInRepresentation
instance ToHeader     CmafWriteSegmentTimelineInRepresentation

instance ToJSON CmafWriteSegmentTimelineInRepresentation where
    toJSON = toJSONText

instance FromJSON CmafWriteSegmentTimelineInRepresentation where
    parseJSON = parseJSONText "CmafWriteSegmentTimelineInRepresentation"

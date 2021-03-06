{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsSegmentationMarkers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.M2tsSegmentationMarkers (
  M2tsSegmentationMarkers (
    ..
    , MSMEbp
    , MSMEbpLegacy
    , MSMNone
    , MSMPsiSegstart
    , MSMRaiAdapt
    , MSMRaiSegstart
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Inserts segmentation markers at each segmentation_time period. rai_segstart sets the Random Access Indicator bit in the adaptation field. rai_adapt sets the RAI bit and adds the current timecode in the private data bytes. psi_segstart inserts PAT and PMT tables at the start of segments. ebp adds Encoder Boundary Point information to the adaptation field as per OpenCable specification OC-SP-EBP-I01-130118. ebp_legacy adds Encoder Boundary Point information to the adaptation field using a legacy proprietary format.
data M2tsSegmentationMarkers = M2tsSegmentationMarkers' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern MSMEbp :: M2tsSegmentationMarkers
pattern MSMEbp = M2tsSegmentationMarkers' "EBP"

pattern MSMEbpLegacy :: M2tsSegmentationMarkers
pattern MSMEbpLegacy = M2tsSegmentationMarkers' "EBP_LEGACY"

pattern MSMNone :: M2tsSegmentationMarkers
pattern MSMNone = M2tsSegmentationMarkers' "NONE"

pattern MSMPsiSegstart :: M2tsSegmentationMarkers
pattern MSMPsiSegstart = M2tsSegmentationMarkers' "PSI_SEGSTART"

pattern MSMRaiAdapt :: M2tsSegmentationMarkers
pattern MSMRaiAdapt = M2tsSegmentationMarkers' "RAI_ADAPT"

pattern MSMRaiSegstart :: M2tsSegmentationMarkers
pattern MSMRaiSegstart = M2tsSegmentationMarkers' "RAI_SEGSTART"

{-# COMPLETE
  MSMEbp,
  MSMEbpLegacy,
  MSMNone,
  MSMPsiSegstart,
  MSMRaiAdapt,
  MSMRaiSegstart,
  M2tsSegmentationMarkers' #-}

instance FromText M2tsSegmentationMarkers where
    parser = (M2tsSegmentationMarkers' . mk) <$> takeText

instance ToText M2tsSegmentationMarkers where
    toText (M2tsSegmentationMarkers' ci) = original ci

-- | Represents an enum of /known/ $M2tsSegmentationMarkers.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum M2tsSegmentationMarkers where
    toEnum i = case i of
        0 -> MSMEbp
        1 -> MSMEbpLegacy
        2 -> MSMNone
        3 -> MSMPsiSegstart
        4 -> MSMRaiAdapt
        5 -> MSMRaiSegstart
        _ -> (error . showText) $ "Unknown index for M2tsSegmentationMarkers: " <> toText i
    fromEnum x = case x of
        MSMEbp -> 0
        MSMEbpLegacy -> 1
        MSMNone -> 2
        MSMPsiSegstart -> 3
        MSMRaiAdapt -> 4
        MSMRaiSegstart -> 5
        M2tsSegmentationMarkers' name -> (error . showText) $ "Unknown M2tsSegmentationMarkers: " <> original name

-- | Represents the bounds of /known/ $M2tsSegmentationMarkers.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded M2tsSegmentationMarkers where
    minBound = MSMEbp
    maxBound = MSMRaiSegstart

instance Hashable     M2tsSegmentationMarkers
instance NFData       M2tsSegmentationMarkers
instance ToByteString M2tsSegmentationMarkers
instance ToQuery      M2tsSegmentationMarkers
instance ToHeader     M2tsSegmentationMarkers

instance ToJSON M2tsSegmentationMarkers where
    toJSON = toJSONText

instance FromJSON M2tsSegmentationMarkers where
    parseJSON = parseJSONText "M2tsSegmentationMarkers"

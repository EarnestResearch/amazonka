{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsSegmentControl
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.HlsSegmentControl (
  HlsSegmentControl (
    ..
    , SegmentedFiles
    , SingleFile
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | When set to SINGLE_FILE, emits program as a single media resource (.ts) file, uses #EXT-X-BYTERANGE tags to index segment for playback.
data HlsSegmentControl = HlsSegmentControl' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern SegmentedFiles :: HlsSegmentControl
pattern SegmentedFiles = HlsSegmentControl' "SEGMENTED_FILES"

pattern SingleFile :: HlsSegmentControl
pattern SingleFile = HlsSegmentControl' "SINGLE_FILE"

{-# COMPLETE
  SegmentedFiles,
  SingleFile,
  HlsSegmentControl' #-}

instance FromText HlsSegmentControl where
    parser = (HlsSegmentControl' . mk) <$> takeText

instance ToText HlsSegmentControl where
    toText (HlsSegmentControl' ci) = original ci

-- | Represents an enum of /known/ $HlsSegmentControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HlsSegmentControl where
    toEnum i = case i of
        0 -> SegmentedFiles
        1 -> SingleFile
        _ -> (error . showText) $ "Unknown index for HlsSegmentControl: " <> toText i
    fromEnum x = case x of
        SegmentedFiles -> 0
        SingleFile -> 1
        HlsSegmentControl' name -> (error . showText) $ "Unknown HlsSegmentControl: " <> original name

-- | Represents the bounds of /known/ $HlsSegmentControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HlsSegmentControl where
    minBound = SegmentedFiles
    maxBound = SingleFile

instance Hashable     HlsSegmentControl
instance NFData       HlsSegmentControl
instance ToByteString HlsSegmentControl
instance ToQuery      HlsSegmentControl
instance ToHeader     HlsSegmentControl

instance ToJSON HlsSegmentControl where
    toJSON = toJSONText

instance FromJSON HlsSegmentControl where
    parseJSON = parseJSONText "HlsSegmentControl"

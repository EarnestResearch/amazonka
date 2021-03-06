{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionRf
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionRf (
  Eac3AtmosDynamicRangeCompressionRf (
    ..
    , EADRCRFilmLight
    , EADRCRFilmStandard
    , EADRCRMusicLight
    , EADRCRMusicStandard
    , EADRCRNone
    , EADRCRSpeech
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Specify how the service limits the audio dynamic range when compressing the audio.
data Eac3AtmosDynamicRangeCompressionRf = Eac3AtmosDynamicRangeCompressionRf' (CI
                                                                                 Text)
                                            deriving (Eq, Ord, Read, Show, Data,
                                                      Typeable, Generic)

pattern EADRCRFilmLight :: Eac3AtmosDynamicRangeCompressionRf
pattern EADRCRFilmLight = Eac3AtmosDynamicRangeCompressionRf' "FILM_LIGHT"

pattern EADRCRFilmStandard :: Eac3AtmosDynamicRangeCompressionRf
pattern EADRCRFilmStandard = Eac3AtmosDynamicRangeCompressionRf' "FILM_STANDARD"

pattern EADRCRMusicLight :: Eac3AtmosDynamicRangeCompressionRf
pattern EADRCRMusicLight = Eac3AtmosDynamicRangeCompressionRf' "MUSIC_LIGHT"

pattern EADRCRMusicStandard :: Eac3AtmosDynamicRangeCompressionRf
pattern EADRCRMusicStandard = Eac3AtmosDynamicRangeCompressionRf' "MUSIC_STANDARD"

pattern EADRCRNone :: Eac3AtmosDynamicRangeCompressionRf
pattern EADRCRNone = Eac3AtmosDynamicRangeCompressionRf' "NONE"

pattern EADRCRSpeech :: Eac3AtmosDynamicRangeCompressionRf
pattern EADRCRSpeech = Eac3AtmosDynamicRangeCompressionRf' "SPEECH"

{-# COMPLETE
  EADRCRFilmLight,
  EADRCRFilmStandard,
  EADRCRMusicLight,
  EADRCRMusicStandard,
  EADRCRNone,
  EADRCRSpeech,
  Eac3AtmosDynamicRangeCompressionRf' #-}

instance FromText Eac3AtmosDynamicRangeCompressionRf where
    parser = (Eac3AtmosDynamicRangeCompressionRf' . mk) <$> takeText

instance ToText Eac3AtmosDynamicRangeCompressionRf where
    toText (Eac3AtmosDynamicRangeCompressionRf' ci) = original ci

-- | Represents an enum of /known/ $Eac3AtmosDynamicRangeCompressionRf.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Eac3AtmosDynamicRangeCompressionRf where
    toEnum i = case i of
        0 -> EADRCRFilmLight
        1 -> EADRCRFilmStandard
        2 -> EADRCRMusicLight
        3 -> EADRCRMusicStandard
        4 -> EADRCRNone
        5 -> EADRCRSpeech
        _ -> (error . showText) $ "Unknown index for Eac3AtmosDynamicRangeCompressionRf: " <> toText i
    fromEnum x = case x of
        EADRCRFilmLight -> 0
        EADRCRFilmStandard -> 1
        EADRCRMusicLight -> 2
        EADRCRMusicStandard -> 3
        EADRCRNone -> 4
        EADRCRSpeech -> 5
        Eac3AtmosDynamicRangeCompressionRf' name -> (error . showText) $ "Unknown Eac3AtmosDynamicRangeCompressionRf: " <> original name

-- | Represents the bounds of /known/ $Eac3AtmosDynamicRangeCompressionRf.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Eac3AtmosDynamicRangeCompressionRf where
    minBound = EADRCRFilmLight
    maxBound = EADRCRSpeech

instance Hashable     Eac3AtmosDynamicRangeCompressionRf
instance NFData       Eac3AtmosDynamicRangeCompressionRf
instance ToByteString Eac3AtmosDynamicRangeCompressionRf
instance ToQuery      Eac3AtmosDynamicRangeCompressionRf
instance ToHeader     Eac3AtmosDynamicRangeCompressionRf

instance ToJSON Eac3AtmosDynamicRangeCompressionRf where
    toJSON = toJSONText

instance FromJSON Eac3AtmosDynamicRangeCompressionRf where
    parseJSON = parseJSONText "Eac3AtmosDynamicRangeCompressionRf"

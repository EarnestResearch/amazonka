{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.HLSDisplayFragmentTimestamp
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideoArchivedMedia.Types.HLSDisplayFragmentTimestamp (
  HLSDisplayFragmentTimestamp (
    ..
    , HLSDFTAlways
    , HLSDFTNever
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data HLSDisplayFragmentTimestamp = HLSDisplayFragmentTimestamp' (CI
                                                                   Text)
                                     deriving (Eq, Ord, Read, Show, Data,
                                               Typeable, Generic)

pattern HLSDFTAlways :: HLSDisplayFragmentTimestamp
pattern HLSDFTAlways = HLSDisplayFragmentTimestamp' "ALWAYS"

pattern HLSDFTNever :: HLSDisplayFragmentTimestamp
pattern HLSDFTNever = HLSDisplayFragmentTimestamp' "NEVER"

{-# COMPLETE
  HLSDFTAlways,
  HLSDFTNever,
  HLSDisplayFragmentTimestamp' #-}

instance FromText HLSDisplayFragmentTimestamp where
    parser = (HLSDisplayFragmentTimestamp' . mk) <$> takeText

instance ToText HLSDisplayFragmentTimestamp where
    toText (HLSDisplayFragmentTimestamp' ci) = original ci

-- | Represents an enum of /known/ $HLSDisplayFragmentTimestamp.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HLSDisplayFragmentTimestamp where
    toEnum i = case i of
        0 -> HLSDFTAlways
        1 -> HLSDFTNever
        _ -> (error . showText) $ "Unknown index for HLSDisplayFragmentTimestamp: " <> toText i
    fromEnum x = case x of
        HLSDFTAlways -> 0
        HLSDFTNever -> 1
        HLSDisplayFragmentTimestamp' name -> (error . showText) $ "Unknown HLSDisplayFragmentTimestamp: " <> original name

-- | Represents the bounds of /known/ $HLSDisplayFragmentTimestamp.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HLSDisplayFragmentTimestamp where
    minBound = HLSDFTAlways
    maxBound = HLSDFTNever

instance Hashable     HLSDisplayFragmentTimestamp
instance NFData       HLSDisplayFragmentTimestamp
instance ToByteString HLSDisplayFragmentTimestamp
instance ToQuery      HLSDisplayFragmentTimestamp
instance ToHeader     HLSDisplayFragmentTimestamp

instance ToJSON HLSDisplayFragmentTimestamp where
    toJSON = toJSONText

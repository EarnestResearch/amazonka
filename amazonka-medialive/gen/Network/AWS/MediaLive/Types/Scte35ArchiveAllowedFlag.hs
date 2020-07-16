{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35ArchiveAllowedFlag
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Scte35ArchiveAllowedFlag (
  Scte35ArchiveAllowedFlag (
    ..
    , ArchiveAllowed
    , ArchiveNotAllowed
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Corresponds to the archive_allowed parameter. A value of ARCHIVE_NOT_ALLOWED corresponds to 0 (false) in the SCTE-35 specification. If you include one of the "restriction" flags then you must include all four of them.
data Scte35ArchiveAllowedFlag = Scte35ArchiveAllowedFlag' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern ArchiveAllowed :: Scte35ArchiveAllowedFlag
pattern ArchiveAllowed = Scte35ArchiveAllowedFlag' "ARCHIVE_ALLOWED"

pattern ArchiveNotAllowed :: Scte35ArchiveAllowedFlag
pattern ArchiveNotAllowed = Scte35ArchiveAllowedFlag' "ARCHIVE_NOT_ALLOWED"

{-# COMPLETE
  ArchiveAllowed,
  ArchiveNotAllowed,
  Scte35ArchiveAllowedFlag' #-}

instance FromText Scte35ArchiveAllowedFlag where
    parser = (Scte35ArchiveAllowedFlag' . mk) <$> takeText

instance ToText Scte35ArchiveAllowedFlag where
    toText (Scte35ArchiveAllowedFlag' ci) = original ci

-- | Represents an enum of /known/ $Scte35ArchiveAllowedFlag.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Scte35ArchiveAllowedFlag where
    toEnum i = case i of
        0 -> ArchiveAllowed
        1 -> ArchiveNotAllowed
        _ -> (error . showText) $ "Unknown index for Scte35ArchiveAllowedFlag: " <> toText i
    fromEnum x = case x of
        ArchiveAllowed -> 0
        ArchiveNotAllowed -> 1
        Scte35ArchiveAllowedFlag' name -> (error . showText) $ "Unknown Scte35ArchiveAllowedFlag: " <> original name

-- | Represents the bounds of /known/ $Scte35ArchiveAllowedFlag.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Scte35ArchiveAllowedFlag where
    minBound = ArchiveAllowed
    maxBound = ArchiveNotAllowed

instance Hashable     Scte35ArchiveAllowedFlag
instance NFData       Scte35ArchiveAllowedFlag
instance ToByteString Scte35ArchiveAllowedFlag
instance ToQuery      Scte35ArchiveAllowedFlag
instance ToHeader     Scte35ArchiveAllowedFlag

instance ToJSON Scte35ArchiveAllowedFlag where
    toJSON = toJSONText

instance FromJSON Scte35ArchiveAllowedFlag where
    parseJSON = parseJSONText "Scte35ArchiveAllowedFlag"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.DimensionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.DimensionType (
  DimensionType (
    ..
    , DTExclusive
    , DTInclusive
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DimensionType = DimensionType' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern DTExclusive :: DimensionType
pattern DTExclusive = DimensionType' "EXCLUSIVE"

pattern DTInclusive :: DimensionType
pattern DTInclusive = DimensionType' "INCLUSIVE"

{-# COMPLETE
  DTExclusive,
  DTInclusive,
  DimensionType' #-}

instance FromText DimensionType where
    parser = (DimensionType' . mk) <$> takeText

instance ToText DimensionType where
    toText (DimensionType' ci) = original ci

-- | Represents an enum of /known/ $DimensionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DimensionType where
    toEnum i = case i of
        0 -> DTExclusive
        1 -> DTInclusive
        _ -> (error . showText) $ "Unknown index for DimensionType: " <> toText i
    fromEnum x = case x of
        DTExclusive -> 0
        DTInclusive -> 1
        DimensionType' name -> (error . showText) $ "Unknown DimensionType: " <> original name

-- | Represents the bounds of /known/ $DimensionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DimensionType where
    minBound = DTExclusive
    maxBound = DTInclusive

instance Hashable     DimensionType
instance NFData       DimensionType
instance ToByteString DimensionType
instance ToQuery      DimensionType
instance ToHeader     DimensionType

instance ToJSON DimensionType where
    toJSON = toJSONText

instance FromJSON DimensionType where
    parseJSON = parseJSONText "DimensionType"

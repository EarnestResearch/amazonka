{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.InventoryIncludedObjectVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.InventoryIncludedObjectVersions (
  InventoryIncludedObjectVersions (
    ..
    , All
    , Current
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.S3.Internal

data InventoryIncludedObjectVersions = InventoryIncludedObjectVersions' (CI
                                                                           Text)
                                         deriving (Eq, Ord, Read, Show, Data,
                                                   Typeable, Generic)

pattern All :: InventoryIncludedObjectVersions
pattern All = InventoryIncludedObjectVersions' "All"

pattern Current :: InventoryIncludedObjectVersions
pattern Current = InventoryIncludedObjectVersions' "Current"

{-# COMPLETE
  All,
  Current,
  InventoryIncludedObjectVersions' #-}

instance FromText InventoryIncludedObjectVersions where
    parser = (InventoryIncludedObjectVersions' . mk) <$> takeText

instance ToText InventoryIncludedObjectVersions where
    toText (InventoryIncludedObjectVersions' ci) = original ci

-- | Represents an enum of /known/ $InventoryIncludedObjectVersions.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InventoryIncludedObjectVersions where
    toEnum i = case i of
        0 -> All
        1 -> Current
        _ -> (error . showText) $ "Unknown index for InventoryIncludedObjectVersions: " <> toText i
    fromEnum x = case x of
        All -> 0
        Current -> 1
        InventoryIncludedObjectVersions' name -> (error . showText) $ "Unknown InventoryIncludedObjectVersions: " <> original name

-- | Represents the bounds of /known/ $InventoryIncludedObjectVersions.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InventoryIncludedObjectVersions where
    minBound = All
    maxBound = Current

instance Hashable     InventoryIncludedObjectVersions
instance NFData       InventoryIncludedObjectVersions
instance ToByteString InventoryIncludedObjectVersions
instance ToQuery      InventoryIncludedObjectVersions
instance ToHeader     InventoryIncludedObjectVersions

instance FromXML InventoryIncludedObjectVersions where
    parseXML = parseXMLText "InventoryIncludedObjectVersions"

instance ToXML InventoryIncludedObjectVersions where
    toXML = toXMLText

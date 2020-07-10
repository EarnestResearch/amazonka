{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LocationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LocationType (
  LocationType (
    ..
    , AvailabilityZone
    , AvailabilityZoneId
    , Region
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data LocationType = LocationType' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern AvailabilityZone :: LocationType
pattern AvailabilityZone = LocationType' "availability-zone"

pattern AvailabilityZoneId :: LocationType
pattern AvailabilityZoneId = LocationType' "availability-zone-id"

pattern Region :: LocationType
pattern Region = LocationType' "region"

{-# COMPLETE
  AvailabilityZone,
  AvailabilityZoneId,
  Region,
  LocationType' #-}

instance FromText LocationType where
    parser = (LocationType' . mk) <$> takeText

instance ToText LocationType where
    toText (LocationType' ci) = original ci

-- | Represents an enum of /known/ $LocationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LocationType where
    toEnum i = case i of
        0 -> AvailabilityZone
        1 -> AvailabilityZoneId
        2 -> Region
        _ -> (error . showText) $ "Unknown index for LocationType: " <> toText i
    fromEnum x = case x of
        AvailabilityZone -> 0
        AvailabilityZoneId -> 1
        Region -> 2
        LocationType' name -> (error . showText) $ "Unknown LocationType: " <> original name

-- | Represents the bounds of /known/ $LocationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LocationType where
    minBound = AvailabilityZone
    maxBound = Region

instance Hashable     LocationType
instance NFData       LocationType
instance ToByteString LocationType
instance ToQuery      LocationType
instance ToHeader     LocationType

instance FromXML LocationType where
    parseXML = parseXMLText "LocationType"

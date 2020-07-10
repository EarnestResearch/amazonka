{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.AdTriggersElement
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaPackage.Types.AdTriggersElement (
  AdTriggersElement (
    ..
    , Break
    , DistributorAdvertisement
    , DistributorOverlayPlacementOpportunity
    , DistributorPlacementOpportunity
    , ProviderAdvertisement
    , ProviderOverlayPlacementOpportunity
    , ProviderPlacementOpportunity
    , SpliceInsert
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AdTriggersElement = AdTriggersElement' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern Break :: AdTriggersElement
pattern Break = AdTriggersElement' "BREAK"

pattern DistributorAdvertisement :: AdTriggersElement
pattern DistributorAdvertisement = AdTriggersElement' "DISTRIBUTOR_ADVERTISEMENT"

pattern DistributorOverlayPlacementOpportunity :: AdTriggersElement
pattern DistributorOverlayPlacementOpportunity = AdTriggersElement' "DISTRIBUTOR_OVERLAY_PLACEMENT_OPPORTUNITY"

pattern DistributorPlacementOpportunity :: AdTriggersElement
pattern DistributorPlacementOpportunity = AdTriggersElement' "DISTRIBUTOR_PLACEMENT_OPPORTUNITY"

pattern ProviderAdvertisement :: AdTriggersElement
pattern ProviderAdvertisement = AdTriggersElement' "PROVIDER_ADVERTISEMENT"

pattern ProviderOverlayPlacementOpportunity :: AdTriggersElement
pattern ProviderOverlayPlacementOpportunity = AdTriggersElement' "PROVIDER_OVERLAY_PLACEMENT_OPPORTUNITY"

pattern ProviderPlacementOpportunity :: AdTriggersElement
pattern ProviderPlacementOpportunity = AdTriggersElement' "PROVIDER_PLACEMENT_OPPORTUNITY"

pattern SpliceInsert :: AdTriggersElement
pattern SpliceInsert = AdTriggersElement' "SPLICE_INSERT"

{-# COMPLETE
  Break,
  DistributorAdvertisement,
  DistributorOverlayPlacementOpportunity,
  DistributorPlacementOpportunity,
  ProviderAdvertisement,
  ProviderOverlayPlacementOpportunity,
  ProviderPlacementOpportunity,
  SpliceInsert,
  AdTriggersElement' #-}

instance FromText AdTriggersElement where
    parser = (AdTriggersElement' . mk) <$> takeText

instance ToText AdTriggersElement where
    toText (AdTriggersElement' ci) = original ci

-- | Represents an enum of /known/ $AdTriggersElement.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AdTriggersElement where
    toEnum i = case i of
        0 -> Break
        1 -> DistributorAdvertisement
        2 -> DistributorOverlayPlacementOpportunity
        3 -> DistributorPlacementOpportunity
        4 -> ProviderAdvertisement
        5 -> ProviderOverlayPlacementOpportunity
        6 -> ProviderPlacementOpportunity
        7 -> SpliceInsert
        _ -> (error . showText) $ "Unknown index for AdTriggersElement: " <> toText i
    fromEnum x = case x of
        Break -> 0
        DistributorAdvertisement -> 1
        DistributorOverlayPlacementOpportunity -> 2
        DistributorPlacementOpportunity -> 3
        ProviderAdvertisement -> 4
        ProviderOverlayPlacementOpportunity -> 5
        ProviderPlacementOpportunity -> 6
        SpliceInsert -> 7
        AdTriggersElement' name -> (error . showText) $ "Unknown AdTriggersElement: " <> original name

-- | Represents the bounds of /known/ $AdTriggersElement.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AdTriggersElement where
    minBound = Break
    maxBound = SpliceInsert

instance Hashable     AdTriggersElement
instance NFData       AdTriggersElement
instance ToByteString AdTriggersElement
instance ToQuery      AdTriggersElement
instance ToHeader     AdTriggersElement

instance ToJSON AdTriggersElement where
    toJSON = toJSONText

instance FromJSON AdTriggersElement where
    parseJSON = parseJSONText "AdTriggersElement"

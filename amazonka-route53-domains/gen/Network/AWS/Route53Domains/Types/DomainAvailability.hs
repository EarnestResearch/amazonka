{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.DomainAvailability
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53Domains.Types.DomainAvailability (
  DomainAvailability (
    ..
    , DAAvailable
    , DAAvailablePreorder
    , DAAvailableReserved
    , DADontKnow
    , DAReserved
    , DAUnavailable
    , DAUnavailablePremium
    , DAUnavailableRestricted
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DomainAvailability = DomainAvailability' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern DAAvailable :: DomainAvailability
pattern DAAvailable = DomainAvailability' "AVAILABLE"

pattern DAAvailablePreorder :: DomainAvailability
pattern DAAvailablePreorder = DomainAvailability' "AVAILABLE_PREORDER"

pattern DAAvailableReserved :: DomainAvailability
pattern DAAvailableReserved = DomainAvailability' "AVAILABLE_RESERVED"

pattern DADontKnow :: DomainAvailability
pattern DADontKnow = DomainAvailability' "DONT_KNOW"

pattern DAReserved :: DomainAvailability
pattern DAReserved = DomainAvailability' "RESERVED"

pattern DAUnavailable :: DomainAvailability
pattern DAUnavailable = DomainAvailability' "UNAVAILABLE"

pattern DAUnavailablePremium :: DomainAvailability
pattern DAUnavailablePremium = DomainAvailability' "UNAVAILABLE_PREMIUM"

pattern DAUnavailableRestricted :: DomainAvailability
pattern DAUnavailableRestricted = DomainAvailability' "UNAVAILABLE_RESTRICTED"

{-# COMPLETE
  DAAvailable,
  DAAvailablePreorder,
  DAAvailableReserved,
  DADontKnow,
  DAReserved,
  DAUnavailable,
  DAUnavailablePremium,
  DAUnavailableRestricted,
  DomainAvailability' #-}

instance FromText DomainAvailability where
    parser = (DomainAvailability' . mk) <$> takeText

instance ToText DomainAvailability where
    toText (DomainAvailability' ci) = original ci

-- | Represents an enum of /known/ $DomainAvailability.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DomainAvailability where
    toEnum i = case i of
        0 -> DAAvailable
        1 -> DAAvailablePreorder
        2 -> DAAvailableReserved
        3 -> DADontKnow
        4 -> DAReserved
        5 -> DAUnavailable
        6 -> DAUnavailablePremium
        7 -> DAUnavailableRestricted
        _ -> (error . showText) $ "Unknown index for DomainAvailability: " <> toText i
    fromEnum x = case x of
        DAAvailable -> 0
        DAAvailablePreorder -> 1
        DAAvailableReserved -> 2
        DADontKnow -> 3
        DAReserved -> 4
        DAUnavailable -> 5
        DAUnavailablePremium -> 6
        DAUnavailableRestricted -> 7
        DomainAvailability' name -> (error . showText) $ "Unknown DomainAvailability: " <> original name

-- | Represents the bounds of /known/ $DomainAvailability.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DomainAvailability where
    minBound = DAAvailable
    maxBound = DAUnavailableRestricted

instance Hashable     DomainAvailability
instance NFData       DomainAvailability
instance ToByteString DomainAvailability
instance ToQuery      DomainAvailability
instance ToHeader     DomainAvailability

instance FromJSON DomainAvailability where
    parseJSON = parseJSONText "DomainAvailability"

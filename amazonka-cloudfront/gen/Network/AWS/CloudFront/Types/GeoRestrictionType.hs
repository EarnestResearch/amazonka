{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.GeoRestrictionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.GeoRestrictionType where

import Network.AWS.Prelude
  
data GeoRestrictionType = Blacklist
                        | None
                        | Whitelist
                            deriving (Eq, Ord, Read, Show, Enum, Bounded, Data,
                                      Typeable, Generic)

instance FromText GeoRestrictionType where
    parser = takeLowerText >>= \case
        "blacklist" -> pure Blacklist
        "none" -> pure None
        "whitelist" -> pure Whitelist
        e -> fromTextError $ "Failure parsing GeoRestrictionType from value: '" <> e
           <> "'. Accepted values: blacklist, none, whitelist"

instance ToText GeoRestrictionType where
    toText = \case
        Blacklist -> "blacklist"
        None -> "none"
        Whitelist -> "whitelist"

instance Hashable     GeoRestrictionType
instance NFData       GeoRestrictionType
instance ToByteString GeoRestrictionType
instance ToQuery      GeoRestrictionType
instance ToHeader     GeoRestrictionType

instance FromXML GeoRestrictionType where
    parseXML = parseXMLText "GeoRestrictionType"

instance ToXML GeoRestrictionType where
    toXML = toXMLText

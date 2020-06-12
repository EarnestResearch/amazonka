{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.MembershipType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.MembershipType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude
  
data MembershipType = MTIgmp
                    | MTStatic
                        deriving (Eq, Ord, Read, Show, Enum, Bounded, Data,
                                  Typeable, Generic)

instance FromText MembershipType where
    parser = takeLowerText >>= \case
        "igmp" -> pure MTIgmp
        "static" -> pure MTStatic
        e -> fromTextError $ "Failure parsing MembershipType from value: '" <> e
           <> "'. Accepted values: igmp, static"

instance ToText MembershipType where
    toText = \case
        MTIgmp -> "igmp"
        MTStatic -> "static"

instance Hashable     MembershipType
instance NFData       MembershipType
instance ToByteString MembershipType
instance ToQuery      MembershipType
instance ToHeader     MembershipType

instance FromXML MembershipType where
    parseXML = parseXMLText "MembershipType"

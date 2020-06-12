{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LocalGatewayRouteType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LocalGatewayRouteType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude
  
data LocalGatewayRouteType = LGRTPropagated
                           | LGRTStatic
                               deriving (Eq, Ord, Read, Show, Enum, Bounded,
                                         Data, Typeable, Generic)

instance FromText LocalGatewayRouteType where
    parser = takeLowerText >>= \case
        "propagated" -> pure LGRTPropagated
        "static" -> pure LGRTStatic
        e -> fromTextError $ "Failure parsing LocalGatewayRouteType from value: '" <> e
           <> "'. Accepted values: propagated, static"

instance ToText LocalGatewayRouteType where
    toText = \case
        LGRTPropagated -> "propagated"
        LGRTStatic -> "static"

instance Hashable     LocalGatewayRouteType
instance NFData       LocalGatewayRouteType
instance ToByteString LocalGatewayRouteType
instance ToQuery      LocalGatewayRouteType
instance ToHeader     LocalGatewayRouteType

instance FromXML LocalGatewayRouteType where
    parseXML = parseXMLText "LocalGatewayRouteType"

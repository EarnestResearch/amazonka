{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficMirrorTargetType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TrafficMirrorTargetType (
  TrafficMirrorTargetType (
    ..
    , NetworkInterface
    , NetworkLoadBalancer
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TrafficMirrorTargetType = TrafficMirrorTargetType' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern NetworkInterface :: TrafficMirrorTargetType
pattern NetworkInterface = TrafficMirrorTargetType' "network-interface"

pattern NetworkLoadBalancer :: TrafficMirrorTargetType
pattern NetworkLoadBalancer = TrafficMirrorTargetType' "network-load-balancer"

{-# COMPLETE
  NetworkInterface,
  NetworkLoadBalancer,
  TrafficMirrorTargetType' #-}

instance FromText TrafficMirrorTargetType where
    parser = (TrafficMirrorTargetType' . mk) <$> takeText

instance ToText TrafficMirrorTargetType where
    toText (TrafficMirrorTargetType' ci) = original ci

-- | Represents an enum of /known/ $TrafficMirrorTargetType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TrafficMirrorTargetType where
    toEnum i = case i of
        0 -> NetworkInterface
        1 -> NetworkLoadBalancer
        _ -> (error . showText) $ "Unknown index for TrafficMirrorTargetType: " <> toText i
    fromEnum x = case x of
        NetworkInterface -> 0
        NetworkLoadBalancer -> 1
        TrafficMirrorTargetType' name -> (error . showText) $ "Unknown TrafficMirrorTargetType: " <> original name

-- | Represents the bounds of /known/ $TrafficMirrorTargetType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TrafficMirrorTargetType where
    minBound = NetworkInterface
    maxBound = NetworkLoadBalancer

instance Hashable     TrafficMirrorTargetType
instance NFData       TrafficMirrorTargetType
instance ToByteString TrafficMirrorTargetType
instance ToQuery      TrafficMirrorTargetType
instance ToHeader     TrafficMirrorTargetType

instance FromXML TrafficMirrorTargetType where
    parseXML = parseXMLText "TrafficMirrorTargetType"

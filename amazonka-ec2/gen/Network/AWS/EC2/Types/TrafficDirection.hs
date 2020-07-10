{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficDirection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TrafficDirection (
  TrafficDirection (
    ..
    , Egress
    , Ingress
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TrafficDirection = TrafficDirection' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern Egress :: TrafficDirection
pattern Egress = TrafficDirection' "egress"

pattern Ingress :: TrafficDirection
pattern Ingress = TrafficDirection' "ingress"

{-# COMPLETE
  Egress,
  Ingress,
  TrafficDirection' #-}

instance FromText TrafficDirection where
    parser = (TrafficDirection' . mk) <$> takeText

instance ToText TrafficDirection where
    toText (TrafficDirection' ci) = original ci

-- | Represents an enum of /known/ $TrafficDirection.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TrafficDirection where
    toEnum i = case i of
        0 -> Egress
        1 -> Ingress
        _ -> (error . showText) $ "Unknown index for TrafficDirection: " <> toText i
    fromEnum x = case x of
        Egress -> 0
        Ingress -> 1
        TrafficDirection' name -> (error . showText) $ "Unknown TrafficDirection: " <> original name

-- | Represents the bounds of /known/ $TrafficDirection.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TrafficDirection where
    minBound = Egress
    maxBound = Ingress

instance Hashable     TrafficDirection
instance NFData       TrafficDirection
instance ToByteString TrafficDirection
instance ToQuery      TrafficDirection
instance ToHeader     TrafficDirection

instance FromXML TrafficDirection where
    parseXML = parseXMLText "TrafficDirection"

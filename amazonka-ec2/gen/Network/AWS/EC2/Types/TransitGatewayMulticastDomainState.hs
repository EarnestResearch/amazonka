{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TransitGatewayMulticastDomainState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TransitGatewayMulticastDomainState (
  TransitGatewayMulticastDomainState (
    ..
    , TGMDSAvailable
    , TGMDSDeleted
    , TGMDSDeleting
    , TGMDSPending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TransitGatewayMulticastDomainState = TransitGatewayMulticastDomainState' (CI
                                                                                 Text)
                                            deriving (Eq, Ord, Read, Show, Data,
                                                      Typeable, Generic)

pattern TGMDSAvailable :: TransitGatewayMulticastDomainState
pattern TGMDSAvailable = TransitGatewayMulticastDomainState' "available"

pattern TGMDSDeleted :: TransitGatewayMulticastDomainState
pattern TGMDSDeleted = TransitGatewayMulticastDomainState' "deleted"

pattern TGMDSDeleting :: TransitGatewayMulticastDomainState
pattern TGMDSDeleting = TransitGatewayMulticastDomainState' "deleting"

pattern TGMDSPending :: TransitGatewayMulticastDomainState
pattern TGMDSPending = TransitGatewayMulticastDomainState' "pending"

{-# COMPLETE
  TGMDSAvailable,
  TGMDSDeleted,
  TGMDSDeleting,
  TGMDSPending,
  TransitGatewayMulticastDomainState' #-}

instance FromText TransitGatewayMulticastDomainState where
    parser = (TransitGatewayMulticastDomainState' . mk) <$> takeText

instance ToText TransitGatewayMulticastDomainState where
    toText (TransitGatewayMulticastDomainState' ci) = original ci

-- | Represents an enum of /known/ $TransitGatewayMulticastDomainState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TransitGatewayMulticastDomainState where
    toEnum i = case i of
        0 -> TGMDSAvailable
        1 -> TGMDSDeleted
        2 -> TGMDSDeleting
        3 -> TGMDSPending
        _ -> (error . showText) $ "Unknown index for TransitGatewayMulticastDomainState: " <> toText i
    fromEnum x = case x of
        TGMDSAvailable -> 0
        TGMDSDeleted -> 1
        TGMDSDeleting -> 2
        TGMDSPending -> 3
        TransitGatewayMulticastDomainState' name -> (error . showText) $ "Unknown TransitGatewayMulticastDomainState: " <> original name

-- | Represents the bounds of /known/ $TransitGatewayMulticastDomainState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TransitGatewayMulticastDomainState where
    minBound = TGMDSAvailable
    maxBound = TGMDSPending

instance Hashable     TransitGatewayMulticastDomainState
instance NFData       TransitGatewayMulticastDomainState
instance ToByteString TransitGatewayMulticastDomainState
instance ToQuery      TransitGatewayMulticastDomainState
instance ToHeader     TransitGatewayMulticastDomainState

instance FromXML TransitGatewayMulticastDomainState where
    parseXML = parseXMLText "TransitGatewayMulticastDomainState"

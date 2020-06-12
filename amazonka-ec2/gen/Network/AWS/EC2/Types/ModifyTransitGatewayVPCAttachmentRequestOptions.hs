{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ModifyTransitGatewayVPCAttachmentRequestOptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ModifyTransitGatewayVPCAttachmentRequestOptions where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DNSSupportValue
import Network.AWS.EC2.Types.IPv6SupportValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the options for a VPC attachment.
--
--
--
-- /See:/ 'modifyTransitGatewayVPCAttachmentRequestOptions' smart constructor.
data ModifyTransitGatewayVPCAttachmentRequestOptions = ModifyTransitGatewayVPCAttachmentRequestOptions'{_mtgvaroIPv6Support
                                                                                                        ::
                                                                                                        !(Maybe
                                                                                                            IPv6SupportValue),
                                                                                                        _mtgvaroDNSSupport
                                                                                                        ::
                                                                                                        !(Maybe
                                                                                                            DNSSupportValue)}
                                                         deriving (Eq, Read,
                                                                   Show, Data,
                                                                   Typeable,
                                                                   Generic)

-- | Creates a value of 'ModifyTransitGatewayVPCAttachmentRequestOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mtgvaroIPv6Support' - Enable or disable IPv6 support. The default is @enable@ .
--
-- * 'mtgvaroDNSSupport' - Enable or disable DNS support. The default is @enable@ .
modifyTransitGatewayVPCAttachmentRequestOptions
    :: ModifyTransitGatewayVPCAttachmentRequestOptions
modifyTransitGatewayVPCAttachmentRequestOptions
  = ModifyTransitGatewayVPCAttachmentRequestOptions'{_mtgvaroIPv6Support
                                                       = Nothing,
                                                     _mtgvaroDNSSupport =
                                                       Nothing}

-- | Enable or disable IPv6 support. The default is @enable@ .
mtgvaroIPv6Support :: Lens' ModifyTransitGatewayVPCAttachmentRequestOptions (Maybe IPv6SupportValue)
mtgvaroIPv6Support = lens _mtgvaroIPv6Support (\ s a -> s{_mtgvaroIPv6Support = a})

-- | Enable or disable DNS support. The default is @enable@ .
mtgvaroDNSSupport :: Lens' ModifyTransitGatewayVPCAttachmentRequestOptions (Maybe DNSSupportValue)
mtgvaroDNSSupport = lens _mtgvaroDNSSupport (\ s a -> s{_mtgvaroDNSSupport = a})

instance Hashable
           ModifyTransitGatewayVPCAttachmentRequestOptions
         where

instance NFData
           ModifyTransitGatewayVPCAttachmentRequestOptions
         where

instance ToQuery
           ModifyTransitGatewayVPCAttachmentRequestOptions
         where
        toQuery
          ModifyTransitGatewayVPCAttachmentRequestOptions'{..}
          = mconcat
              ["Ipv6Support" =: _mtgvaroIPv6Support,
               "DnsSupport" =: _mtgvaroDNSSupport]

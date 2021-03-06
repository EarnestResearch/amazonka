{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTransitGatewayPeeringAttachment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a transit gateway peering attachment.
--
--
module Network.AWS.EC2.DeleteTransitGatewayPeeringAttachment
    (
    -- * Creating a Request
      deleteTransitGatewayPeeringAttachment
    , DeleteTransitGatewayPeeringAttachment
    -- * Request Lenses
    , dlttrnstgtwyprngattchmntDryRun
    , dlttrnstgtwyprngattchmntTransitGatewayAttachmentId

    -- * Destructuring the Response
    , deleteTransitGatewayPeeringAttachmentResponse
    , DeleteTransitGatewayPeeringAttachmentResponse
    -- * Response Lenses
    , dtgparsTransitGatewayPeeringAttachment
    , dtgparsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteTransitGatewayPeeringAttachment' smart constructor.
data DeleteTransitGatewayPeeringAttachment = DeleteTransitGatewayPeeringAttachment'{_dlttrnstgtwyprngattchmntDryRun
                                                                                    ::
                                                                                    !(Maybe
                                                                                        Bool),
                                                                                    _dlttrnstgtwyprngattchmntTransitGatewayAttachmentId
                                                                                    ::
                                                                                    !Text}
                                               deriving (Eq, Read, Show, Data,
                                                         Typeable, Generic)

-- | Creates a value of 'DeleteTransitGatewayPeeringAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlttrnstgtwyprngattchmntDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dlttrnstgtwyprngattchmntTransitGatewayAttachmentId' - The ID of the transit gateway peering attachment.
deleteTransitGatewayPeeringAttachment
    :: Text -- ^ 'dlttrnstgtwyprngattchmntTransitGatewayAttachmentId'
    -> DeleteTransitGatewayPeeringAttachment
deleteTransitGatewayPeeringAttachment
  pTransitGatewayAttachmentId_
  = DeleteTransitGatewayPeeringAttachment'{_dlttrnstgtwyprngattchmntDryRun
                                             = Nothing,
                                           _dlttrnstgtwyprngattchmntTransitGatewayAttachmentId
                                             = pTransitGatewayAttachmentId_}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dlttrnstgtwyprngattchmntDryRun :: Lens' DeleteTransitGatewayPeeringAttachment (Maybe Bool)
dlttrnstgtwyprngattchmntDryRun = lens _dlttrnstgtwyprngattchmntDryRun (\ s a -> s{_dlttrnstgtwyprngattchmntDryRun = a})

-- | The ID of the transit gateway peering attachment.
dlttrnstgtwyprngattchmntTransitGatewayAttachmentId :: Lens' DeleteTransitGatewayPeeringAttachment Text
dlttrnstgtwyprngattchmntTransitGatewayAttachmentId = lens _dlttrnstgtwyprngattchmntTransitGatewayAttachmentId (\ s a -> s{_dlttrnstgtwyprngattchmntTransitGatewayAttachmentId = a})

instance AWSRequest
           DeleteTransitGatewayPeeringAttachment
         where
        type Rs DeleteTransitGatewayPeeringAttachment =
             DeleteTransitGatewayPeeringAttachmentResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DeleteTransitGatewayPeeringAttachmentResponse' <$>
                   (x .@? "transitGatewayPeeringAttachment") <*>
                     (pure (fromEnum s)))

instance Hashable
           DeleteTransitGatewayPeeringAttachment
         where

instance NFData DeleteTransitGatewayPeeringAttachment
         where

instance ToHeaders
           DeleteTransitGatewayPeeringAttachment
         where
        toHeaders = const mempty

instance ToPath DeleteTransitGatewayPeeringAttachment
         where
        toPath = const "/"

instance ToQuery
           DeleteTransitGatewayPeeringAttachment
         where
        toQuery DeleteTransitGatewayPeeringAttachment'{..}
          = mconcat
              ["Action" =:
                 ("DeleteTransitGatewayPeeringAttachment" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dlttrnstgtwyprngattchmntDryRun,
               "TransitGatewayAttachmentId" =:
                 _dlttrnstgtwyprngattchmntTransitGatewayAttachmentId]

-- | /See:/ 'deleteTransitGatewayPeeringAttachmentResponse' smart constructor.
data DeleteTransitGatewayPeeringAttachmentResponse = DeleteTransitGatewayPeeringAttachmentResponse'{_dtgparsTransitGatewayPeeringAttachment
                                                                                                    ::
                                                                                                    !(Maybe
                                                                                                        TransitGatewayPeeringAttachment),
                                                                                                    _dtgparsResponseStatus
                                                                                                    ::
                                                                                                    !Int}
                                                       deriving (Eq, Read, Show,
                                                                 Data, Typeable,
                                                                 Generic)

-- | Creates a value of 'DeleteTransitGatewayPeeringAttachmentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgparsTransitGatewayPeeringAttachment' - The transit gateway peering attachment.
--
-- * 'dtgparsResponseStatus' - -- | The response status code.
deleteTransitGatewayPeeringAttachmentResponse
    :: Int -- ^ 'dtgparsResponseStatus'
    -> DeleteTransitGatewayPeeringAttachmentResponse
deleteTransitGatewayPeeringAttachmentResponse
  pResponseStatus_
  = DeleteTransitGatewayPeeringAttachmentResponse'{_dtgparsTransitGatewayPeeringAttachment
                                                     = Nothing,
                                                   _dtgparsResponseStatus =
                                                     pResponseStatus_}

-- | The transit gateway peering attachment.
dtgparsTransitGatewayPeeringAttachment :: Lens' DeleteTransitGatewayPeeringAttachmentResponse (Maybe TransitGatewayPeeringAttachment)
dtgparsTransitGatewayPeeringAttachment = lens _dtgparsTransitGatewayPeeringAttachment (\ s a -> s{_dtgparsTransitGatewayPeeringAttachment = a})

-- | -- | The response status code.
dtgparsResponseStatus :: Lens' DeleteTransitGatewayPeeringAttachmentResponse Int
dtgparsResponseStatus = lens _dtgparsResponseStatus (\ s a -> s{_dtgparsResponseStatus = a})

instance NFData
           DeleteTransitGatewayPeeringAttachmentResponse
         where

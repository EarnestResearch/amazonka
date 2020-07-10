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
-- Module      : Network.AWS.DMS.DeleteCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified certificate. 
--
--
module Network.AWS.DMS.DeleteCertificate
    (
    -- * Creating a Request
      deleteCertificate
    , DeleteCertificate
    -- * Request Lenses
    , dcCertificateARN

    -- * Destructuring the Response
    , deleteCertificateResponse
    , DeleteCertificateResponse
    -- * Response Lenses
    , dltcrtfctrsCertificate
    , dltcrtfctrsResponseStatus
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteCertificate' smart constructor.
newtype DeleteCertificate = DeleteCertificate'{_dcCertificateARN
                                               :: Text}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcCertificateARN' - The Amazon Resource Name (ARN) of the deleted certificate.
deleteCertificate
    :: Text -- ^ 'dcCertificateARN'
    -> DeleteCertificate
deleteCertificate pCertificateARN_
  = DeleteCertificate'{_dcCertificateARN =
                         pCertificateARN_}

-- | The Amazon Resource Name (ARN) of the deleted certificate.
dcCertificateARN :: Lens' DeleteCertificate Text
dcCertificateARN = lens _dcCertificateARN (\ s a -> s{_dcCertificateARN = a})

instance AWSRequest DeleteCertificate where
        type Rs DeleteCertificate = DeleteCertificateResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 DeleteCertificateResponse' <$>
                   (x .?> "Certificate") <*> (pure (fromEnum s)))

instance Hashable DeleteCertificate where

instance NFData DeleteCertificate where

instance ToHeaders DeleteCertificate where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.DeleteCertificate" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteCertificate where
        toJSON DeleteCertificate'{..}
          = object
              (catMaybes
                 [Just ("CertificateArn" .= _dcCertificateARN)])

instance ToPath DeleteCertificate where
        toPath = const "/"

instance ToQuery DeleteCertificate where
        toQuery = const mempty

-- | /See:/ 'deleteCertificateResponse' smart constructor.
data DeleteCertificateResponse = DeleteCertificateResponse'{_dltcrtfctrsCertificate
                                                            ::
                                                            !(Maybe
                                                                Certificate),
                                                            _dltcrtfctrsResponseStatus
                                                            :: !Int}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'DeleteCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltcrtfctrsCertificate' - The Secure Sockets Layer (SSL) certificate.
--
-- * 'dltcrtfctrsResponseStatus' - -- | The response status code.
deleteCertificateResponse
    :: Int -- ^ 'dltcrtfctrsResponseStatus'
    -> DeleteCertificateResponse
deleteCertificateResponse pResponseStatus_
  = DeleteCertificateResponse'{_dltcrtfctrsCertificate
                                 = Nothing,
                               _dltcrtfctrsResponseStatus = pResponseStatus_}

-- | The Secure Sockets Layer (SSL) certificate.
dltcrtfctrsCertificate :: Lens' DeleteCertificateResponse (Maybe Certificate)
dltcrtfctrsCertificate = lens _dltcrtfctrsCertificate (\ s a -> s{_dltcrtfctrsCertificate = a})

-- | -- | The response status code.
dltcrtfctrsResponseStatus :: Lens' DeleteCertificateResponse Int
dltcrtfctrsResponseStatus = lens _dltcrtfctrsResponseStatus (\ s a -> s{_dltcrtfctrsResponseStatus = a})

instance NFData DeleteCertificateResponse where

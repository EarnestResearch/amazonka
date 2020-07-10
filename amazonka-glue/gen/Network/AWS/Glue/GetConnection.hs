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
-- Module      : Network.AWS.Glue.GetConnection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a connection definition from the Data Catalog.
--
--
module Network.AWS.Glue.GetConnection
    (
    -- * Creating a Request
      getConnection
    , GetConnection
    -- * Request Lenses
    , gCatalogId
    , gHidePassword
    , gName

    -- * Destructuring the Response
    , getConnectionResponse
    , GetConnectionResponse
    -- * Response Lenses
    , gtcnnctnrsConnection
    , gtcnnctnrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getConnection' smart constructor.
data GetConnection = GetConnection'{_gCatalogId ::
                                    !(Maybe Text),
                                    _gHidePassword :: !(Maybe Bool),
                                    _gName :: !Text}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetConnection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gCatalogId' - The ID of the Data Catalog in which the connection resides. If none is provided, the AWS account ID is used by default.
--
-- * 'gHidePassword' - Allows you to retrieve the connection metadata without returning the password. For instance, the AWS Glue console uses this flag to retrieve the connection, and does not display the password. Set this parameter when the caller might not have permission to use the AWS KMS key to decrypt the password, but it does have permission to access the rest of the connection properties.
--
-- * 'gName' - The name of the connection definition to retrieve.
getConnection
    :: Text -- ^ 'gName'
    -> GetConnection
getConnection pName_
  = GetConnection'{_gCatalogId = Nothing,
                   _gHidePassword = Nothing, _gName = pName_}

-- | The ID of the Data Catalog in which the connection resides. If none is provided, the AWS account ID is used by default.
gCatalogId :: Lens' GetConnection (Maybe Text)
gCatalogId = lens _gCatalogId (\ s a -> s{_gCatalogId = a})

-- | Allows you to retrieve the connection metadata without returning the password. For instance, the AWS Glue console uses this flag to retrieve the connection, and does not display the password. Set this parameter when the caller might not have permission to use the AWS KMS key to decrypt the password, but it does have permission to access the rest of the connection properties.
gHidePassword :: Lens' GetConnection (Maybe Bool)
gHidePassword = lens _gHidePassword (\ s a -> s{_gHidePassword = a})

-- | The name of the connection definition to retrieve.
gName :: Lens' GetConnection Text
gName = lens _gName (\ s a -> s{_gName = a})

instance AWSRequest GetConnection where
        type Rs GetConnection = GetConnectionResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 GetConnectionResponse' <$>
                   (x .?> "Connection") <*> (pure (fromEnum s)))

instance Hashable GetConnection where

instance NFData GetConnection where

instance ToHeaders GetConnection where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.GetConnection" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetConnection where
        toJSON GetConnection'{..}
          = object
              (catMaybes
                 [("CatalogId" .=) <$> _gCatalogId,
                  ("HidePassword" .=) <$> _gHidePassword,
                  Just ("Name" .= _gName)])

instance ToPath GetConnection where
        toPath = const "/"

instance ToQuery GetConnection where
        toQuery = const mempty

-- | /See:/ 'getConnectionResponse' smart constructor.
data GetConnectionResponse = GetConnectionResponse'{_gtcnnctnrsConnection
                                                    :: !(Maybe Connection),
                                                    _gtcnnctnrsResponseStatus ::
                                                    !Int}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'GetConnectionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtcnnctnrsConnection' - The requested connection definition.
--
-- * 'gtcnnctnrsResponseStatus' - -- | The response status code.
getConnectionResponse
    :: Int -- ^ 'gtcnnctnrsResponseStatus'
    -> GetConnectionResponse
getConnectionResponse pResponseStatus_
  = GetConnectionResponse'{_gtcnnctnrsConnection =
                             Nothing,
                           _gtcnnctnrsResponseStatus = pResponseStatus_}

-- | The requested connection definition.
gtcnnctnrsConnection :: Lens' GetConnectionResponse (Maybe Connection)
gtcnnctnrsConnection = lens _gtcnnctnrsConnection (\ s a -> s{_gtcnnctnrsConnection = a})

-- | -- | The response status code.
gtcnnctnrsResponseStatus :: Lens' GetConnectionResponse Int
gtcnnctnrsResponseStatus = lens _gtcnnctnrsResponseStatus (\ s a -> s{_gtcnnctnrsResponseStatus = a})

instance NFData GetConnectionResponse where

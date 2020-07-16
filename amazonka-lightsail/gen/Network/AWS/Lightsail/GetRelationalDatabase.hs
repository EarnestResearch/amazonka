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
-- Module      : Network.AWS.Lightsail.GetRelationalDatabase
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific database in Amazon Lightsail.
--
--
module Network.AWS.Lightsail.GetRelationalDatabase
    (
    -- * Creating a Request
      getRelationalDatabase
    , GetRelationalDatabase
    -- * Request Lenses
    , grdRelationalDatabaseName

    -- * Destructuring the Response
    , getRelationalDatabaseResponse
    , GetRelationalDatabaseResponse
    -- * Response Lenses
    , gtrltnldtbsrsRelationalDatabase
    , gtrltnldtbsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getRelationalDatabase' smart constructor.
newtype GetRelationalDatabase = GetRelationalDatabase'{_grdRelationalDatabaseName
                                                       :: Text}
                                  deriving (Eq, Read, Show, Data, Typeable,
                                            Generic)

-- | Creates a value of 'GetRelationalDatabase' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdRelationalDatabaseName' - The name of the database that you are looking up.
getRelationalDatabase
    :: Text -- ^ 'grdRelationalDatabaseName'
    -> GetRelationalDatabase
getRelationalDatabase pRelationalDatabaseName_
  = GetRelationalDatabase'{_grdRelationalDatabaseName =
                             pRelationalDatabaseName_}

-- | The name of the database that you are looking up.
grdRelationalDatabaseName :: Lens' GetRelationalDatabase Text
grdRelationalDatabaseName = lens _grdRelationalDatabaseName (\ s a -> s{_grdRelationalDatabaseName = a})

instance AWSRequest GetRelationalDatabase where
        type Rs GetRelationalDatabase =
             GetRelationalDatabaseResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetRelationalDatabaseResponse' <$>
                   (x .?> "relationalDatabase") <*> (pure (fromEnum s)))

instance Hashable GetRelationalDatabase where

instance NFData GetRelationalDatabase where

instance ToHeaders GetRelationalDatabase where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetRelationalDatabase" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetRelationalDatabase where
        toJSON GetRelationalDatabase'{..}
          = object
              (catMaybes
                 [Just
                    ("relationalDatabaseName" .=
                       _grdRelationalDatabaseName)])

instance ToPath GetRelationalDatabase where
        toPath = const "/"

instance ToQuery GetRelationalDatabase where
        toQuery = const mempty

-- | /See:/ 'getRelationalDatabaseResponse' smart constructor.
data GetRelationalDatabaseResponse = GetRelationalDatabaseResponse'{_gtrltnldtbsrsRelationalDatabase
                                                                    ::
                                                                    !(Maybe
                                                                        RelationalDatabase),
                                                                    _gtrltnldtbsrsResponseStatus
                                                                    :: !Int}
                                       deriving (Eq, Read, Show, Data, Typeable,
                                                 Generic)

-- | Creates a value of 'GetRelationalDatabaseResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtrltnldtbsrsRelationalDatabase' - An object describing the specified database.
--
-- * 'gtrltnldtbsrsResponseStatus' - -- | The response status code.
getRelationalDatabaseResponse
    :: Int -- ^ 'gtrltnldtbsrsResponseStatus'
    -> GetRelationalDatabaseResponse
getRelationalDatabaseResponse pResponseStatus_
  = GetRelationalDatabaseResponse'{_gtrltnldtbsrsRelationalDatabase
                                     = Nothing,
                                   _gtrltnldtbsrsResponseStatus =
                                     pResponseStatus_}

-- | An object describing the specified database.
gtrltnldtbsrsRelationalDatabase :: Lens' GetRelationalDatabaseResponse (Maybe RelationalDatabase)
gtrltnldtbsrsRelationalDatabase = lens _gtrltnldtbsrsRelationalDatabase (\ s a -> s{_gtrltnldtbsrsRelationalDatabase = a})

-- | -- | The response status code.
gtrltnldtbsrsResponseStatus :: Lens' GetRelationalDatabaseResponse Int
gtrltnldtbsrsResponseStatus = lens _gtrltnldtbsrsResponseStatus (\ s a -> s{_gtrltnldtbsrsResponseStatus = a})

instance NFData GetRelationalDatabaseResponse where

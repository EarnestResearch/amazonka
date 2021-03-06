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
-- Module      : Network.AWS.Athena.GetQueryResults
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Streams the results of a single query execution specified by @QueryExecutionId@ from the Athena query results location in Amazon S3. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results> in the /Amazon Athena User Guide/ . This request does not execute the query but returns results. Use 'StartQueryExecution' to run a query.
--
--
-- To stream query results successfully, the IAM principal with permission to call @GetQueryResults@ also must have permissions to the Amazon S3 @GetObject@ action for the Athena query results location.
--
-- /Important:/ IAM principals with permission to the Amazon S3 @GetObject@ action for the query results location are able to retrieve query results from Amazon S3 even if permission to the @GetQueryResults@ action is denied. To restrict user or role access, ensure that Amazon S3 permissions to the Athena query location are denied.
--
--
-- This operation returns paginated results.
module Network.AWS.Athena.GetQueryResults
    (
    -- * Creating a Request
      getQueryResults
    , GetQueryResults
    -- * Request Lenses
    , gqrNextToken
    , gqrMaxResults
    , gqrQueryExecutionId

    -- * Destructuring the Response
    , getQueryResultsResponse
    , GetQueryResultsResponse
    -- * Response Lenses
    , gqrrsUpdateCount
    , gqrrsNextToken
    , gqrrsResultSet
    , gqrrsResponseStatus
    ) where

import Network.AWS.Athena.Types
import Network.AWS.Athena.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getQueryResults' smart constructor.
data GetQueryResults = GetQueryResults'{_gqrNextToken
                                        :: !(Maybe Text),
                                        _gqrMaxResults :: !(Maybe Nat),
                                        _gqrQueryExecutionId :: !Text}
                         deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetQueryResults' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gqrNextToken' - The token that specifies where to start pagination if a previous request was truncated.
--
-- * 'gqrMaxResults' - The maximum number of results (rows) to return in this request.
--
-- * 'gqrQueryExecutionId' - The unique ID of the query execution.
getQueryResults
    :: Text -- ^ 'gqrQueryExecutionId'
    -> GetQueryResults
getQueryResults pQueryExecutionId_
  = GetQueryResults'{_gqrNextToken = Nothing,
                     _gqrMaxResults = Nothing,
                     _gqrQueryExecutionId = pQueryExecutionId_}

-- | The token that specifies where to start pagination if a previous request was truncated.
gqrNextToken :: Lens' GetQueryResults (Maybe Text)
gqrNextToken = lens _gqrNextToken (\ s a -> s{_gqrNextToken = a})

-- | The maximum number of results (rows) to return in this request.
gqrMaxResults :: Lens' GetQueryResults (Maybe Natural)
gqrMaxResults = lens _gqrMaxResults (\ s a -> s{_gqrMaxResults = a}) . mapping _Nat

-- | The unique ID of the query execution.
gqrQueryExecutionId :: Lens' GetQueryResults Text
gqrQueryExecutionId = lens _gqrQueryExecutionId (\ s a -> s{_gqrQueryExecutionId = a})

instance AWSPager GetQueryResults where
        page rq rs
          | stop (rs ^. gqrrsNextToken) = Nothing
          | stop (rs ^? gqrrsResultSet . _Just . rsRows) =
            Nothing
          | otherwise =
            Just $ rq & gqrNextToken .~ rs ^. gqrrsNextToken

instance AWSRequest GetQueryResults where
        type Rs GetQueryResults = GetQueryResultsResponse
        request = postJSON athena
        response
          = receiveJSON
              (\ s h x ->
                 GetQueryResultsResponse' <$>
                   (x .?> "UpdateCount") <*> (x .?> "NextToken") <*>
                     (x .?> "ResultSet")
                     <*> (pure (fromEnum s)))

instance Hashable GetQueryResults where

instance NFData GetQueryResults where

instance ToHeaders GetQueryResults where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonAthena.GetQueryResults" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetQueryResults where
        toJSON GetQueryResults'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _gqrNextToken,
                  ("MaxResults" .=) <$> _gqrMaxResults,
                  Just ("QueryExecutionId" .= _gqrQueryExecutionId)])

instance ToPath GetQueryResults where
        toPath = const "/"

instance ToQuery GetQueryResults where
        toQuery = const mempty

-- | /See:/ 'getQueryResultsResponse' smart constructor.
data GetQueryResultsResponse = GetQueryResultsResponse'{_gqrrsUpdateCount
                                                        :: !(Maybe Integer),
                                                        _gqrrsNextToken ::
                                                        !(Maybe Text),
                                                        _gqrrsResultSet ::
                                                        !(Maybe ResultSet),
                                                        _gqrrsResponseStatus ::
                                                        !Int}
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'GetQueryResultsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gqrrsUpdateCount' - The number of rows inserted with a CREATE TABLE AS SELECT statement. 
--
-- * 'gqrrsNextToken' - A token to be used by the next request if this request is truncated.
--
-- * 'gqrrsResultSet' - The results of the query execution.
--
-- * 'gqrrsResponseStatus' - -- | The response status code.
getQueryResultsResponse
    :: Int -- ^ 'gqrrsResponseStatus'
    -> GetQueryResultsResponse
getQueryResultsResponse pResponseStatus_
  = GetQueryResultsResponse'{_gqrrsUpdateCount =
                               Nothing,
                             _gqrrsNextToken = Nothing,
                             _gqrrsResultSet = Nothing,
                             _gqrrsResponseStatus = pResponseStatus_}

-- | The number of rows inserted with a CREATE TABLE AS SELECT statement. 
gqrrsUpdateCount :: Lens' GetQueryResultsResponse (Maybe Integer)
gqrrsUpdateCount = lens _gqrrsUpdateCount (\ s a -> s{_gqrrsUpdateCount = a})

-- | A token to be used by the next request if this request is truncated.
gqrrsNextToken :: Lens' GetQueryResultsResponse (Maybe Text)
gqrrsNextToken = lens _gqrrsNextToken (\ s a -> s{_gqrrsNextToken = a})

-- | The results of the query execution.
gqrrsResultSet :: Lens' GetQueryResultsResponse (Maybe ResultSet)
gqrrsResultSet = lens _gqrrsResultSet (\ s a -> s{_gqrrsResultSet = a})

-- | -- | The response status code.
gqrrsResponseStatus :: Lens' GetQueryResultsResponse Int
gqrrsResponseStatus = lens _gqrrsResponseStatus (\ s a -> s{_gqrrsResponseStatus = a})

instance NFData GetQueryResultsResponse where

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
-- Module      : Network.AWS.SageMaker.ListAlgorithms
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the machine learning algorithms that have been created.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListAlgorithms
    (
    -- * Creating a Request
      listAlgorithms
    , ListAlgorithms
    -- * Request Lenses
    , lNameContains
    , lCreationTimeAfter
    , lNextToken
    , lSortOrder
    , lCreationTimeBefore
    , lMaxResults
    , lSortBy

    -- * Destructuring the Response
    , listAlgorithmsResponse
    , ListAlgorithmsResponse
    -- * Response Lenses
    , lrsNextToken
    , lrsResponseStatus
    , lrsAlgorithmSummaryList
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'listAlgorithms' smart constructor.
data ListAlgorithms = ListAlgorithms'{_lNameContains
                                      :: !(Maybe Text),
                                      _lCreationTimeAfter :: !(Maybe POSIX),
                                      _lNextToken :: !(Maybe Text),
                                      _lSortOrder :: !(Maybe SortOrder),
                                      _lCreationTimeBefore :: !(Maybe POSIX),
                                      _lMaxResults :: !(Maybe Nat),
                                      _lSortBy :: !(Maybe AlgorithmSortBy)}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListAlgorithms' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lNameContains' - A string in the algorithm name. This filter returns only algorithms whose name contains the specified string.
--
-- * 'lCreationTimeAfter' - A filter that returns only algorithms created after the specified time (timestamp).
--
-- * 'lNextToken' - If the response to a previous @ListAlgorithms@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of algorithms, use the token in the next request.
--
-- * 'lSortOrder' - The sort order for the results. The default is @Ascending@ .
--
-- * 'lCreationTimeBefore' - A filter that returns only algorithms created before the specified time (timestamp).
--
-- * 'lMaxResults' - The maximum number of algorithms to return in the response.
--
-- * 'lSortBy' - The parameter by which to sort the results. The default is @CreationTime@ .
listAlgorithms
    :: ListAlgorithms
listAlgorithms
  = ListAlgorithms'{_lNameContains = Nothing,
                    _lCreationTimeAfter = Nothing, _lNextToken = Nothing,
                    _lSortOrder = Nothing,
                    _lCreationTimeBefore = Nothing,
                    _lMaxResults = Nothing, _lSortBy = Nothing}

-- | A string in the algorithm name. This filter returns only algorithms whose name contains the specified string.
lNameContains :: Lens' ListAlgorithms (Maybe Text)
lNameContains = lens _lNameContains (\ s a -> s{_lNameContains = a})

-- | A filter that returns only algorithms created after the specified time (timestamp).
lCreationTimeAfter :: Lens' ListAlgorithms (Maybe UTCTime)
lCreationTimeAfter = lens _lCreationTimeAfter (\ s a -> s{_lCreationTimeAfter = a}) . mapping _Time

-- | If the response to a previous @ListAlgorithms@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of algorithms, use the token in the next request.
lNextToken :: Lens' ListAlgorithms (Maybe Text)
lNextToken = lens _lNextToken (\ s a -> s{_lNextToken = a})

-- | The sort order for the results. The default is @Ascending@ .
lSortOrder :: Lens' ListAlgorithms (Maybe SortOrder)
lSortOrder = lens _lSortOrder (\ s a -> s{_lSortOrder = a})

-- | A filter that returns only algorithms created before the specified time (timestamp).
lCreationTimeBefore :: Lens' ListAlgorithms (Maybe UTCTime)
lCreationTimeBefore = lens _lCreationTimeBefore (\ s a -> s{_lCreationTimeBefore = a}) . mapping _Time

-- | The maximum number of algorithms to return in the response.
lMaxResults :: Lens' ListAlgorithms (Maybe Natural)
lMaxResults = lens _lMaxResults (\ s a -> s{_lMaxResults = a}) . mapping _Nat

-- | The parameter by which to sort the results. The default is @CreationTime@ .
lSortBy :: Lens' ListAlgorithms (Maybe AlgorithmSortBy)
lSortBy = lens _lSortBy (\ s a -> s{_lSortBy = a})

instance AWSPager ListAlgorithms where
        page rq rs
          | stop (rs ^. lrsNextToken) = Nothing
          | stop (rs ^. lrsAlgorithmSummaryList) = Nothing
          | otherwise =
            Just $ rq & lNextToken .~ rs ^. lrsNextToken

instance AWSRequest ListAlgorithms where
        type Rs ListAlgorithms = ListAlgorithmsResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 ListAlgorithmsResponse' <$>
                   (x .?> "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "AlgorithmSummaryList" .!@ mempty))

instance Hashable ListAlgorithms where

instance NFData ListAlgorithms where

instance ToHeaders ListAlgorithms where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.ListAlgorithms" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListAlgorithms where
        toJSON ListAlgorithms'{..}
          = object
              (catMaybes
                 [("NameContains" .=) <$> _lNameContains,
                  ("CreationTimeAfter" .=) <$> _lCreationTimeAfter,
                  ("NextToken" .=) <$> _lNextToken,
                  ("SortOrder" .=) <$> _lSortOrder,
                  ("CreationTimeBefore" .=) <$> _lCreationTimeBefore,
                  ("MaxResults" .=) <$> _lMaxResults,
                  ("SortBy" .=) <$> _lSortBy])

instance ToPath ListAlgorithms where
        toPath = const "/"

instance ToQuery ListAlgorithms where
        toQuery = const mempty

-- | /See:/ 'listAlgorithmsResponse' smart constructor.
data ListAlgorithmsResponse = ListAlgorithmsResponse'{_lrsNextToken
                                                      :: !(Maybe Text),
                                                      _lrsResponseStatus ::
                                                      !Int,
                                                      _lrsAlgorithmSummaryList
                                                      :: ![AlgorithmSummary]}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'ListAlgorithmsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrsNextToken' - If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of algorithms, use it in the subsequent request.
--
-- * 'lrsResponseStatus' - -- | The response status code.
--
-- * 'lrsAlgorithmSummaryList' - >An array of @AlgorithmSummary@ objects, each of which lists an algorithm.
listAlgorithmsResponse
    :: Int -- ^ 'lrsResponseStatus'
    -> ListAlgorithmsResponse
listAlgorithmsResponse pResponseStatus_
  = ListAlgorithmsResponse'{_lrsNextToken = Nothing,
                            _lrsResponseStatus = pResponseStatus_,
                            _lrsAlgorithmSummaryList = mempty}

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of algorithms, use it in the subsequent request.
lrsNextToken :: Lens' ListAlgorithmsResponse (Maybe Text)
lrsNextToken = lens _lrsNextToken (\ s a -> s{_lrsNextToken = a})

-- | -- | The response status code.
lrsResponseStatus :: Lens' ListAlgorithmsResponse Int
lrsResponseStatus = lens _lrsResponseStatus (\ s a -> s{_lrsResponseStatus = a})

-- | >An array of @AlgorithmSummary@ objects, each of which lists an algorithm.
lrsAlgorithmSummaryList :: Lens' ListAlgorithmsResponse [AlgorithmSummary]
lrsAlgorithmSummaryList = lens _lrsAlgorithmSummaryList (\ s a -> s{_lrsAlgorithmSummaryList = a}) . _Coerce

instance NFData ListAlgorithmsResponse where

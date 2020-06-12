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
-- Module      : Network.AWS.Connect.ListSecurityProfiles
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides summary information about the security profiles for the specified Amazon Connect instance.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListSecurityProfiles
    (
    -- * Creating a Request
      listSecurityProfiles
    , ListSecurityProfiles
    -- * Request Lenses
    , lspNextToken
    , lspMaxResults
    , lspInstanceId

    -- * Destructuring the Response
    , listSecurityProfilesResponse
    , ListSecurityProfilesResponse
    -- * Response Lenses
    , lsprsNextToken
    , lsprsSecurityProfileSummaryList
    , lsprsResponseStatus
    ) where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listSecurityProfiles' smart constructor.
data ListSecurityProfiles = ListSecurityProfiles'{_lspNextToken
                                                  :: !(Maybe Text),
                                                  _lspMaxResults ::
                                                  !(Maybe Nat),
                                                  _lspInstanceId :: !Text}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSecurityProfiles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lspNextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- * 'lspMaxResults' - The maximimum number of results to return per page.
--
-- * 'lspInstanceId' - The identifier of the Amazon Connect instance.
listSecurityProfiles
    :: Text -- ^ 'lspInstanceId'
    -> ListSecurityProfiles
listSecurityProfiles pInstanceId_
  = ListSecurityProfiles'{_lspNextToken = Nothing,
                          _lspMaxResults = Nothing,
                          _lspInstanceId = pInstanceId_}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
lspNextToken :: Lens' ListSecurityProfiles (Maybe Text)
lspNextToken = lens _lspNextToken (\ s a -> s{_lspNextToken = a})

-- | The maximimum number of results to return per page.
lspMaxResults :: Lens' ListSecurityProfiles (Maybe Natural)
lspMaxResults = lens _lspMaxResults (\ s a -> s{_lspMaxResults = a}) . mapping _Nat

-- | The identifier of the Amazon Connect instance.
lspInstanceId :: Lens' ListSecurityProfiles Text
lspInstanceId = lens _lspInstanceId (\ s a -> s{_lspInstanceId = a})

instance AWSPager ListSecurityProfiles where
        page rq rs
          | stop (rs ^. lsprsNextToken) = Nothing
          | stop (rs ^. lsprsSecurityProfileSummaryList) =
            Nothing
          | otherwise =
            Just $ rq & lspNextToken .~ rs ^. lsprsNextToken

instance AWSRequest ListSecurityProfiles where
        type Rs ListSecurityProfiles =
             ListSecurityProfilesResponse
        request = get connect
        response
          = receiveJSON
              (\ s h x ->
                 ListSecurityProfilesResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "SecurityProfileSummaryList" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListSecurityProfiles where

instance NFData ListSecurityProfiles where

instance ToHeaders ListSecurityProfiles where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListSecurityProfiles where
        toPath ListSecurityProfiles'{..}
          = mconcat
              ["/security-profiles-summary/", toBS _lspInstanceId]

instance ToQuery ListSecurityProfiles where
        toQuery ListSecurityProfiles'{..}
          = mconcat
              ["nextToken" =: _lspNextToken,
               "maxResults" =: _lspMaxResults]

-- | /See:/ 'listSecurityProfilesResponse' smart constructor.
data ListSecurityProfilesResponse = ListSecurityProfilesResponse'{_lsprsNextToken
                                                                  ::
                                                                  !(Maybe Text),
                                                                  _lsprsSecurityProfileSummaryList
                                                                  ::
                                                                  !(Maybe
                                                                      [SecurityProfileSummary]),
                                                                  _lsprsResponseStatus
                                                                  :: !Int}
                                      deriving (Eq, Read, Show, Data, Typeable,
                                                Generic)

-- | Creates a value of 'ListSecurityProfilesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsprsNextToken' - If there are additional results, this is the token for the next set of results.
--
-- * 'lsprsSecurityProfileSummaryList' - Information about the security profiles.
--
-- * 'lsprsResponseStatus' - -- | The response status code.
listSecurityProfilesResponse
    :: Int -- ^ 'lsprsResponseStatus'
    -> ListSecurityProfilesResponse
listSecurityProfilesResponse pResponseStatus_
  = ListSecurityProfilesResponse'{_lsprsNextToken =
                                    Nothing,
                                  _lsprsSecurityProfileSummaryList = Nothing,
                                  _lsprsResponseStatus = pResponseStatus_}

-- | If there are additional results, this is the token for the next set of results.
lsprsNextToken :: Lens' ListSecurityProfilesResponse (Maybe Text)
lsprsNextToken = lens _lsprsNextToken (\ s a -> s{_lsprsNextToken = a})

-- | Information about the security profiles.
lsprsSecurityProfileSummaryList :: Lens' ListSecurityProfilesResponse [SecurityProfileSummary]
lsprsSecurityProfileSummaryList = lens _lsprsSecurityProfileSummaryList (\ s a -> s{_lsprsSecurityProfileSummaryList = a}) . _Default . _Coerce

-- | -- | The response status code.
lsprsResponseStatus :: Lens' ListSecurityProfilesResponse Int
lsprsResponseStatus = lens _lsprsResponseStatus (\ s a -> s{_lsprsResponseStatus = a})

instance NFData ListSecurityProfilesResponse where

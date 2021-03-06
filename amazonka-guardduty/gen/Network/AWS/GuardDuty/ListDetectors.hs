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
-- Module      : Network.AWS.GuardDuty.ListDetectors
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists detectorIds of all the existing Amazon GuardDuty detector resources.
--
--
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListDetectors
    (
    -- * Creating a Request
      listDetectors
    , ListDetectors
    -- * Request Lenses
    , ldNextToken
    , ldMaxResults

    -- * Destructuring the Response
    , listDetectorsResponse
    , ListDetectorsResponse
    -- * Response Lenses
    , ldrsNextToken
    , ldrsResponseStatus
    , ldrsDetectorIds
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDetectors' smart constructor.
data ListDetectors = ListDetectors'{_ldNextToken ::
                                    !(Maybe Text),
                                    _ldMaxResults :: !(Maybe Nat)}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDetectors' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldNextToken' - You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
--
-- * 'ldMaxResults' - You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 50. The maximum value is 50.
listDetectors
    :: ListDetectors
listDetectors
  = ListDetectors'{_ldNextToken = Nothing,
                   _ldMaxResults = Nothing}

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
ldNextToken :: Lens' ListDetectors (Maybe Text)
ldNextToken = lens _ldNextToken (\ s a -> s{_ldNextToken = a})

-- | You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 50. The maximum value is 50.
ldMaxResults :: Lens' ListDetectors (Maybe Natural)
ldMaxResults = lens _ldMaxResults (\ s a -> s{_ldMaxResults = a}) . mapping _Nat

instance AWSPager ListDetectors where
        page rq rs
          | stop (rs ^. ldrsNextToken) = Nothing
          | stop (rs ^. ldrsDetectorIds) = Nothing
          | otherwise =
            Just $ rq & ldNextToken .~ rs ^. ldrsNextToken

instance AWSRequest ListDetectors where
        type Rs ListDetectors = ListDetectorsResponse
        request = get guardDuty
        response
          = receiveJSON
              (\ s h x ->
                 ListDetectorsResponse' <$>
                   (x .?> "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "detectorIds" .!@ mempty))

instance Hashable ListDetectors where

instance NFData ListDetectors where

instance ToHeaders ListDetectors where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListDetectors where
        toPath = const "/detector"

instance ToQuery ListDetectors where
        toQuery ListDetectors'{..}
          = mconcat
              ["nextToken" =: _ldNextToken,
               "maxResults" =: _ldMaxResults]

-- | /See:/ 'listDetectorsResponse' smart constructor.
data ListDetectorsResponse = ListDetectorsResponse'{_ldrsNextToken
                                                    :: !(Maybe Text),
                                                    _ldrsResponseStatus :: !Int,
                                                    _ldrsDetectorIds :: ![Text]}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'ListDetectorsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldrsNextToken' - The pagination parameter to be used on the next list operation to retrieve more items.
--
-- * 'ldrsResponseStatus' - -- | The response status code.
--
-- * 'ldrsDetectorIds' - A list of detector IDs.
listDetectorsResponse
    :: Int -- ^ 'ldrsResponseStatus'
    -> ListDetectorsResponse
listDetectorsResponse pResponseStatus_
  = ListDetectorsResponse'{_ldrsNextToken = Nothing,
                           _ldrsResponseStatus = pResponseStatus_,
                           _ldrsDetectorIds = mempty}

-- | The pagination parameter to be used on the next list operation to retrieve more items.
ldrsNextToken :: Lens' ListDetectorsResponse (Maybe Text)
ldrsNextToken = lens _ldrsNextToken (\ s a -> s{_ldrsNextToken = a})

-- | -- | The response status code.
ldrsResponseStatus :: Lens' ListDetectorsResponse Int
ldrsResponseStatus = lens _ldrsResponseStatus (\ s a -> s{_ldrsResponseStatus = a})

-- | A list of detector IDs.
ldrsDetectorIds :: Lens' ListDetectorsResponse [Text]
ldrsDetectorIds = lens _ldrsDetectorIds (\ s a -> s{_ldrsDetectorIds = a}) . _Coerce

instance NFData ListDetectorsResponse where

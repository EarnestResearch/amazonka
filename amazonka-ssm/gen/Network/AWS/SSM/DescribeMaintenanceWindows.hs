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
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindows
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the maintenance windows in an AWS account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeMaintenanceWindows
    (
    -- * Creating a Request
      describeMaintenanceWindows
    , DescribeMaintenanceWindows
    -- * Request Lenses
    , dmwFilters
    , dmwNextToken
    , dmwMaxResults

    -- * Destructuring the Response
    , describeMaintenanceWindowsResponse
    , DescribeMaintenanceWindowsResponse
    -- * Response Lenses
    , dscrbmntnncwndwsrsWindowIdentities
    , dscrbmntnncwndwsrsNextToken
    , dscrbmntnncwndwsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'describeMaintenanceWindows' smart constructor.
data DescribeMaintenanceWindows = DescribeMaintenanceWindows'{_dmwFilters
                                                              ::
                                                              !(Maybe
                                                                  [MaintenanceWindowFilter]),
                                                              _dmwNextToken ::
                                                              !(Maybe Text),
                                                              _dmwMaxResults ::
                                                              !(Maybe Nat)}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'DescribeMaintenanceWindows' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmwFilters' - Optional filters used to narrow down the scope of the returned maintenance windows. Supported filter keys are __Name__ and __Enabled__ .
--
-- * 'dmwNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dmwMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
describeMaintenanceWindows
    :: DescribeMaintenanceWindows
describeMaintenanceWindows
  = DescribeMaintenanceWindows'{_dmwFilters = Nothing,
                                _dmwNextToken = Nothing,
                                _dmwMaxResults = Nothing}

-- | Optional filters used to narrow down the scope of the returned maintenance windows. Supported filter keys are __Name__ and __Enabled__ .
dmwFilters :: Lens' DescribeMaintenanceWindows [MaintenanceWindowFilter]
dmwFilters = lens _dmwFilters (\ s a -> s{_dmwFilters = a}) . _Default . _Coerce

-- | The token for the next set of items to return. (You received this token from a previous call.)
dmwNextToken :: Lens' DescribeMaintenanceWindows (Maybe Text)
dmwNextToken = lens _dmwNextToken (\ s a -> s{_dmwNextToken = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
dmwMaxResults :: Lens' DescribeMaintenanceWindows (Maybe Natural)
dmwMaxResults = lens _dmwMaxResults (\ s a -> s{_dmwMaxResults = a}) . mapping _Nat

instance AWSPager DescribeMaintenanceWindows where
        page rq rs
          | stop (rs ^. dscrbmntnncwndwsrsNextToken) = Nothing
          | stop (rs ^. dscrbmntnncwndwsrsWindowIdentities) =
            Nothing
          | otherwise =
            Just $ rq &
              dmwNextToken .~ rs ^. dscrbmntnncwndwsrsNextToken

instance AWSRequest DescribeMaintenanceWindows where
        type Rs DescribeMaintenanceWindows =
             DescribeMaintenanceWindowsResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DescribeMaintenanceWindowsResponse' <$>
                   (x .?> "WindowIdentities" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeMaintenanceWindows where

instance NFData DescribeMaintenanceWindows where

instance ToHeaders DescribeMaintenanceWindows where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DescribeMaintenanceWindows" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeMaintenanceWindows where
        toJSON DescribeMaintenanceWindows'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _dmwFilters,
                  ("NextToken" .=) <$> _dmwNextToken,
                  ("MaxResults" .=) <$> _dmwMaxResults])

instance ToPath DescribeMaintenanceWindows where
        toPath = const "/"

instance ToQuery DescribeMaintenanceWindows where
        toQuery = const mempty

-- | /See:/ 'describeMaintenanceWindowsResponse' smart constructor.
data DescribeMaintenanceWindowsResponse = DescribeMaintenanceWindowsResponse'{_dscrbmntnncwndwsrsWindowIdentities
                                                                              ::
                                                                              !(Maybe
                                                                                  [MaintenanceWindowIdentity]),
                                                                              _dscrbmntnncwndwsrsNextToken
                                                                              ::
                                                                              !(Maybe
                                                                                  Text),
                                                                              _dscrbmntnncwndwsrsResponseStatus
                                                                              ::
                                                                              !Int}
                                            deriving (Eq, Show, Data, Typeable,
                                                      Generic)

-- | Creates a value of 'DescribeMaintenanceWindowsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrbmntnncwndwsrsWindowIdentities' - Information about the maintenance windows.
--
-- * 'dscrbmntnncwndwsrsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'dscrbmntnncwndwsrsResponseStatus' - -- | The response status code.
describeMaintenanceWindowsResponse
    :: Int -- ^ 'dscrbmntnncwndwsrsResponseStatus'
    -> DescribeMaintenanceWindowsResponse
describeMaintenanceWindowsResponse pResponseStatus_
  = DescribeMaintenanceWindowsResponse'{_dscrbmntnncwndwsrsWindowIdentities
                                          = Nothing,
                                        _dscrbmntnncwndwsrsNextToken = Nothing,
                                        _dscrbmntnncwndwsrsResponseStatus =
                                          pResponseStatus_}

-- | Information about the maintenance windows.
dscrbmntnncwndwsrsWindowIdentities :: Lens' DescribeMaintenanceWindowsResponse [MaintenanceWindowIdentity]
dscrbmntnncwndwsrsWindowIdentities = lens _dscrbmntnncwndwsrsWindowIdentities (\ s a -> s{_dscrbmntnncwndwsrsWindowIdentities = a}) . _Default . _Coerce

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
dscrbmntnncwndwsrsNextToken :: Lens' DescribeMaintenanceWindowsResponse (Maybe Text)
dscrbmntnncwndwsrsNextToken = lens _dscrbmntnncwndwsrsNextToken (\ s a -> s{_dscrbmntnncwndwsrsNextToken = a})

-- | -- | The response status code.
dscrbmntnncwndwsrsResponseStatus :: Lens' DescribeMaintenanceWindowsResponse Int
dscrbmntnncwndwsrsResponseStatus = lens _dscrbmntnncwndwsrsResponseStatus (\ s a -> s{_dscrbmntnncwndwsrsResponseStatus = a})

instance NFData DescribeMaintenanceWindowsResponse
         where

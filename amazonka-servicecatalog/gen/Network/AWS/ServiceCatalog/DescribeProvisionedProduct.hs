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
-- Module      : Network.AWS.ServiceCatalog.DescribeProvisionedProduct
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified provisioned product.
--
--
module Network.AWS.ServiceCatalog.DescribeProvisionedProduct
    (
    -- * Creating a Request
      describeProvisionedProduct
    , DescribeProvisionedProduct
    -- * Request Lenses
    , dscrbprvsndprdctAcceptLanguage
    , dscrbprvsndprdctId

    -- * Destructuring the Response
    , describeProvisionedProductResponse
    , DescribeProvisionedProductResponse
    -- * Response Lenses
    , drsProvisionedProductDetail
    , drsCloudWatchDashboards
    , drsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'describeProvisionedProduct' smart constructor.
data DescribeProvisionedProduct = DescribeProvisionedProduct'{_dscrbprvsndprdctAcceptLanguage
                                                              :: !(Maybe Text),
                                                              _dscrbprvsndprdctId
                                                              :: !Text}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'DescribeProvisionedProduct' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrbprvsndprdctAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'dscrbprvsndprdctId' - The provisioned product identifier.
describeProvisionedProduct
    :: Text -- ^ 'dscrbprvsndprdctId'
    -> DescribeProvisionedProduct
describeProvisionedProduct pId_
  = DescribeProvisionedProduct'{_dscrbprvsndprdctAcceptLanguage
                                  = Nothing,
                                _dscrbprvsndprdctId = pId_}

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
dscrbprvsndprdctAcceptLanguage :: Lens' DescribeProvisionedProduct (Maybe Text)
dscrbprvsndprdctAcceptLanguage = lens _dscrbprvsndprdctAcceptLanguage (\ s a -> s{_dscrbprvsndprdctAcceptLanguage = a})

-- | The provisioned product identifier.
dscrbprvsndprdctId :: Lens' DescribeProvisionedProduct Text
dscrbprvsndprdctId = lens _dscrbprvsndprdctId (\ s a -> s{_dscrbprvsndprdctId = a})

instance AWSRequest DescribeProvisionedProduct where
        type Rs DescribeProvisionedProduct =
             DescribeProvisionedProductResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 DescribeProvisionedProductResponse' <$>
                   (x .?> "ProvisionedProductDetail") <*>
                     (x .?> "CloudWatchDashboards" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeProvisionedProduct where

instance NFData DescribeProvisionedProduct where

instance ToHeaders DescribeProvisionedProduct where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DescribeProvisionedProduct"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeProvisionedProduct where
        toJSON DescribeProvisionedProduct'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$>
                    _dscrbprvsndprdctAcceptLanguage,
                  Just ("Id" .= _dscrbprvsndprdctId)])

instance ToPath DescribeProvisionedProduct where
        toPath = const "/"

instance ToQuery DescribeProvisionedProduct where
        toQuery = const mempty

-- | /See:/ 'describeProvisionedProductResponse' smart constructor.
data DescribeProvisionedProductResponse = DescribeProvisionedProductResponse'{_drsProvisionedProductDetail
                                                                              ::
                                                                              !(Maybe
                                                                                  ProvisionedProductDetail),
                                                                              _drsCloudWatchDashboards
                                                                              ::
                                                                              !(Maybe
                                                                                  [CloudWatchDashboard]),
                                                                              _drsResponseStatus
                                                                              ::
                                                                              !Int}
                                            deriving (Eq, Read, Show, Data,
                                                      Typeable, Generic)

-- | Creates a value of 'DescribeProvisionedProductResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsProvisionedProductDetail' - Information about the provisioned product.
--
-- * 'drsCloudWatchDashboards' - Any CloudWatch dashboards that were created when provisioning the product.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeProvisionedProductResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeProvisionedProductResponse
describeProvisionedProductResponse pResponseStatus_
  = DescribeProvisionedProductResponse'{_drsProvisionedProductDetail
                                          = Nothing,
                                        _drsCloudWatchDashboards = Nothing,
                                        _drsResponseStatus = pResponseStatus_}

-- | Information about the provisioned product.
drsProvisionedProductDetail :: Lens' DescribeProvisionedProductResponse (Maybe ProvisionedProductDetail)
drsProvisionedProductDetail = lens _drsProvisionedProductDetail (\ s a -> s{_drsProvisionedProductDetail = a})

-- | Any CloudWatch dashboards that were created when provisioning the product.
drsCloudWatchDashboards :: Lens' DescribeProvisionedProductResponse [CloudWatchDashboard]
drsCloudWatchDashboards = lens _drsCloudWatchDashboards (\ s a -> s{_drsCloudWatchDashboards = a}) . _Default . _Coerce

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeProvisionedProductResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DescribeProvisionedProductResponse
         where

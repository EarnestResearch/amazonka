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
-- Module      : Network.AWS.ServiceCatalog.DeletePortfolio
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified portfolio.
--
--
-- You cannot delete a portfolio if it was shared with you or if it has associated products, users, constraints, or shared accounts.
--
module Network.AWS.ServiceCatalog.DeletePortfolio
    (
    -- * Creating a Request
      deletePortfolio
    , DeletePortfolio
    -- * Request Lenses
    , dltprtflAcceptLanguage
    , dltprtflId

    -- * Destructuring the Response
    , deletePortfolioResponse
    , DeletePortfolioResponse
    -- * Response Lenses
    , delrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types
import Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'deletePortfolio' smart constructor.
data DeletePortfolio = DeletePortfolio'{_dltprtflAcceptLanguage
                                        :: !(Maybe Text),
                                        _dltprtflId :: !Text}
                         deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeletePortfolio' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltprtflAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'dltprtflId' - The portfolio identifier.
deletePortfolio
    :: Text -- ^ 'dltprtflId'
    -> DeletePortfolio
deletePortfolio pId_
  = DeletePortfolio'{_dltprtflAcceptLanguage = Nothing,
                     _dltprtflId = pId_}

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
dltprtflAcceptLanguage :: Lens' DeletePortfolio (Maybe Text)
dltprtflAcceptLanguage = lens _dltprtflAcceptLanguage (\ s a -> s{_dltprtflAcceptLanguage = a})

-- | The portfolio identifier.
dltprtflId :: Lens' DeletePortfolio Text
dltprtflId = lens _dltprtflId (\ s a -> s{_dltprtflId = a})

instance AWSRequest DeletePortfolio where
        type Rs DeletePortfolio = DeletePortfolioResponse
        request = postJSON serviceCatalog
        response
          = receiveEmpty
              (\ s h x ->
                 DeletePortfolioResponse' <$> (pure (fromEnum s)))

instance Hashable DeletePortfolio where

instance NFData DeletePortfolio where

instance ToHeaders DeletePortfolio where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DeletePortfolio" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeletePortfolio where
        toJSON DeletePortfolio'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _dltprtflAcceptLanguage,
                  Just ("Id" .= _dltprtflId)])

instance ToPath DeletePortfolio where
        toPath = const "/"

instance ToQuery DeletePortfolio where
        toQuery = const mempty

-- | /See:/ 'deletePortfolioResponse' smart constructor.
newtype DeletePortfolioResponse = DeletePortfolioResponse'{_delrsResponseStatus
                                                           :: Int}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'DeletePortfolioResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsResponseStatus' - -- | The response status code.
deletePortfolioResponse
    :: Int -- ^ 'delrsResponseStatus'
    -> DeletePortfolioResponse
deletePortfolioResponse pResponseStatus_
  = DeletePortfolioResponse'{_delrsResponseStatus =
                               pResponseStatus_}

-- | -- | The response status code.
delrsResponseStatus :: Lens' DeletePortfolioResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\ s a -> s{_delrsResponseStatus = a})

instance NFData DeletePortfolioResponse where

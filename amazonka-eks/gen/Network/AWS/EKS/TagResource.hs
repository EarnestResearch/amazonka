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
-- Module      : Network.AWS.EKS.TagResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified tags to a resource with the specified @resourceArn@ . If existing tags on a resource are not specified in the request parameters, they are not changed. When a resource is deleted, the tags associated with that resource are deleted as well. Tags that you create for Amazon EKS resources do not propagate to any other resources associated with the cluster. For example, if you tag a cluster with this operation, that tag does not automatically propagate to the subnets and worker nodes associated with the cluster.
--
--
module Network.AWS.EKS.TagResource
    (
    -- * Creating a Request
      tagResource
    , TagResource
    -- * Request Lenses
    , trResourceARN
    , trTags

    -- * Destructuring the Response
    , tagResourceResponse
    , TagResourceResponse
    -- * Response Lenses
    , trrsResponseStatus
    ) where

import Network.AWS.EKS.Types
import Network.AWS.EKS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'tagResource' smart constructor.
data TagResource = TagResource'{_trResourceARN ::
                                !Text,
                                _trTags :: !(Map Text Text)}
                     deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trResourceARN' - The Amazon Resource Name (ARN) of the resource to which to add tags. Currently, the supported resources are Amazon EKS clusters and managed node groups.
--
-- * 'trTags' - The tags to add to the resource. A tag is an array of key-value pairs.
tagResource
    :: Text -- ^ 'trResourceARN'
    -> TagResource
tagResource pResourceARN_
  = TagResource'{_trResourceARN = pResourceARN_,
                 _trTags = mempty}

-- | The Amazon Resource Name (ARN) of the resource to which to add tags. Currently, the supported resources are Amazon EKS clusters and managed node groups.
trResourceARN :: Lens' TagResource Text
trResourceARN = lens _trResourceARN (\ s a -> s{_trResourceARN = a})

-- | The tags to add to the resource. A tag is an array of key-value pairs.
trTags :: Lens' TagResource (HashMap Text Text)
trTags = lens _trTags (\ s a -> s{_trTags = a}) . _Map

instance AWSRequest TagResource where
        type Rs TagResource = TagResourceResponse
        request = postJSON eks
        response
          = receiveEmpty
              (\ s h x ->
                 TagResourceResponse' <$> (pure (fromEnum s)))

instance Hashable TagResource where

instance NFData TagResource where

instance ToHeaders TagResource where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON TagResource where
        toJSON TagResource'{..}
          = object (catMaybes [Just ("tags" .= _trTags)])

instance ToPath TagResource where
        toPath TagResource'{..}
          = mconcat ["/tags/", toBS _trResourceARN]

instance ToQuery TagResource where
        toQuery = const mempty

-- | /See:/ 'tagResourceResponse' smart constructor.
newtype TagResourceResponse = TagResourceResponse'{_trrsResponseStatus
                                                   :: Int}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'TagResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trrsResponseStatus' - -- | The response status code.
tagResourceResponse
    :: Int -- ^ 'trrsResponseStatus'
    -> TagResourceResponse
tagResourceResponse pResponseStatus_
  = TagResourceResponse'{_trrsResponseStatus =
                           pResponseStatus_}

-- | -- | The response status code.
trrsResponseStatus :: Lens' TagResourceResponse Int
trrsResponseStatus = lens _trrsResponseStatus (\ s a -> s{_trrsResponseStatus = a})

instance NFData TagResourceResponse where

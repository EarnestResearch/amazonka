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
-- Module      : Network.AWS.S3.GetBucketLocation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the region the bucket resides in.
module Network.AWS.S3.GetBucketLocation
    (
    -- * Creating a Request
      getBucketLocation
    , GetBucketLocation
    -- * Request Lenses
    , gblBucket

    -- * Destructuring the Response
    , getBucketLocationResponse
    , GetBucketLocationResponse
    -- * Response Lenses
    , gtbcktlctnrsResponseStatus
    , gtbcktlctnrsLocationConstraint
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'getBucketLocation' smart constructor.
newtype GetBucketLocation = GetBucketLocation'{_gblBucket
                                               :: BucketName}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gblBucket' - Undocumented member.
getBucketLocation
    :: BucketName -- ^ 'gblBucket'
    -> GetBucketLocation
getBucketLocation pBucket_
  = GetBucketLocation'{_gblBucket = pBucket_}

-- | Undocumented member.
gblBucket :: Lens' GetBucketLocation BucketName
gblBucket = lens _gblBucket (\ s a -> s{_gblBucket = a})

instance AWSRequest GetBucketLocation where
        type Rs GetBucketLocation = GetBucketLocationResponse
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 GetBucketLocationResponse' <$>
                   (pure (fromEnum s)) <*> (parseXML x))

instance Hashable GetBucketLocation where

instance NFData GetBucketLocation where

instance ToHeaders GetBucketLocation where
        toHeaders = const mempty

instance ToPath GetBucketLocation where
        toPath GetBucketLocation'{..}
          = mconcat ["/", toBS _gblBucket]

instance ToQuery GetBucketLocation where
        toQuery = const (mconcat ["location"])

-- | /See:/ 'getBucketLocationResponse' smart constructor.
data GetBucketLocationResponse = GetBucketLocationResponse'{_gtbcktlctnrsResponseStatus
                                                            :: !Int,
                                                            _gtbcktlctnrsLocationConstraint
                                                            ::
                                                            !LocationConstraint}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'GetBucketLocationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtbcktlctnrsResponseStatus' - -- | The response status code.
--
-- * 'gtbcktlctnrsLocationConstraint' - Undocumented member.
getBucketLocationResponse
    :: Int -- ^ 'gtbcktlctnrsResponseStatus'
    -> LocationConstraint -- ^ 'gtbcktlctnrsLocationConstraint'
    -> GetBucketLocationResponse
getBucketLocationResponse pResponseStatus_
  pLocationConstraint_
  = GetBucketLocationResponse'{_gtbcktlctnrsResponseStatus
                                 = pResponseStatus_,
                               _gtbcktlctnrsLocationConstraint =
                                 pLocationConstraint_}

-- | -- | The response status code.
gtbcktlctnrsResponseStatus :: Lens' GetBucketLocationResponse Int
gtbcktlctnrsResponseStatus = lens _gtbcktlctnrsResponseStatus (\ s a -> s{_gtbcktlctnrsResponseStatus = a})

-- | Undocumented member.
gtbcktlctnrsLocationConstraint :: Lens' GetBucketLocationResponse LocationConstraint
gtbcktlctnrsLocationConstraint = lens _gtbcktlctnrsLocationConstraint (\ s a -> s{_gtbcktlctnrsLocationConstraint = a})

instance NFData GetBucketLocationResponse where

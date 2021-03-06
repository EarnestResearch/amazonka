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
-- Module      : Network.AWS.S3.GetBucketNotificationConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the notification configuration of a bucket.
--
--
-- If notifications are not enabled on the bucket, the operation returns an empty @NotificationConfiguration@ element.
--
-- By default, you must be the bucket owner to read the notification configuration of a bucket. However, the bucket owner can use a bucket policy to grant permission to other users to read this configuration with the @s3:GetBucketNotification@ permission.
--
-- For more information about setting and reading the notification configuration on a bucket, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Setting Up Notification of Bucket Events> . For more information about bucket policies, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-iam-policies.html Using Bucket Policies> .
--
-- The following operation is related to @GetBucketNotification@ :
--
--     * 'PutBucketNotification' 
--
--
--
module Network.AWS.S3.GetBucketNotificationConfiguration
    (
    -- * Creating a Request
      getBucketNotificationConfiguration
    , GetBucketNotificationConfiguration
    -- * Request Lenses
    , gbncBucket

    -- * Destructuring the Response
    , notificationConfiguration
    , NotificationConfiguration
    -- * Response Lenses
    , ncQueueConfigurations
    , ncTopicConfigurations
    , ncLambdaFunctionConfigurations
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'getBucketNotificationConfiguration' smart constructor.
newtype GetBucketNotificationConfiguration = GetBucketNotificationConfiguration'{_gbncBucket
                                                                                 ::
                                                                                 BucketName}
                                               deriving (Eq, Read, Show, Data,
                                                         Typeable, Generic)

-- | Creates a value of 'GetBucketNotificationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbncBucket' - Name of the bucket for which to get the notification configuration
getBucketNotificationConfiguration
    :: BucketName -- ^ 'gbncBucket'
    -> GetBucketNotificationConfiguration
getBucketNotificationConfiguration pBucket_
  = GetBucketNotificationConfiguration'{_gbncBucket =
                                          pBucket_}

-- | Name of the bucket for which to get the notification configuration
gbncBucket :: Lens' GetBucketNotificationConfiguration BucketName
gbncBucket = lens _gbncBucket (\ s a -> s{_gbncBucket = a})

instance AWSRequest
           GetBucketNotificationConfiguration
         where
        type Rs GetBucketNotificationConfiguration =
             NotificationConfiguration
        request = get s3
        response = receiveXML (\ s h x -> parseXML x)

instance Hashable GetBucketNotificationConfiguration
         where

instance NFData GetBucketNotificationConfiguration
         where

instance ToHeaders GetBucketNotificationConfiguration
         where
        toHeaders = const mempty

instance ToPath GetBucketNotificationConfiguration
         where
        toPath GetBucketNotificationConfiguration'{..}
          = mconcat ["/", toBS _gbncBucket]

instance ToQuery GetBucketNotificationConfiguration
         where
        toQuery = const (mconcat ["notification"])

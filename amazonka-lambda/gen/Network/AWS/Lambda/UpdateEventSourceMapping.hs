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
-- Module      : Network.AWS.Lambda.UpdateEventSourceMapping
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an event source mapping. You can change the function that AWS Lambda invokes, or pause invocation and resume later from the same location.
--
--
-- The following error handling options are only available for stream sources (DynamoDB and Kinesis):
--
--     * @BisectBatchOnFunctionError@ - If the function returns an error, split the batch in two and retry.
--
--     * @DestinationConfig@ - Send discarded records to an Amazon SQS queue or Amazon SNS topic.
--
--     * @MaximumRecordAgeInSeconds@ - Discard records older than the specified age.
--
--     * @MaximumRetryAttempts@ - Discard records after the specified number of retries.
--
--     * @ParallelizationFactor@ - Process multiple batches from each shard concurrently.
--
--
--
module Network.AWS.Lambda.UpdateEventSourceMapping
    (
    -- * Creating a Request
      updateEventSourceMapping
    , UpdateEventSourceMapping
    -- * Request Lenses
    , uesmEnabled
    , uesmBisectBatchOnFunctionError
    , uesmParallelizationFactor
    , uesmMaximumRetryAttempts
    , uesmBatchSize
    , uesmMaximumBatchingWindowInSeconds
    , uesmMaximumRecordAgeInSeconds
    , uesmFunctionName
    , uesmDestinationConfig
    , uesmUUId

    -- * Destructuring the Response
    , eventSourceMappingConfiguration
    , EventSourceMappingConfiguration
    -- * Response Lenses
    , esmcEventSourceARN
    , esmcState
    , esmcFunctionARN
    , esmcBisectBatchOnFunctionError
    , esmcUUId
    , esmcParallelizationFactor
    , esmcLastProcessingResult
    , esmcMaximumRetryAttempts
    , esmcBatchSize
    , esmcStateTransitionReason
    , esmcMaximumBatchingWindowInSeconds
    , esmcMaximumRecordAgeInSeconds
    , esmcLastModified
    , esmcDestinationConfig
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateEventSourceMapping' smart constructor.
data UpdateEventSourceMapping = UpdateEventSourceMapping'{_uesmEnabled
                                                          :: !(Maybe Bool),
                                                          _uesmBisectBatchOnFunctionError
                                                          :: !(Maybe Bool),
                                                          _uesmParallelizationFactor
                                                          :: !(Maybe Nat),
                                                          _uesmMaximumRetryAttempts
                                                          :: !(Maybe Nat),
                                                          _uesmBatchSize ::
                                                          !(Maybe Nat),
                                                          _uesmMaximumBatchingWindowInSeconds
                                                          :: !(Maybe Nat),
                                                          _uesmMaximumRecordAgeInSeconds
                                                          :: !(Maybe Nat),
                                                          _uesmFunctionName ::
                                                          !(Maybe Text),
                                                          _uesmDestinationConfig
                                                          ::
                                                          !(Maybe
                                                              DestinationConfig),
                                                          _uesmUUId :: !Text}
                                  deriving (Eq, Read, Show, Data, Typeable,
                                            Generic)

-- | Creates a value of 'UpdateEventSourceMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uesmEnabled' - Disables the event source mapping to pause polling and invocation.
--
-- * 'uesmBisectBatchOnFunctionError' - (Streams) If the function returns an error, split the batch in two and retry.
--
-- * 'uesmParallelizationFactor' - (Streams) The number of batches to process from each shard concurrently.
--
-- * 'uesmMaximumRetryAttempts' - (Streams) The maximum number of times to retry when the function returns an error.
--
-- * 'uesmBatchSize' - The maximum number of items to retrieve in a single batch.     * __Amazon Kinesis__ - Default 100. Max 10,000.     * __Amazon DynamoDB Streams__ - Default 100. Max 1,000.     * __Amazon Simple Queue Service__ - Default 10. Max 10.
--
-- * 'uesmMaximumBatchingWindowInSeconds' - (Streams) The maximum amount of time to gather records before invoking the function, in seconds.
--
-- * 'uesmMaximumRecordAgeInSeconds' - (Streams) The maximum age of a record that Lambda sends to a function for processing.
--
-- * 'uesmFunctionName' - The name of the Lambda function. __Name formats__      * __Function name__ - @MyFunction@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .     * __Version or Alias ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction:PROD@ .     * __Partial ARN__ - @123456789012:function:MyFunction@ . The length constraint applies only to the full ARN. If you specify only the function name, it's limited to 64 characters in length.
--
-- * 'uesmDestinationConfig' - (Streams) An Amazon SQS queue or Amazon SNS topic destination for discarded records.
--
-- * 'uesmUUId' - The identifier of the event source mapping.
updateEventSourceMapping
    :: Text -- ^ 'uesmUUId'
    -> UpdateEventSourceMapping
updateEventSourceMapping pUUId_
  = UpdateEventSourceMapping'{_uesmEnabled = Nothing,
                              _uesmBisectBatchOnFunctionError = Nothing,
                              _uesmParallelizationFactor = Nothing,
                              _uesmMaximumRetryAttempts = Nothing,
                              _uesmBatchSize = Nothing,
                              _uesmMaximumBatchingWindowInSeconds = Nothing,
                              _uesmMaximumRecordAgeInSeconds = Nothing,
                              _uesmFunctionName = Nothing,
                              _uesmDestinationConfig = Nothing,
                              _uesmUUId = pUUId_}

-- | Disables the event source mapping to pause polling and invocation.
uesmEnabled :: Lens' UpdateEventSourceMapping (Maybe Bool)
uesmEnabled = lens _uesmEnabled (\ s a -> s{_uesmEnabled = a})

-- | (Streams) If the function returns an error, split the batch in two and retry.
uesmBisectBatchOnFunctionError :: Lens' UpdateEventSourceMapping (Maybe Bool)
uesmBisectBatchOnFunctionError = lens _uesmBisectBatchOnFunctionError (\ s a -> s{_uesmBisectBatchOnFunctionError = a})

-- | (Streams) The number of batches to process from each shard concurrently.
uesmParallelizationFactor :: Lens' UpdateEventSourceMapping (Maybe Natural)
uesmParallelizationFactor = lens _uesmParallelizationFactor (\ s a -> s{_uesmParallelizationFactor = a}) . mapping _Nat

-- | (Streams) The maximum number of times to retry when the function returns an error.
uesmMaximumRetryAttempts :: Lens' UpdateEventSourceMapping (Maybe Natural)
uesmMaximumRetryAttempts = lens _uesmMaximumRetryAttempts (\ s a -> s{_uesmMaximumRetryAttempts = a}) . mapping _Nat

-- | The maximum number of items to retrieve in a single batch.     * __Amazon Kinesis__ - Default 100. Max 10,000.     * __Amazon DynamoDB Streams__ - Default 100. Max 1,000.     * __Amazon Simple Queue Service__ - Default 10. Max 10.
uesmBatchSize :: Lens' UpdateEventSourceMapping (Maybe Natural)
uesmBatchSize = lens _uesmBatchSize (\ s a -> s{_uesmBatchSize = a}) . mapping _Nat

-- | (Streams) The maximum amount of time to gather records before invoking the function, in seconds.
uesmMaximumBatchingWindowInSeconds :: Lens' UpdateEventSourceMapping (Maybe Natural)
uesmMaximumBatchingWindowInSeconds = lens _uesmMaximumBatchingWindowInSeconds (\ s a -> s{_uesmMaximumBatchingWindowInSeconds = a}) . mapping _Nat

-- | (Streams) The maximum age of a record that Lambda sends to a function for processing.
uesmMaximumRecordAgeInSeconds :: Lens' UpdateEventSourceMapping (Maybe Natural)
uesmMaximumRecordAgeInSeconds = lens _uesmMaximumRecordAgeInSeconds (\ s a -> s{_uesmMaximumRecordAgeInSeconds = a}) . mapping _Nat

-- | The name of the Lambda function. __Name formats__      * __Function name__ - @MyFunction@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .     * __Version or Alias ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction:PROD@ .     * __Partial ARN__ - @123456789012:function:MyFunction@ . The length constraint applies only to the full ARN. If you specify only the function name, it's limited to 64 characters in length.
uesmFunctionName :: Lens' UpdateEventSourceMapping (Maybe Text)
uesmFunctionName = lens _uesmFunctionName (\ s a -> s{_uesmFunctionName = a})

-- | (Streams) An Amazon SQS queue or Amazon SNS topic destination for discarded records.
uesmDestinationConfig :: Lens' UpdateEventSourceMapping (Maybe DestinationConfig)
uesmDestinationConfig = lens _uesmDestinationConfig (\ s a -> s{_uesmDestinationConfig = a})

-- | The identifier of the event source mapping.
uesmUUId :: Lens' UpdateEventSourceMapping Text
uesmUUId = lens _uesmUUId (\ s a -> s{_uesmUUId = a})

instance AWSRequest UpdateEventSourceMapping where
        type Rs UpdateEventSourceMapping =
             EventSourceMappingConfiguration
        request = putJSON lambda
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateEventSourceMapping where

instance NFData UpdateEventSourceMapping where

instance ToHeaders UpdateEventSourceMapping where
        toHeaders = const mempty

instance ToJSON UpdateEventSourceMapping where
        toJSON UpdateEventSourceMapping'{..}
          = object
              (catMaybes
                 [("Enabled" .=) <$> _uesmEnabled,
                  ("BisectBatchOnFunctionError" .=) <$>
                    _uesmBisectBatchOnFunctionError,
                  ("ParallelizationFactor" .=) <$>
                    _uesmParallelizationFactor,
                  ("MaximumRetryAttempts" .=) <$>
                    _uesmMaximumRetryAttempts,
                  ("BatchSize" .=) <$> _uesmBatchSize,
                  ("MaximumBatchingWindowInSeconds" .=) <$>
                    _uesmMaximumBatchingWindowInSeconds,
                  ("MaximumRecordAgeInSeconds" .=) <$>
                    _uesmMaximumRecordAgeInSeconds,
                  ("FunctionName" .=) <$> _uesmFunctionName,
                  ("DestinationConfig" .=) <$> _uesmDestinationConfig])

instance ToPath UpdateEventSourceMapping where
        toPath UpdateEventSourceMapping'{..}
          = mconcat
              ["/2015-03-31/event-source-mappings/",
               toBS _uesmUUId]

instance ToQuery UpdateEventSourceMapping where
        toQuery = const mempty

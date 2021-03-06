{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types
    (
    -- * Service Configuration
      kinesisAnalytics

    -- * Errors
    , _UnsupportedOperationException
    , _TooManyTagsException
    , _ResourceNotFoundException
    , _UnableToDetectSchemaException
    , _ServiceUnavailableException
    , _InvalidArgumentException
    , _ConcurrentModificationException
    , _InvalidApplicationConfigurationException
    , _ResourceProvisionedThroughputExceededException
    , _LimitExceededException
    , _ResourceInUseException
    , _CodeValidationException

    -- * ApplicationStatus
    , ApplicationStatus (..)

    -- * InputStartingPosition
    , InputStartingPosition (..)

    -- * RecordFormatType
    , RecordFormatType (..)

    -- * ApplicationDetail
    , ApplicationDetail
    , applicationDetail
    , adApplicationDescription
    , adOutputDescriptions
    , adCloudWatchLoggingOptionDescriptions
    , adReferenceDataSourceDescriptions
    , adInputDescriptions
    , adApplicationCode
    , adCreateTimestamp
    , adLastUpdateTimestamp
    , adApplicationName
    , adApplicationARN
    , adApplicationStatus
    , adApplicationVersionId

    -- * ApplicationSummary
    , ApplicationSummary
    , applicationSummary
    , asApplicationName
    , asApplicationARN
    , asApplicationStatus

    -- * ApplicationUpdate
    , ApplicationUpdate
    , applicationUpdate
    , auReferenceDataSourceUpdates
    , auInputUpdates
    , auCloudWatchLoggingOptionUpdates
    , auOutputUpdates
    , auApplicationCodeUpdate

    -- * CSVMappingParameters
    , CSVMappingParameters
    , csvMappingParameters
    , cmpRecordRowDelimiter
    , cmpRecordColumnDelimiter

    -- * CloudWatchLoggingOption
    , CloudWatchLoggingOption
    , cloudWatchLoggingOption
    , cwloLogStreamARN
    , cwloRoleARN

    -- * CloudWatchLoggingOptionDescription
    , CloudWatchLoggingOptionDescription
    , cloudWatchLoggingOptionDescription
    , cwlodCloudWatchLoggingOptionId
    , cwlodLogStreamARN
    , cwlodRoleARN

    -- * CloudWatchLoggingOptionUpdate
    , CloudWatchLoggingOptionUpdate
    , cloudWatchLoggingOptionUpdate
    , cwlouRoleARNUpdate
    , cwlouLogStreamARNUpdate
    , cwlouCloudWatchLoggingOptionId

    -- * DestinationSchema
    , DestinationSchema
    , destinationSchema
    , dsRecordFormatType

    -- * Input
    , Input
    , input
    , iInputParallelism
    , iInputProcessingConfiguration
    , iKinesisStreamsInput
    , iKinesisFirehoseInput
    , iNamePrefix
    , iInputSchema

    -- * InputConfiguration
    , InputConfiguration
    , inputConfiguration
    , icId
    , icInputStartingPositionConfiguration

    -- * InputDescription
    , InputDescription
    , inputDescription
    , idInputStartingPositionConfiguration
    , idInputParallelism
    , idInputId
    , idInAppStreamNames
    , idKinesisFirehoseInputDescription
    , idInputSchema
    , idKinesisStreamsInputDescription
    , idNamePrefix
    , idInputProcessingConfigurationDescription

    -- * InputLambdaProcessor
    , InputLambdaProcessor
    , inputLambdaProcessor
    , ilpResourceARN
    , ilpRoleARN

    -- * InputLambdaProcessorDescription
    , InputLambdaProcessorDescription
    , inputLambdaProcessorDescription
    , ilpdResourceARN
    , ilpdRoleARN

    -- * InputLambdaProcessorUpdate
    , InputLambdaProcessorUpdate
    , inputLambdaProcessorUpdate
    , ilpuRoleARNUpdate
    , ilpuResourceARNUpdate

    -- * InputParallelism
    , InputParallelism
    , inputParallelism
    , ipCount

    -- * InputParallelismUpdate
    , InputParallelismUpdate
    , inputParallelismUpdate
    , ipuCountUpdate

    -- * InputProcessingConfiguration
    , InputProcessingConfiguration
    , inputProcessingConfiguration
    , ipcInputLambdaProcessor

    -- * InputProcessingConfigurationDescription
    , InputProcessingConfigurationDescription
    , inputProcessingConfigurationDescription
    , ipcdInputLambdaProcessorDescription

    -- * InputProcessingConfigurationUpdate
    , InputProcessingConfigurationUpdate
    , inputProcessingConfigurationUpdate
    , ipcuInputLambdaProcessorUpdate

    -- * InputSchemaUpdate
    , InputSchemaUpdate
    , inputSchemaUpdate
    , isuRecordFormatUpdate
    , isuRecordEncodingUpdate
    , isuRecordColumnUpdates

    -- * InputStartingPositionConfiguration
    , InputStartingPositionConfiguration
    , inputStartingPositionConfiguration
    , ispcInputStartingPosition

    -- * InputUpdate
    , InputUpdate
    , inputUpdate
    , iuInputProcessingConfigurationUpdate
    , iuKinesisStreamsInputUpdate
    , iuInputParallelismUpdate
    , iuNamePrefixUpdate
    , iuInputSchemaUpdate
    , iuKinesisFirehoseInputUpdate
    , iuInputId

    -- * JSONMappingParameters
    , JSONMappingParameters
    , jsonMappingParameters
    , jmpRecordRowPath

    -- * KinesisFirehoseInput
    , KinesisFirehoseInput
    , kinesisFirehoseInput
    , kfiResourceARN
    , kfiRoleARN

    -- * KinesisFirehoseInputDescription
    , KinesisFirehoseInputDescription
    , kinesisFirehoseInputDescription
    , kfidResourceARN
    , kfidRoleARN

    -- * KinesisFirehoseInputUpdate
    , KinesisFirehoseInputUpdate
    , kinesisFirehoseInputUpdate
    , kfiuRoleARNUpdate
    , kfiuResourceARNUpdate

    -- * KinesisFirehoseOutput
    , KinesisFirehoseOutput
    , kinesisFirehoseOutput
    , kfoResourceARN
    , kfoRoleARN

    -- * KinesisFirehoseOutputDescription
    , KinesisFirehoseOutputDescription
    , kinesisFirehoseOutputDescription
    , kfodResourceARN
    , kfodRoleARN

    -- * KinesisFirehoseOutputUpdate
    , KinesisFirehoseOutputUpdate
    , kinesisFirehoseOutputUpdate
    , kfouRoleARNUpdate
    , kfouResourceARNUpdate

    -- * KinesisStreamsInput
    , KinesisStreamsInput
    , kinesisStreamsInput
    , ksiResourceARN
    , ksiRoleARN

    -- * KinesisStreamsInputDescription
    , KinesisStreamsInputDescription
    , kinesisStreamsInputDescription
    , ksidResourceARN
    , ksidRoleARN

    -- * KinesisStreamsInputUpdate
    , KinesisStreamsInputUpdate
    , kinesisStreamsInputUpdate
    , ksiuRoleARNUpdate
    , ksiuResourceARNUpdate

    -- * KinesisStreamsOutput
    , KinesisStreamsOutput
    , kinesisStreamsOutput
    , ksoResourceARN
    , ksoRoleARN

    -- * KinesisStreamsOutputDescription
    , KinesisStreamsOutputDescription
    , kinesisStreamsOutputDescription
    , ksodResourceARN
    , ksodRoleARN

    -- * KinesisStreamsOutputUpdate
    , KinesisStreamsOutputUpdate
    , kinesisStreamsOutputUpdate
    , ksouRoleARNUpdate
    , ksouResourceARNUpdate

    -- * LambdaOutput
    , LambdaOutput
    , lambdaOutput
    , loResourceARN
    , loRoleARN

    -- * LambdaOutputDescription
    , LambdaOutputDescription
    , lambdaOutputDescription
    , lodResourceARN
    , lodRoleARN

    -- * LambdaOutputUpdate
    , LambdaOutputUpdate
    , lambdaOutputUpdate
    , louRoleARNUpdate
    , louResourceARNUpdate

    -- * MappingParameters
    , MappingParameters
    , mappingParameters
    , mpCSVMappingParameters
    , mpJSONMappingParameters

    -- * Output
    , Output
    , output
    , oLambdaOutput
    , oKinesisStreamsOutput
    , oKinesisFirehoseOutput
    , oName
    , oDestinationSchema

    -- * OutputDescription
    , OutputDescription
    , outputDescription
    , odOutputId
    , odDestinationSchema
    , odKinesisFirehoseOutputDescription
    , odKinesisStreamsOutputDescription
    , odName
    , odLambdaOutputDescription

    -- * OutputUpdate
    , OutputUpdate
    , outputUpdate
    , ouKinesisStreamsOutputUpdate
    , ouDestinationSchemaUpdate
    , ouKinesisFirehoseOutputUpdate
    , ouNameUpdate
    , ouLambdaOutputUpdate
    , ouOutputId

    -- * RecordColumn
    , RecordColumn
    , recordColumn
    , rcMapping
    , rcName
    , rcSqlType

    -- * RecordFormat
    , RecordFormat
    , recordFormat
    , rfMappingParameters
    , rfRecordFormatType

    -- * ReferenceDataSource
    , ReferenceDataSource
    , referenceDataSource
    , rdsS3ReferenceDataSource
    , rdsTableName
    , rdsReferenceSchema

    -- * ReferenceDataSourceDescription
    , ReferenceDataSourceDescription
    , referenceDataSourceDescription
    , rdsdReferenceSchema
    , rdsdReferenceId
    , rdsdTableName
    , rdsdS3ReferenceDataSourceDescription

    -- * ReferenceDataSourceUpdate
    , ReferenceDataSourceUpdate
    , referenceDataSourceUpdate
    , rdsuTableNameUpdate
    , rdsuS3ReferenceDataSourceUpdate
    , rdsuReferenceSchemaUpdate
    , rdsuReferenceId

    -- * S3Configuration
    , S3Configuration
    , s3Configuration
    , scRoleARN
    , scBucketARN
    , scFileKey

    -- * S3ReferenceDataSource
    , S3ReferenceDataSource
    , s3ReferenceDataSource
    , srdsBucketARN
    , srdsFileKey
    , srdsReferenceRoleARN

    -- * S3ReferenceDataSourceDescription
    , S3ReferenceDataSourceDescription
    , s3ReferenceDataSourceDescription
    , srdsdBucketARN
    , srdsdFileKey
    , srdsdReferenceRoleARN

    -- * S3ReferenceDataSourceUpdate
    , S3ReferenceDataSourceUpdate
    , s3ReferenceDataSourceUpdate
    , srdsuBucketARNUpdate
    , srdsuFileKeyUpdate
    , srdsuReferenceRoleARNUpdate

    -- * SourceSchema
    , SourceSchema
    , sourceSchema
    , ssRecordEncoding
    , ssRecordFormat
    , ssRecordColumns

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.KinesisAnalytics.Types.ApplicationStatus
import Network.AWS.KinesisAnalytics.Types.InputStartingPosition
import Network.AWS.KinesisAnalytics.Types.RecordFormatType
import Network.AWS.KinesisAnalytics.Types.ApplicationDetail
import Network.AWS.KinesisAnalytics.Types.ApplicationSummary
import Network.AWS.KinesisAnalytics.Types.ApplicationUpdate
import Network.AWS.KinesisAnalytics.Types.CSVMappingParameters
import Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOption
import Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionDescription
import Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionUpdate
import Network.AWS.KinesisAnalytics.Types.DestinationSchema
import Network.AWS.KinesisAnalytics.Types.Input
import Network.AWS.KinesisAnalytics.Types.InputConfiguration
import Network.AWS.KinesisAnalytics.Types.InputDescription
import Network.AWS.KinesisAnalytics.Types.InputLambdaProcessor
import Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorDescription
import Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorUpdate
import Network.AWS.KinesisAnalytics.Types.InputParallelism
import Network.AWS.KinesisAnalytics.Types.InputParallelismUpdate
import Network.AWS.KinesisAnalytics.Types.InputProcessingConfiguration
import Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationDescription
import Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationUpdate
import Network.AWS.KinesisAnalytics.Types.InputSchemaUpdate
import Network.AWS.KinesisAnalytics.Types.InputStartingPositionConfiguration
import Network.AWS.KinesisAnalytics.Types.InputUpdate
import Network.AWS.KinesisAnalytics.Types.JSONMappingParameters
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInput
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputDescription
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputUpdate
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutput
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputDescription
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputUpdate
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsInput
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputDescription
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputUpdate
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutput
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputDescription
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputUpdate
import Network.AWS.KinesisAnalytics.Types.LambdaOutput
import Network.AWS.KinesisAnalytics.Types.LambdaOutputDescription
import Network.AWS.KinesisAnalytics.Types.LambdaOutputUpdate
import Network.AWS.KinesisAnalytics.Types.MappingParameters
import Network.AWS.KinesisAnalytics.Types.Output
import Network.AWS.KinesisAnalytics.Types.OutputDescription
import Network.AWS.KinesisAnalytics.Types.OutputUpdate
import Network.AWS.KinesisAnalytics.Types.RecordColumn
import Network.AWS.KinesisAnalytics.Types.RecordFormat
import Network.AWS.KinesisAnalytics.Types.ReferenceDataSource
import Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceDescription
import Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceUpdate
import Network.AWS.KinesisAnalytics.Types.S3Configuration
import Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSource
import Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceDescription
import Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceUpdate
import Network.AWS.KinesisAnalytics.Types.SourceSchema
import Network.AWS.KinesisAnalytics.Types.Tag

-- | API version @2015-08-14@ of the Amazon Kinesis Analytics SDK configuration.
kinesisAnalytics :: Service
kinesisAnalytics
  = Service{_svcAbbrev = "KinesisAnalytics",
            _svcSigner = v4, _svcPrefix = "kinesisanalytics",
            _svcVersion = "2015-08-14",
            _svcEndpoint = defaultEndpoint kinesisAnalytics,
            _svcTimeout = Just 70, _svcCheck = statusSuccess,
            _svcError = parseJSONError "KinesisAnalytics",
            _svcRetry = retry}
  where retry
          = Exponential{_retryBase = 5.0e-2, _retryGrowth = 2,
                        _retryAttempts = 5, _retryCheck = check}
        check e
          | has (hasCode "ThrottledException" . hasStatus 400)
              e
            = Just "throttled_exception"
          | has (hasStatus 429) e = Just "too_many_requests"
          | has (hasCode "ThrottlingException" . hasStatus 400)
              e
            = Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e =
            Just "throttling"
          | has
              (hasCode "ProvisionedThroughputExceededException" .
                 hasStatus 400)
              e
            = Just "throughput_exceeded"
          | has (hasStatus 504) e = Just "gateway_timeout"
          | has
              (hasCode "RequestThrottledException" . hasStatus 400)
              e
            = Just "request_throttled_exception"
          | has (hasStatus 502) e = Just "bad_gateway"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | The request was rejected because a specified parameter is not supported or a specified resource is not valid for this operation. 
--
--
_UnsupportedOperationException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedOperationException
  = _MatchServiceError kinesisAnalytics
      "UnsupportedOperationException"

-- | Application created with too many tags, or too many tags added to an application. Note that the maximum number of application tags includes system tags. The maximum number of user-defined application tags is 50.
--
--
_TooManyTagsException :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyTagsException
  = _MatchServiceError kinesisAnalytics
      "TooManyTagsException"

-- | Specified application can't be found.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException
  = _MatchServiceError kinesisAnalytics
      "ResourceNotFoundException"

-- | Data format is not valid. Amazon Kinesis Analytics is not able to detect schema for the given streaming source.
--
--
_UnableToDetectSchemaException :: AsError a => Getting (First ServiceError) a ServiceError
_UnableToDetectSchemaException
  = _MatchServiceError kinesisAnalytics
      "UnableToDetectSchemaException"

-- | The service is unavailable. Back off and retry the operation. 
--
--
_ServiceUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceUnavailableException
  = _MatchServiceError kinesisAnalytics
      "ServiceUnavailableException"

-- | Specified input parameter value is invalid.
--
--
_InvalidArgumentException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidArgumentException
  = _MatchServiceError kinesisAnalytics
      "InvalidArgumentException"

-- | Exception thrown as a result of concurrent modification to an application. For example, two individuals attempting to edit the same application at the same time.
--
--
_ConcurrentModificationException :: AsError a => Getting (First ServiceError) a ServiceError
_ConcurrentModificationException
  = _MatchServiceError kinesisAnalytics
      "ConcurrentModificationException"

-- | User-provided application configuration is not valid.
--
--
_InvalidApplicationConfigurationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidApplicationConfigurationException
  = _MatchServiceError kinesisAnalytics
      "InvalidApplicationConfigurationException"

-- | Discovery failed to get a record from the streaming source because of the Amazon Kinesis Streams ProvisionedThroughputExceededException. For more information, see <https://docs.aws.amazon.com/kinesis/latest/APIReference/API_GetRecords.html GetRecords> in the Amazon Kinesis Streams API Reference.
--
--
_ResourceProvisionedThroughputExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceProvisionedThroughputExceededException
  = _MatchServiceError kinesisAnalytics
      "ResourceProvisionedThroughputExceededException"

-- | Exceeded the number of applications allowed.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException
  = _MatchServiceError kinesisAnalytics
      "LimitExceededException"

-- | Application is not available for this operation.
--
--
_ResourceInUseException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceInUseException
  = _MatchServiceError kinesisAnalytics
      "ResourceInUseException"

-- | User-provided application code (query) is invalid. This can be a simple syntax error.
--
--
_CodeValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_CodeValidationException
  = _MatchServiceError kinesisAnalytics
      "CodeValidationException"

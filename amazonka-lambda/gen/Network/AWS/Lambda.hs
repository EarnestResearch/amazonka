{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Lambda__ 
--
-- __Overview__ 
--
-- This is the /AWS Lambda API Reference/ . The AWS Lambda Developer Guide provides additional information. For the service overview, see <https://docs.aws.amazon.com/lambda/latest/dg/welcome.html What is AWS Lambda> , and for information about how the service works, see <https://docs.aws.amazon.com/lambda/latest/dg/lambda-introduction.html AWS Lambda: How it Works> in the __AWS Lambda Developer Guide__ .
--
module Network.AWS.Lambda
    (
    -- * Service Configuration
      lambda

    -- * Errors
    -- $errors

    -- ** PolicyLengthExceededException
    , _PolicyLengthExceededException

    -- ** CodeStorageExceededException
    , _CodeStorageExceededException

    -- ** PreconditionFailedException
    , _PreconditionFailedException

    -- ** KMSDisabledException
    , _KMSDisabledException

    -- ** SubnetIPAddressLimitReachedException
    , _SubnetIPAddressLimitReachedException

    -- ** TooManyRequestsException
    , _TooManyRequestsException

    -- ** ServiceException
    , _ServiceException

    -- ** InvalidRuntimeException
    , _InvalidRuntimeException

    -- ** EC2ThrottledException
    , _EC2ThrottledException

    -- ** RequestTooLargeException
    , _RequestTooLargeException

    -- ** ENILimitReachedException
    , _ENILimitReachedException

    -- ** InvalidParameterValueException
    , _InvalidParameterValueException

    -- ** KMSNotFoundException
    , _KMSNotFoundException

    -- ** InvalidRequestContentException
    , _InvalidRequestContentException

    -- ** ResourceNotFoundException
    , _ResourceNotFoundException

    -- ** InvalidZipFileException
    , _InvalidZipFileException

    -- ** ResourceNotReadyException
    , _ResourceNotReadyException

    -- ** ProvisionedConcurrencyConfigNotFoundException
    , _ProvisionedConcurrencyConfigNotFoundException

    -- ** ResourceConflictException
    , _ResourceConflictException

    -- ** EC2AccessDeniedException
    , _EC2AccessDeniedException

    -- ** InvalidSubnetIdException
    , _InvalidSubnetIdException

    -- ** InvalidSecurityGroupIdException
    , _InvalidSecurityGroupIdException

    -- ** KMSInvalidStateException
    , _KMSInvalidStateException

    -- ** KMSAccessDeniedException
    , _KMSAccessDeniedException

    -- ** ResourceInUseException
    , _ResourceInUseException

    -- ** EC2UnexpectedException
    , _EC2UnexpectedException

    -- ** UnsupportedMediaTypeException
    , _UnsupportedMediaTypeException

    -- * Waiters
    -- $waiters

    -- ** FunctionExists
    , functionExists

    -- ** FunctionActive
    , functionActive

    -- ** FunctionUpdated
    , functionUpdated

    -- * Operations
    -- $operations

    -- ** GetFunctionConfiguration 
    , module Network.AWS.Lambda.GetFunctionConfiguration

    -- ** DeleteEventSourceMapping 
    , module Network.AWS.Lambda.DeleteEventSourceMapping

    -- ** UpdateEventSourceMapping 
    , module Network.AWS.Lambda.UpdateEventSourceMapping

    -- ** GetLayerVersion 
    , module Network.AWS.Lambda.GetLayerVersion

    -- ** CreateAlias 
    , module Network.AWS.Lambda.CreateAlias

    -- ** ListVersionsByFunction (Paginated)
    , module Network.AWS.Lambda.ListVersionsByFunction

    -- ** ListAliases (Paginated)
    , module Network.AWS.Lambda.ListAliases

    -- ** RemovePermission 
    , module Network.AWS.Lambda.RemovePermission

    -- ** DeleteFunctionEventInvokeConfig 
    , module Network.AWS.Lambda.DeleteFunctionEventInvokeConfig

    -- ** UpdateFunctionEventInvokeConfig 
    , module Network.AWS.Lambda.UpdateFunctionEventInvokeConfig

    -- ** PutFunctionEventInvokeConfig 
    , module Network.AWS.Lambda.PutFunctionEventInvokeConfig

    -- ** Invoke 
    , module Network.AWS.Lambda.Invoke

    -- ** DeleteLayerVersion 
    , module Network.AWS.Lambda.DeleteLayerVersion

    -- ** GetAlias 
    , module Network.AWS.Lambda.GetAlias

    -- ** PublishLayerVersion 
    , module Network.AWS.Lambda.PublishLayerVersion

    -- ** GetEventSourceMapping 
    , module Network.AWS.Lambda.GetEventSourceMapping

    -- ** AddLayerVersionPermission 
    , module Network.AWS.Lambda.AddLayerVersionPermission

    -- ** ListProvisionedConcurrencyConfigs (Paginated)
    , module Network.AWS.Lambda.ListProvisionedConcurrencyConfigs

    -- ** PutFunctionConcurrency 
    , module Network.AWS.Lambda.PutFunctionConcurrency

    -- ** CreateFunction 
    , module Network.AWS.Lambda.CreateFunction

    -- ** DeleteFunctionConcurrency 
    , module Network.AWS.Lambda.DeleteFunctionConcurrency

    -- ** GetLayerVersionByARN 
    , module Network.AWS.Lambda.GetLayerVersionByARN

    -- ** GetFunctionConcurrency 
    , module Network.AWS.Lambda.GetFunctionConcurrency

    -- ** CreateEventSourceMapping 
    , module Network.AWS.Lambda.CreateEventSourceMapping

    -- ** GetProvisionedConcurrencyConfig 
    , module Network.AWS.Lambda.GetProvisionedConcurrencyConfig

    -- ** RemoveLayerVersionPermission 
    , module Network.AWS.Lambda.RemoveLayerVersionPermission

    -- ** GetFunction 
    , module Network.AWS.Lambda.GetFunction

    -- ** ListEventSourceMappings (Paginated)
    , module Network.AWS.Lambda.ListEventSourceMappings

    -- ** GetLayerVersionPolicy 
    , module Network.AWS.Lambda.GetLayerVersionPolicy

    -- ** DeleteAlias 
    , module Network.AWS.Lambda.DeleteAlias

    -- ** UpdateAlias 
    , module Network.AWS.Lambda.UpdateAlias

    -- ** GetAccountSettings 
    , module Network.AWS.Lambda.GetAccountSettings

    -- ** GetFunctionEventInvokeConfig 
    , module Network.AWS.Lambda.GetFunctionEventInvokeConfig

    -- ** AddPermission 
    , module Network.AWS.Lambda.AddPermission

    -- ** ListLayers (Paginated)
    , module Network.AWS.Lambda.ListLayers

    -- ** ListFunctionEventInvokeConfigs (Paginated)
    , module Network.AWS.Lambda.ListFunctionEventInvokeConfigs

    -- ** ListLayerVersions (Paginated)
    , module Network.AWS.Lambda.ListLayerVersions

    -- ** TagResource 
    , module Network.AWS.Lambda.TagResource

    -- ** PublishVersion 
    , module Network.AWS.Lambda.PublishVersion

    -- ** ListTags 
    , module Network.AWS.Lambda.ListTags

    -- ** DeleteFunction 
    , module Network.AWS.Lambda.DeleteFunction

    -- ** UntagResource 
    , module Network.AWS.Lambda.UntagResource

    -- ** UpdateFunctionConfiguration 
    , module Network.AWS.Lambda.UpdateFunctionConfiguration

    -- ** ListFunctions (Paginated)
    , module Network.AWS.Lambda.ListFunctions

    -- ** UpdateFunctionCode 
    , module Network.AWS.Lambda.UpdateFunctionCode

    -- ** DeleteProvisionedConcurrencyConfig 
    , module Network.AWS.Lambda.DeleteProvisionedConcurrencyConfig

    -- ** GetPolicy 
    , module Network.AWS.Lambda.GetPolicy

    -- ** PutProvisionedConcurrencyConfig 
    , module Network.AWS.Lambda.PutProvisionedConcurrencyConfig

    -- * Types

    -- ** EventSourcePosition
    , EventSourcePosition (..)

    -- ** FunctionVersion
    , FunctionVersion (..)

    -- ** InvocationType
    , InvocationType (..)

    -- ** LastUpdateStatus
    , LastUpdateStatus (..)

    -- ** LastUpdateStatusReasonCode
    , LastUpdateStatusReasonCode (..)

    -- ** LogType
    , LogType (..)

    -- ** ProvisionedConcurrencyStatusEnum
    , ProvisionedConcurrencyStatusEnum (..)

    -- ** Runtime
    , Runtime (..)

    -- ** State
    , State (..)

    -- ** StateReasonCode
    , StateReasonCode (..)

    -- ** TracingMode
    , TracingMode (..)

    -- ** AccountLimit
    , AccountLimit
    , accountLimit
    , alConcurrentExecutions
    , alTotalCodeSize
    , alUnreservedConcurrentExecutions
    , alCodeSizeUnzipped
    , alCodeSizeZipped

    -- ** AccountUsage
    , AccountUsage
    , accountUsage
    , auTotalCodeSize
    , auFunctionCount

    -- ** AliasConfiguration
    , AliasConfiguration
    , aliasConfiguration
    , acRoutingConfig
    , acName
    , acFunctionVersion
    , acAliasARN
    , acDescription
    , acRevisionId

    -- ** AliasRoutingConfiguration
    , AliasRoutingConfiguration
    , aliasRoutingConfiguration
    , arcAdditionalVersionWeights

    -- ** Concurrency
    , Concurrency
    , concurrency
    , cReservedConcurrentExecutions

    -- ** DeadLetterConfig
    , DeadLetterConfig
    , deadLetterConfig
    , dlcTargetARN

    -- ** DestinationConfig
    , DestinationConfig
    , destinationConfig
    , dcOnSuccess
    , dcOnFailure

    -- ** Environment
    , Environment
    , environment
    , eVariables

    -- ** EnvironmentError
    , EnvironmentError
    , environmentError
    , eeErrorCode
    , eeMessage

    -- ** EnvironmentResponse
    , EnvironmentResponse
    , environmentResponse
    , envVariables
    , envError

    -- ** EventSourceMappingConfiguration
    , EventSourceMappingConfiguration
    , eventSourceMappingConfiguration
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

    -- ** FunctionCode
    , FunctionCode
    , functionCode
    , fcS3ObjectVersion
    , fcS3Key
    , fcZipFile
    , fcS3Bucket

    -- ** FunctionCodeLocation
    , FunctionCodeLocation
    , functionCodeLocation
    , fclLocation
    , fclRepositoryType

    -- ** FunctionConfiguration
    , FunctionConfiguration
    , functionConfiguration
    , fcMemorySize
    , fcRuntime
    , fcState
    , fcLastUpdateStatus
    , fcFunctionARN
    , fcKMSKeyARN
    , fcEnvironment
    , fcDeadLetterConfig
    , fcRole
    , fcVPCConfig
    , fcVersion
    , fcFunctionName
    , fcLayers
    , fcCodeSize
    , fcHandler
    , fcTimeout
    , fcLastUpdateStatusReason
    , fcStateReason
    , fcLastModified
    , fcCodeSha256
    , fcTracingConfig
    , fcStateReasonCode
    , fcDescription
    , fcLastUpdateStatusReasonCode
    , fcRevisionId
    , fcMasterARN

    -- ** FunctionEventInvokeConfig
    , FunctionEventInvokeConfig
    , functionEventInvokeConfig
    , feicFunctionARN
    , feicMaximumEventAgeInSeconds
    , feicMaximumRetryAttempts
    , feicLastModified
    , feicDestinationConfig

    -- ** GetLayerVersionResponse
    , GetLayerVersionResponse
    , getLayerVersionResponse
    , glvLayerVersionARN
    , glvContent
    , glvCreatedDate
    , glvVersion
    , glvLicenseInfo
    , glvLayerARN
    , glvDescription
    , glvCompatibleRuntimes

    -- ** Layer
    , Layer
    , layer
    , lARN
    , lCodeSize

    -- ** LayerVersionContentInput
    , LayerVersionContentInput
    , layerVersionContentInput
    , lvciS3ObjectVersion
    , lvciS3Key
    , lvciZipFile
    , lvciS3Bucket

    -- ** LayerVersionContentOutput
    , LayerVersionContentOutput
    , layerVersionContentOutput
    , lvcoLocation
    , lvcoCodeSize
    , lvcoCodeSha256

    -- ** LayerVersionsListItem
    , LayerVersionsListItem
    , layerVersionsListItem
    , lvliLayerVersionARN
    , lvliCreatedDate
    , lvliVersion
    , lvliLicenseInfo
    , lvliDescription
    , lvliCompatibleRuntimes

    -- ** LayersListItem
    , LayersListItem
    , layersListItem
    , lliLayerName
    , lliLatestMatchingVersion
    , lliLayerARN

    -- ** OnFailure
    , OnFailure
    , onFailure
    , ofDestination

    -- ** OnSuccess
    , OnSuccess
    , onSuccess
    , osDestination

    -- ** ProvisionedConcurrencyConfigListItem
    , ProvisionedConcurrencyConfigListItem
    , provisionedConcurrencyConfigListItem
    , pccliStatus
    , pccliFunctionARN
    , pccliRequestedProvisionedConcurrentExecutions
    , pccliAvailableProvisionedConcurrentExecutions
    , pccliStatusReason
    , pccliAllocatedProvisionedConcurrentExecutions
    , pccliLastModified

    -- ** TracingConfig
    , TracingConfig
    , tracingConfig
    , tMode

    -- ** TracingConfigResponse
    , TracingConfigResponse
    , tracingConfigResponse
    , tcMode

    -- ** VPCConfig
    , VPCConfig
    , vpcConfig
    , vpccSecurityGroupIds
    , vpccSubnetIds

    -- ** VPCConfigResponse
    , VPCConfigResponse
    , vpcConfigResponse
    , vcSecurityGroupIds
    , vcSubnetIds
    , vcVPCId
    ) where

import Network.AWS.Lambda.AddLayerVersionPermission
import Network.AWS.Lambda.AddPermission
import Network.AWS.Lambda.CreateAlias
import Network.AWS.Lambda.CreateEventSourceMapping
import Network.AWS.Lambda.CreateFunction
import Network.AWS.Lambda.DeleteAlias
import Network.AWS.Lambda.DeleteEventSourceMapping
import Network.AWS.Lambda.DeleteFunction
import Network.AWS.Lambda.DeleteFunctionConcurrency
import Network.AWS.Lambda.DeleteFunctionEventInvokeConfig
import Network.AWS.Lambda.DeleteLayerVersion
import Network.AWS.Lambda.DeleteProvisionedConcurrencyConfig
import Network.AWS.Lambda.GetAccountSettings
import Network.AWS.Lambda.GetAlias
import Network.AWS.Lambda.GetEventSourceMapping
import Network.AWS.Lambda.GetFunction
import Network.AWS.Lambda.GetFunctionConcurrency
import Network.AWS.Lambda.GetFunctionConfiguration
import Network.AWS.Lambda.GetFunctionEventInvokeConfig
import Network.AWS.Lambda.GetLayerVersion
import Network.AWS.Lambda.GetLayerVersionByARN
import Network.AWS.Lambda.GetLayerVersionPolicy
import Network.AWS.Lambda.GetPolicy
import Network.AWS.Lambda.GetProvisionedConcurrencyConfig
import Network.AWS.Lambda.Invoke
import Network.AWS.Lambda.ListAliases
import Network.AWS.Lambda.ListEventSourceMappings
import Network.AWS.Lambda.ListFunctionEventInvokeConfigs
import Network.AWS.Lambda.ListFunctions
import Network.AWS.Lambda.ListLayerVersions
import Network.AWS.Lambda.ListLayers
import Network.AWS.Lambda.ListProvisionedConcurrencyConfigs
import Network.AWS.Lambda.ListTags
import Network.AWS.Lambda.ListVersionsByFunction
import Network.AWS.Lambda.PublishLayerVersion
import Network.AWS.Lambda.PublishVersion
import Network.AWS.Lambda.PutFunctionConcurrency
import Network.AWS.Lambda.PutFunctionEventInvokeConfig
import Network.AWS.Lambda.PutProvisionedConcurrencyConfig
import Network.AWS.Lambda.RemoveLayerVersionPermission
import Network.AWS.Lambda.RemovePermission
import Network.AWS.Lambda.TagResource
import Network.AWS.Lambda.Types
import Network.AWS.Lambda.UntagResource
import Network.AWS.Lambda.UpdateAlias
import Network.AWS.Lambda.UpdateEventSourceMapping
import Network.AWS.Lambda.UpdateFunctionCode
import Network.AWS.Lambda.UpdateFunctionConfiguration
import Network.AWS.Lambda.UpdateFunctionEventInvokeConfig
import Network.AWS.Lambda.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Lambda'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}

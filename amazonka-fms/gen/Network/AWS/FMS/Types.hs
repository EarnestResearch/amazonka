{-# LANGUAGE OverloadedStrings  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.FMS.Types
    (
    -- * Service Configuration
      fms

    -- * Errors
    , _ResourceNotFoundException
    , _InvalidOperationException
    , _InternalErrorException
    , _InvalidInputException
    , _LimitExceededException
    , _InvalidTypeException

    -- * AccountRoleStatus
    , AccountRoleStatus (..)

    -- * CustomerPolicyScopeIdType
    , CustomerPolicyScopeIdType (..)

    -- * DependentServiceName
    , DependentServiceName (..)

    -- * PolicyComplianceStatusType
    , PolicyComplianceStatusType (..)

    -- * SecurityServiceType
    , SecurityServiceType (..)

    -- * ViolationReason
    , ViolationReason (..)

    -- * ComplianceViolator
    , ComplianceViolator
    , complianceViolator
    , cvResourceId
    , cvResourceType
    , cvViolationReason

    -- * EvaluationResult
    , EvaluationResult
    , evaluationResult
    , erViolatorCount
    , erComplianceStatus
    , erEvaluationLimitExceeded

    -- * Policy
    , Policy
    , policy
    , pPolicyId
    , pResourceTypeList
    , pResourceTags
    , pPolicyUpdateToken
    , pExcludeMap
    , pIncludeMap
    , pPolicyName
    , pSecurityServicePolicyData
    , pResourceType
    , pExcludeResourceTags
    , pRemediationEnabled

    -- * PolicyComplianceDetail
    , PolicyComplianceDetail
    , policyComplianceDetail
    , pcdExpiredAt
    , pcdPolicyId
    , pcdViolators
    , pcdEvaluationLimitExceeded
    , pcdIssueInfoMap
    , pcdPolicyOwner
    , pcdMemberAccount

    -- * PolicyComplianceStatus
    , PolicyComplianceStatus
    , policyComplianceStatus
    , pcsEvaluationResults
    , pcsLastUpdated
    , pcsPolicyName
    , pcsPolicyId
    , pcsIssueInfoMap
    , pcsPolicyOwner
    , pcsMemberAccount

    -- * PolicySummary
    , PolicySummary
    , policySummary
    , psPolicyName
    , psRemediationEnabled
    , psResourceType
    , psPolicyId
    , psPolicyARN
    , psSecurityServiceType

    -- * ResourceTag
    , ResourceTag
    , resourceTag
    , rtValue
    , rtKey

    -- * SecurityServicePolicyData
    , SecurityServicePolicyData
    , securityServicePolicyData
    , sspdManagedServiceData
    , sspdType

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.FMS.Types.AccountRoleStatus
import Network.AWS.FMS.Types.CustomerPolicyScopeIdType
import Network.AWS.FMS.Types.DependentServiceName
import Network.AWS.FMS.Types.PolicyComplianceStatusType
import Network.AWS.FMS.Types.SecurityServiceType
import Network.AWS.FMS.Types.ViolationReason
import Network.AWS.FMS.Types.ComplianceViolator
import Network.AWS.FMS.Types.EvaluationResult
import Network.AWS.FMS.Types.Policy
import Network.AWS.FMS.Types.PolicyComplianceDetail
import Network.AWS.FMS.Types.PolicyComplianceStatus
import Network.AWS.FMS.Types.PolicySummary
import Network.AWS.FMS.Types.ResourceTag
import Network.AWS.FMS.Types.SecurityServicePolicyData
import Network.AWS.FMS.Types.Tag

-- | API version @2018-01-01@ of the Amazon Firewall Management Service SDK configuration.
fms :: Service
fms
  = Service{_svcAbbrev = "FMS", _svcSigner = v4,
            _svcPrefix = "fms", _svcVersion = "2018-01-01",
            _svcEndpoint = defaultEndpoint fms,
            _svcTimeout = Just 70, _svcCheck = statusSuccess,
            _svcError = parseJSONError "FMS", _svcRetry = retry}
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

-- | The specified resource was not found.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException
  = _MatchServiceError fms "ResourceNotFoundException"

-- | The operation failed because there was nothing to do. For example, you might have submitted an @AssociateAdminAccount@ request, but the account ID that you submitted was already set as the AWS Firewall Manager administrator.
--
--
_InvalidOperationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidOperationException
  = _MatchServiceError fms "InvalidOperationException"

-- | The operation failed because of a system problem, even though the request was valid. Retry your request.
--
--
_InternalErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalErrorException
  = _MatchServiceError fms "InternalErrorException"

-- | The parameters of the request were invalid.
--
--
_InvalidInputException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInputException
  = _MatchServiceError fms "InvalidInputException"

-- | The operation exceeds a resource limit, for example, the maximum number of @policy@ objects that you can create for an AWS account. For more information, see <https://docs.aws.amazon.com/waf/latest/developerguide/fms-limits.html Firewall Manager Limits> in the /AWS WAF Developer Guide/ .
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException
  = _MatchServiceError fms "LimitExceededException"

-- | The value of the @Type@ parameter is invalid.
--
--
_InvalidTypeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTypeException
  = _MatchServiceError fms "InvalidTypeException"

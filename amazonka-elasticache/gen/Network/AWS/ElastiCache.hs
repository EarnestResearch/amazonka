{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon ElastiCache__ 
--
-- Amazon ElastiCache is a web service that makes it easier to set up, operate, and scale a distributed cache in the cloud.
--
-- With ElastiCache, customers get all of the benefits of a high-performance, in-memory cache with less of the administrative burden involved in launching and managing a distributed cache. The service makes setup, scaling, and cluster failure handling much simpler than in a self-managed cache deployment.
--
-- In addition, through integration with Amazon CloudWatch, customers get enhanced visibility into the key performance statistics associated with their cache and can receive alarms if a part of their cache runs hot.
--
module Network.AWS.ElastiCache
    (
    -- * Service Configuration
      elastiCache

    -- * Errors
    -- $errors

    -- ** CacheParameterGroupNotFoundFault
    , _CacheParameterGroupNotFoundFault

    -- ** ReplicationGroupAlreadyUnderMigrationFault
    , _ReplicationGroupAlreadyUnderMigrationFault

    -- ** ReplicationGroupNotUnderMigrationFault
    , _ReplicationGroupNotUnderMigrationFault

    -- ** InvalidSnapshotStateFault
    , _InvalidSnapshotStateFault

    -- ** InsufficientCacheClusterCapacityFault
    , _InsufficientCacheClusterCapacityFault

    -- ** SnapshotNotFoundFault
    , _SnapshotNotFoundFault

    -- ** ServiceUpdateNotFoundFault
    , _ServiceUpdateNotFoundFault

    -- ** ClusterQuotaForCustomerExceededFault
    , _ClusterQuotaForCustomerExceededFault

    -- ** CacheSecurityGroupQuotaExceededFault
    , _CacheSecurityGroupQuotaExceededFault

    -- ** InvalidSubnet
    , _InvalidSubnet

    -- ** CacheClusterAlreadyExistsFault
    , _CacheClusterAlreadyExistsFault

    -- ** TagQuotaPerResourceExceeded
    , _TagQuotaPerResourceExceeded

    -- ** AuthorizationNotFoundFault
    , _AuthorizationNotFoundFault

    -- ** ReplicationGroupNotFoundFault
    , _ReplicationGroupNotFoundFault

    -- ** ReservedCacheNodesOfferingNotFoundFault
    , _ReservedCacheNodesOfferingNotFoundFault

    -- ** NodeGroupsPerReplicationGroupQuotaExceededFault
    , _NodeGroupsPerReplicationGroupQuotaExceededFault

    -- ** ReplicationGroupAlreadyExistsFault
    , _ReplicationGroupAlreadyExistsFault

    -- ** AuthorizationAlreadyExistsFault
    , _AuthorizationAlreadyExistsFault

    -- ** ReservedCacheNodeQuotaExceededFault
    , _ReservedCacheNodeQuotaExceededFault

    -- ** ReservedCacheNodeAlreadyExistsFault
    , _ReservedCacheNodeAlreadyExistsFault

    -- ** CacheSecurityGroupNotFoundFault
    , _CacheSecurityGroupNotFoundFault

    -- ** InvalidParameterValueException
    , _InvalidParameterValueException

    -- ** CacheSubnetGroupInUse
    , _CacheSubnetGroupInUse

    -- ** InvalidGlobalReplicationGroupStateFault
    , _InvalidGlobalReplicationGroupStateFault

    -- ** SnapshotFeatureNotSupportedFault
    , _SnapshotFeatureNotSupportedFault

    -- ** ServiceLinkedRoleNotFoundFault
    , _ServiceLinkedRoleNotFoundFault

    -- ** InvalidKMSKeyFault
    , _InvalidKMSKeyFault

    -- ** ReservedCacheNodeNotFoundFault
    , _ReservedCacheNodeNotFoundFault

    -- ** InvalidCacheSecurityGroupStateFault
    , _InvalidCacheSecurityGroupStateFault

    -- ** NodeGroupNotFoundFault
    , _NodeGroupNotFoundFault

    -- ** NodeQuotaForClusterExceededFault
    , _NodeQuotaForClusterExceededFault

    -- ** InvalidARNFault
    , _InvalidARNFault

    -- ** SnapshotQuotaExceededFault
    , _SnapshotQuotaExceededFault

    -- ** TagNotFoundFault
    , _TagNotFoundFault

    -- ** CacheSubnetQuotaExceededFault
    , _CacheSubnetQuotaExceededFault

    -- ** NodeQuotaForCustomerExceededFault
    , _NodeQuotaForCustomerExceededFault

    -- ** SnapshotAlreadyExistsFault
    , _SnapshotAlreadyExistsFault

    -- ** CacheParameterGroupQuotaExceededFault
    , _CacheParameterGroupQuotaExceededFault

    -- ** InvalidCacheClusterStateFault
    , _InvalidCacheClusterStateFault

    -- ** CacheSubnetGroupQuotaExceededFault
    , _CacheSubnetGroupQuotaExceededFault

    -- ** InvalidVPCNetworkStateFault
    , _InvalidVPCNetworkStateFault

    -- ** CacheClusterNotFoundFault
    , _CacheClusterNotFoundFault

    -- ** SubnetInUse
    , _SubnetInUse

    -- ** InvalidReplicationGroupStateFault
    , _InvalidReplicationGroupStateFault

    -- ** TestFailoverNotAvailableFault
    , _TestFailoverNotAvailableFault

    -- ** CacheSubnetGroupAlreadyExistsFault
    , _CacheSubnetGroupAlreadyExistsFault

    -- ** GlobalReplicationGroupAlreadyExistsFault
    , _GlobalReplicationGroupAlreadyExistsFault

    -- ** CacheSubnetGroupNotFoundFault
    , _CacheSubnetGroupNotFoundFault

    -- ** GlobalReplicationGroupNotFoundFault
    , _GlobalReplicationGroupNotFoundFault

    -- ** CacheSecurityGroupAlreadyExistsFault
    , _CacheSecurityGroupAlreadyExistsFault

    -- ** NoOperationFault
    , _NoOperationFault

    -- ** InvalidParameterCombinationException
    , _InvalidParameterCombinationException

    -- ** CacheParameterGroupAlreadyExistsFault
    , _CacheParameterGroupAlreadyExistsFault

    -- ** APICallRateForCustomerExceededFault
    , _APICallRateForCustomerExceededFault

    -- ** InvalidCacheParameterGroupStateFault
    , _InvalidCacheParameterGroupStateFault

    -- * Waiters
    -- $waiters

    -- ** CacheClusterAvailable
    , cacheClusterAvailable

    -- ** CacheClusterDeleted
    , cacheClusterDeleted

    -- ** ReplicationGroupDeleted
    , replicationGroupDeleted

    -- ** ReplicationGroupAvailable
    , replicationGroupAvailable

    -- * Operations
    -- $operations

    -- ** StartMigration 
    , module Network.AWS.ElastiCache.StartMigration

    -- ** DeleteCacheSecurityGroup 
    , module Network.AWS.ElastiCache.DeleteCacheSecurityGroup

    -- ** CreateReplicationGroup 
    , module Network.AWS.ElastiCache.CreateReplicationGroup

    -- ** DeleteCacheCluster 
    , module Network.AWS.ElastiCache.DeleteCacheCluster

    -- ** IncreaseNodeGroupsInGlobalReplicationGroup 
    , module Network.AWS.ElastiCache.IncreaseNodeGroupsInGlobalReplicationGroup

    -- ** RebootCacheCluster 
    , module Network.AWS.ElastiCache.RebootCacheCluster

    -- ** RevokeCacheSecurityGroupIngress 
    , module Network.AWS.ElastiCache.RevokeCacheSecurityGroupIngress

    -- ** CreateCacheCluster 
    , module Network.AWS.ElastiCache.CreateCacheCluster

    -- ** DescribeEvents (Paginated)
    , module Network.AWS.ElastiCache.DescribeEvents

    -- ** DescribeEngineDefaultParameters (Paginated)
    , module Network.AWS.ElastiCache.DescribeEngineDefaultParameters

    -- ** DisassociateGlobalReplicationGroup 
    , module Network.AWS.ElastiCache.DisassociateGlobalReplicationGroup

    -- ** ModifyCacheParameterGroup 
    , module Network.AWS.ElastiCache.ModifyCacheParameterGroup

    -- ** TestFailover 
    , module Network.AWS.ElastiCache.TestFailover

    -- ** DeleteReplicationGroup 
    , module Network.AWS.ElastiCache.DeleteReplicationGroup

    -- ** ListTagsForResource 
    , module Network.AWS.ElastiCache.ListTagsForResource

    -- ** CompleteMigration 
    , module Network.AWS.ElastiCache.CompleteMigration

    -- ** DescribeCacheClusters (Paginated)
    , module Network.AWS.ElastiCache.DescribeCacheClusters

    -- ** PurchaseReservedCacheNodesOffering 
    , module Network.AWS.ElastiCache.PurchaseReservedCacheNodesOffering

    -- ** RemoveTagsFromResource 
    , module Network.AWS.ElastiCache.RemoveTagsFromResource

    -- ** ModifyReplicationGroup 
    , module Network.AWS.ElastiCache.ModifyReplicationGroup

    -- ** DescribeCacheParameters (Paginated)
    , module Network.AWS.ElastiCache.DescribeCacheParameters

    -- ** DescribeGlobalReplicationGroups (Paginated)
    , module Network.AWS.ElastiCache.DescribeGlobalReplicationGroups

    -- ** DescribeCacheSubnetGroups (Paginated)
    , module Network.AWS.ElastiCache.DescribeCacheSubnetGroups

    -- ** DescribeUpdateActions (Paginated)
    , module Network.AWS.ElastiCache.DescribeUpdateActions

    -- ** RebalanceSlotsInGlobalReplicationGroup 
    , module Network.AWS.ElastiCache.RebalanceSlotsInGlobalReplicationGroup

    -- ** CreateCacheSecurityGroup 
    , module Network.AWS.ElastiCache.CreateCacheSecurityGroup

    -- ** DecreaseReplicaCount 
    , module Network.AWS.ElastiCache.DecreaseReplicaCount

    -- ** AddTagsToResource 
    , module Network.AWS.ElastiCache.AddTagsToResource

    -- ** AuthorizeCacheSecurityGroupIngress 
    , module Network.AWS.ElastiCache.AuthorizeCacheSecurityGroupIngress

    -- ** CopySnapshot 
    , module Network.AWS.ElastiCache.CopySnapshot

    -- ** FailoverGlobalReplicationGroup 
    , module Network.AWS.ElastiCache.FailoverGlobalReplicationGroup

    -- ** CreateCacheSubnetGroup 
    , module Network.AWS.ElastiCache.CreateCacheSubnetGroup

    -- ** CreateGlobalReplicationGroup 
    , module Network.AWS.ElastiCache.CreateGlobalReplicationGroup

    -- ** DescribeCacheParameterGroups (Paginated)
    , module Network.AWS.ElastiCache.DescribeCacheParameterGroups

    -- ** ResetCacheParameterGroup 
    , module Network.AWS.ElastiCache.ResetCacheParameterGroup

    -- ** ListAllowedNodeTypeModifications 
    , module Network.AWS.ElastiCache.ListAllowedNodeTypeModifications

    -- ** IncreaseReplicaCount 
    , module Network.AWS.ElastiCache.IncreaseReplicaCount

    -- ** ModifyReplicationGroupShardConfiguration 
    , module Network.AWS.ElastiCache.ModifyReplicationGroupShardConfiguration

    -- ** BatchApplyUpdateAction 
    , module Network.AWS.ElastiCache.BatchApplyUpdateAction

    -- ** DescribeServiceUpdates (Paginated)
    , module Network.AWS.ElastiCache.DescribeServiceUpdates

    -- ** DescribeSnapshots (Paginated)
    , module Network.AWS.ElastiCache.DescribeSnapshots

    -- ** DescribeReplicationGroups (Paginated)
    , module Network.AWS.ElastiCache.DescribeReplicationGroups

    -- ** DeleteSnapshot 
    , module Network.AWS.ElastiCache.DeleteSnapshot

    -- ** DescribeReservedCacheNodesOfferings (Paginated)
    , module Network.AWS.ElastiCache.DescribeReservedCacheNodesOfferings

    -- ** ModifyCacheSubnetGroup 
    , module Network.AWS.ElastiCache.ModifyCacheSubnetGroup

    -- ** CreateSnapshot 
    , module Network.AWS.ElastiCache.CreateSnapshot

    -- ** ModifyGlobalReplicationGroup 
    , module Network.AWS.ElastiCache.ModifyGlobalReplicationGroup

    -- ** DeleteCacheParameterGroup 
    , module Network.AWS.ElastiCache.DeleteCacheParameterGroup

    -- ** DescribeCacheSecurityGroups (Paginated)
    , module Network.AWS.ElastiCache.DescribeCacheSecurityGroups

    -- ** BatchStopUpdateAction 
    , module Network.AWS.ElastiCache.BatchStopUpdateAction

    -- ** ModifyCacheCluster 
    , module Network.AWS.ElastiCache.ModifyCacheCluster

    -- ** DescribeCacheEngineVersions (Paginated)
    , module Network.AWS.ElastiCache.DescribeCacheEngineVersions

    -- ** CreateCacheParameterGroup 
    , module Network.AWS.ElastiCache.CreateCacheParameterGroup

    -- ** DescribeReservedCacheNodes (Paginated)
    , module Network.AWS.ElastiCache.DescribeReservedCacheNodes

    -- ** DeleteGlobalReplicationGroup 
    , module Network.AWS.ElastiCache.DeleteGlobalReplicationGroup

    -- ** DecreaseNodeGroupsInGlobalReplicationGroup 
    , module Network.AWS.ElastiCache.DecreaseNodeGroupsInGlobalReplicationGroup

    -- ** DeleteCacheSubnetGroup 
    , module Network.AWS.ElastiCache.DeleteCacheSubnetGroup

    -- * Types

    -- ** AZMode
    , AZMode (..)

    -- ** AuthTokenUpdateStatus
    , AuthTokenUpdateStatus (..)

    -- ** AuthTokenUpdateStrategyType
    , AuthTokenUpdateStrategyType (..)

    -- ** AutomaticFailoverStatus
    , AutomaticFailoverStatus (..)

    -- ** ChangeType
    , ChangeType (..)

    -- ** NodeUpdateInitiatedBy
    , NodeUpdateInitiatedBy (..)

    -- ** NodeUpdateStatus
    , NodeUpdateStatus (..)

    -- ** PendingAutomaticFailoverStatus
    , PendingAutomaticFailoverStatus (..)

    -- ** ServiceUpdateSeverity
    , ServiceUpdateSeverity (..)

    -- ** ServiceUpdateStatus
    , ServiceUpdateStatus (..)

    -- ** ServiceUpdateType
    , ServiceUpdateType (..)

    -- ** SlaMet
    , SlaMet (..)

    -- ** SourceType
    , SourceType (..)

    -- ** UpdateActionStatus
    , UpdateActionStatus (..)

    -- ** AvailabilityZone
    , AvailabilityZone
    , availabilityZone
    , azName

    -- ** CacheCluster
    , CacheCluster
    , cacheCluster
    , ccAuthTokenLastModifiedDate
    , ccEngineVersion
    , ccCacheNodeType
    , ccCacheNodes
    , ccCacheClusterCreateTime
    , ccAtRestEncryptionEnabled
    , ccAutoMinorVersionUpgrade
    , ccSecurityGroups
    , ccNotificationConfiguration
    , ccTransitEncryptionEnabled
    , ccSnapshotWindow
    , ccCacheClusterId
    , ccConfigurationEndpoint
    , ccEngine
    , ccCacheSecurityGroups
    , ccAuthTokenEnabled
    , ccClientDownloadLandingPage
    , ccPreferredMaintenanceWindow
    , ccCacheSubnetGroupName
    , ccPreferredAvailabilityZone
    , ccCacheParameterGroup
    , ccCacheClusterStatus
    , ccSnapshotRetentionLimit
    , ccReplicationGroupId
    , ccPendingModifiedValues
    , ccNumCacheNodes

    -- ** CacheEngineVersion
    , CacheEngineVersion
    , cacheEngineVersion
    , cevEngineVersion
    , cevCacheParameterGroupFamily
    , cevCacheEngineDescription
    , cevEngine
    , cevCacheEngineVersionDescription

    -- ** CacheNode
    , CacheNode
    , cacheNode
    , cnSourceCacheNodeId
    , cnParameterGroupStatus
    , cnCacheNodeCreateTime
    , cnCustomerAvailabilityZone
    , cnCacheNodeId
    , cnCacheNodeStatus
    , cnEndpoint

    -- ** CacheNodeTypeSpecificParameter
    , CacheNodeTypeSpecificParameter
    , cacheNodeTypeSpecificParameter
    , cntspCacheNodeTypeSpecificValues
    , cntspMinimumEngineVersion
    , cntspSource
    , cntspIsModifiable
    , cntspDataType
    , cntspAllowedValues
    , cntspParameterName
    , cntspDescription
    , cntspChangeType

    -- ** CacheNodeTypeSpecificValue
    , CacheNodeTypeSpecificValue
    , cacheNodeTypeSpecificValue
    , cntsvCacheNodeType
    , cntsvValue

    -- ** CacheNodeUpdateStatus
    , CacheNodeUpdateStatus
    , cacheNodeUpdateStatus
    , cnusNodeUpdateEndDate
    , cnusNodeUpdateInitiatedBy
    , cnusNodeUpdateStatusModifiedDate
    , cnusCacheNodeId
    , cnusNodeUpdateInitiatedDate
    , cnusNodeUpdateStartDate
    , cnusNodeUpdateStatus
    , cnusNodeDeletionDate

    -- ** CacheParameterGroup
    , CacheParameterGroup
    , cacheParameterGroup
    , cpgCacheParameterGroupFamily
    , cpgCacheParameterGroupName
    , cpgIsGlobal
    , cpgDescription

    -- ** CacheParameterGroupNameMessage
    , CacheParameterGroupNameMessage
    , cacheParameterGroupNameMessage
    , cpgnmCacheParameterGroupName

    -- ** CacheParameterGroupStatus
    , CacheParameterGroupStatus
    , cacheParameterGroupStatus
    , cpgsCacheParameterGroupName
    , cpgsCacheNodeIdsToReboot
    , cpgsParameterApplyStatus

    -- ** CacheSecurityGroup
    , CacheSecurityGroup
    , cacheSecurityGroup
    , csgCacheSecurityGroupName
    , csgOwnerId
    , csgEC2SecurityGroups
    , csgDescription

    -- ** CacheSecurityGroupMembership
    , CacheSecurityGroupMembership
    , cacheSecurityGroupMembership
    , csgmStatus
    , csgmCacheSecurityGroupName

    -- ** CacheSubnetGroup
    , CacheSubnetGroup
    , cacheSubnetGroup
    , csgVPCId
    , csgSubnets
    , csgCacheSubnetGroupName
    , csgCacheSubnetGroupDescription

    -- ** ConfigureShard
    , ConfigureShard
    , configureShard
    , csPreferredAvailabilityZones
    , csNodeGroupId
    , csNewReplicaCount

    -- ** CustomerNodeEndpoint
    , CustomerNodeEndpoint
    , customerNodeEndpoint
    , cneAddress
    , cnePort

    -- ** EC2SecurityGroup
    , EC2SecurityGroup
    , ec2SecurityGroup
    , esgStatus
    , esgEC2SecurityGroupOwnerId
    , esgEC2SecurityGroupName

    -- ** Endpoint
    , Endpoint
    , endpoint
    , eAddress
    , ePort

    -- ** EngineDefaults
    , EngineDefaults
    , engineDefaults
    , edCacheParameterGroupFamily
    , edCacheNodeTypeSpecificParameters
    , edMarker
    , edParameters

    -- ** Event
    , Event
    , event
    , eSourceType
    , eSourceIdentifier
    , eDate
    , eMessage

    -- ** GlobalNodeGroup
    , GlobalNodeGroup
    , globalNodeGroup
    , gngSlots
    , gngGlobalNodeGroupId

    -- ** GlobalReplicationGroup
    , GlobalReplicationGroup
    , globalReplicationGroup
    , grgEngineVersion
    , grgStatus
    , grgCacheNodeType
    , grgClusterEnabled
    , grgAtRestEncryptionEnabled
    , grgTransitEncryptionEnabled
    , grgMembers
    , grgEngine
    , grgAuthTokenEnabled
    , grgGlobalNodeGroups
    , grgGlobalReplicationGroupId
    , grgGlobalReplicationGroupDescription

    -- ** GlobalReplicationGroupInfo
    , GlobalReplicationGroupInfo
    , globalReplicationGroupInfo
    , grgiGlobalReplicationGroupMemberRole
    , grgiGlobalReplicationGroupId

    -- ** GlobalReplicationGroupMember
    , GlobalReplicationGroupMember
    , globalReplicationGroupMember
    , grgmStatus
    , grgmReplicationGroupRegion
    , grgmRole
    , grgmReplicationGroupId
    , grgmAutomaticFailover

    -- ** NodeGroup
    , NodeGroup
    , nodeGroup
    , ngStatus
    , ngPrimaryEndpoint
    , ngSlots
    , ngNodeGroupMembers
    , ngNodeGroupId
    , ngReaderEndpoint

    -- ** NodeGroupConfiguration
    , NodeGroupConfiguration
    , nodeGroupConfiguration
    , ngcSlots
    , ngcReplicaCount
    , ngcPrimaryAvailabilityZone
    , ngcReplicaAvailabilityZones
    , ngcNodeGroupId

    -- ** NodeGroupMember
    , NodeGroupMember
    , nodeGroupMember
    , ngmCacheClusterId
    , ngmCacheNodeId
    , ngmPreferredAvailabilityZone
    , ngmCurrentRole
    , ngmReadEndpoint

    -- ** NodeGroupMemberUpdateStatus
    , NodeGroupMemberUpdateStatus
    , nodeGroupMemberUpdateStatus
    , ngmusNodeUpdateEndDate
    , ngmusNodeUpdateInitiatedBy
    , ngmusNodeUpdateStatusModifiedDate
    , ngmusCacheClusterId
    , ngmusCacheNodeId
    , ngmusNodeUpdateInitiatedDate
    , ngmusNodeUpdateStartDate
    , ngmusNodeUpdateStatus
    , ngmusNodeDeletionDate

    -- ** NodeGroupUpdateStatus
    , NodeGroupUpdateStatus
    , nodeGroupUpdateStatus
    , ngusNodeGroupMemberUpdateStatus
    , ngusNodeGroupId

    -- ** NodeSnapshot
    , NodeSnapshot
    , nodeSnapshot
    , nsNodeGroupConfiguration
    , nsCacheNodeCreateTime
    , nsCacheClusterId
    , nsCacheNodeId
    , nsNodeGroupId
    , nsSnapshotCreateTime
    , nsCacheSize

    -- ** NotificationConfiguration
    , NotificationConfiguration
    , notificationConfiguration
    , ncTopicStatus
    , ncTopicARN

    -- ** Parameter
    , Parameter
    , parameter
    , pParameterValue
    , pMinimumEngineVersion
    , pSource
    , pIsModifiable
    , pDataType
    , pAllowedValues
    , pParameterName
    , pDescription
    , pChangeType

    -- ** ParameterNameValue
    , ParameterNameValue
    , parameterNameValue
    , pnvParameterValue
    , pnvParameterName

    -- ** PendingModifiedValues
    , PendingModifiedValues
    , pendingModifiedValues
    , pmvEngineVersion
    , pmvCacheNodeType
    , pmvAuthTokenStatus
    , pmvCacheNodeIdsToRemove
    , pmvNumCacheNodes

    -- ** ProcessedUpdateAction
    , ProcessedUpdateAction
    , processedUpdateAction
    , puaCacheClusterId
    , puaServiceUpdateName
    , puaUpdateActionStatus
    , puaReplicationGroupId

    -- ** RecurringCharge
    , RecurringCharge
    , recurringCharge
    , rcRecurringChargeFrequency
    , rcRecurringChargeAmount

    -- ** RegionalConfiguration
    , RegionalConfiguration
    , regionalConfiguration
    , rcReplicationGroupId
    , rcReplicationGroupRegion
    , rcReshardingConfiguration

    -- ** ReplicationGroup
    , ReplicationGroup
    , replicationGroup
    , rgAuthTokenLastModifiedDate
    , rgStatus
    , rgCacheNodeType
    , rgNodeGroups
    , rgSnapshottingClusterId
    , rgClusterEnabled
    , rgAtRestEncryptionEnabled
    , rgTransitEncryptionEnabled
    , rgSnapshotWindow
    , rgConfigurationEndpoint
    , rgAuthTokenEnabled
    , rgMemberClusters
    , rgKMSKeyId
    , rgSnapshotRetentionLimit
    , rgDescription
    , rgReplicationGroupId
    , rgPendingModifiedValues
    , rgGlobalReplicationGroupInfo
    , rgAutomaticFailover

    -- ** ReplicationGroupPendingModifiedValues
    , ReplicationGroupPendingModifiedValues
    , replicationGroupPendingModifiedValues
    , rgpmvAuthTokenStatus
    , rgpmvResharding
    , rgpmvPrimaryClusterId
    , rgpmvAutomaticFailoverStatus

    -- ** ReservedCacheNode
    , ReservedCacheNode
    , reservedCacheNode
    , rcnCacheNodeType
    , rcnState
    , rcnStartTime
    , rcnProductDescription
    , rcnReservationARN
    , rcnCacheNodeCount
    , rcnReservedCacheNodeId
    , rcnRecurringCharges
    , rcnOfferingType
    , rcnUsagePrice
    , rcnFixedPrice
    , rcnDuration
    , rcnReservedCacheNodesOfferingId

    -- ** ReservedCacheNodesOffering
    , ReservedCacheNodesOffering
    , reservedCacheNodesOffering
    , rcnoCacheNodeType
    , rcnoProductDescription
    , rcnoRecurringCharges
    , rcnoOfferingType
    , rcnoUsagePrice
    , rcnoFixedPrice
    , rcnoDuration
    , rcnoReservedCacheNodesOfferingId

    -- ** ReshardingConfiguration
    , ReshardingConfiguration
    , reshardingConfiguration
    , rcPreferredAvailabilityZones
    , rcNodeGroupId

    -- ** ReshardingStatus
    , ReshardingStatus
    , reshardingStatus
    , rsSlotMigration

    -- ** SecurityGroupMembership
    , SecurityGroupMembership
    , securityGroupMembership
    , sgmStatus
    , sgmSecurityGroupId

    -- ** ServiceUpdate
    , ServiceUpdate
    , serviceUpdate
    , suEngineVersion
    , suServiceUpdateType
    , suServiceUpdateName
    , suEngine
    , suServiceUpdateReleaseDate
    , suAutoUpdateAfterRecommendedApplyByDate
    , suServiceUpdateSeverity
    , suServiceUpdateEndDate
    , suServiceUpdateDescription
    , suServiceUpdateRecommendedApplyByDate
    , suServiceUpdateStatus
    , suEstimatedUpdateTime

    -- ** SlotMigration
    , SlotMigration
    , slotMigration
    , smProgressPercentage

    -- ** Snapshot
    , Snapshot
    , snapshot
    , sEngineVersion
    , sCacheNodeType
    , sCacheClusterCreateTime
    , sAutoMinorVersionUpgrade
    , sCacheParameterGroupName
    , sReplicationGroupDescription
    , sVPCId
    , sSnapshotStatus
    , sSnapshotWindow
    , sCacheClusterId
    , sEngine
    , sPreferredMaintenanceWindow
    , sTopicARN
    , sKMSKeyId
    , sNodeSnapshots
    , sCacheSubnetGroupName
    , sPreferredAvailabilityZone
    , sNumNodeGroups
    , sSnapshotRetentionLimit
    , sSnapshotName
    , sReplicationGroupId
    , sNumCacheNodes
    , sPort
    , sAutomaticFailover
    , sSnapshotSource

    -- ** Subnet
    , Subnet
    , subnet
    , sSubnetIdentifier
    , sSubnetAvailabilityZone

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- ** TagListMessage
    , TagListMessage
    , tagListMessage
    , tlmTagList

    -- ** TimeRangeFilter
    , TimeRangeFilter
    , timeRangeFilter
    , trfStartTime
    , trfEndTime

    -- ** UnprocessedUpdateAction
    , UnprocessedUpdateAction
    , unprocessedUpdateAction
    , uuaCacheClusterId
    , uuaServiceUpdateName
    , uuaErrorType
    , uuaErrorMessage
    , uuaReplicationGroupId

    -- ** UpdateAction
    , UpdateAction
    , updateAction
    , uaServiceUpdateType
    , uaSlaMet
    , uaCacheClusterId
    , uaServiceUpdateName
    , uaUpdateActionStatus
    , uaEngine
    , uaNodesUpdated
    , uaUpdateActionStatusModifiedDate
    , uaServiceUpdateReleaseDate
    , uaCacheNodeUpdateStatus
    , uaServiceUpdateSeverity
    , uaNodeGroupUpdateStatus
    , uaServiceUpdateRecommendedApplyByDate
    , uaUpdateActionAvailableDate
    , uaServiceUpdateStatus
    , uaEstimatedUpdateTime
    , uaReplicationGroupId

    -- ** UpdateActionResultsMessage
    , UpdateActionResultsMessage
    , updateActionResultsMessage
    , uarmUnprocessedUpdateActions
    , uarmProcessedUpdateActions
    ) where

import Network.AWS.ElastiCache.AddTagsToResource
import Network.AWS.ElastiCache.AuthorizeCacheSecurityGroupIngress
import Network.AWS.ElastiCache.BatchApplyUpdateAction
import Network.AWS.ElastiCache.BatchStopUpdateAction
import Network.AWS.ElastiCache.CompleteMigration
import Network.AWS.ElastiCache.CopySnapshot
import Network.AWS.ElastiCache.CreateCacheCluster
import Network.AWS.ElastiCache.CreateCacheParameterGroup
import Network.AWS.ElastiCache.CreateCacheSecurityGroup
import Network.AWS.ElastiCache.CreateCacheSubnetGroup
import Network.AWS.ElastiCache.CreateGlobalReplicationGroup
import Network.AWS.ElastiCache.CreateReplicationGroup
import Network.AWS.ElastiCache.CreateSnapshot
import Network.AWS.ElastiCache.DecreaseNodeGroupsInGlobalReplicationGroup
import Network.AWS.ElastiCache.DecreaseReplicaCount
import Network.AWS.ElastiCache.DeleteCacheCluster
import Network.AWS.ElastiCache.DeleteCacheParameterGroup
import Network.AWS.ElastiCache.DeleteCacheSecurityGroup
import Network.AWS.ElastiCache.DeleteCacheSubnetGroup
import Network.AWS.ElastiCache.DeleteGlobalReplicationGroup
import Network.AWS.ElastiCache.DeleteReplicationGroup
import Network.AWS.ElastiCache.DeleteSnapshot
import Network.AWS.ElastiCache.DescribeCacheClusters
import Network.AWS.ElastiCache.DescribeCacheEngineVersions
import Network.AWS.ElastiCache.DescribeCacheParameterGroups
import Network.AWS.ElastiCache.DescribeCacheParameters
import Network.AWS.ElastiCache.DescribeCacheSecurityGroups
import Network.AWS.ElastiCache.DescribeCacheSubnetGroups
import Network.AWS.ElastiCache.DescribeEngineDefaultParameters
import Network.AWS.ElastiCache.DescribeEvents
import Network.AWS.ElastiCache.DescribeGlobalReplicationGroups
import Network.AWS.ElastiCache.DescribeReplicationGroups
import Network.AWS.ElastiCache.DescribeReservedCacheNodes
import Network.AWS.ElastiCache.DescribeReservedCacheNodesOfferings
import Network.AWS.ElastiCache.DescribeServiceUpdates
import Network.AWS.ElastiCache.DescribeSnapshots
import Network.AWS.ElastiCache.DescribeUpdateActions
import Network.AWS.ElastiCache.DisassociateGlobalReplicationGroup
import Network.AWS.ElastiCache.FailoverGlobalReplicationGroup
import Network.AWS.ElastiCache.IncreaseNodeGroupsInGlobalReplicationGroup
import Network.AWS.ElastiCache.IncreaseReplicaCount
import Network.AWS.ElastiCache.ListAllowedNodeTypeModifications
import Network.AWS.ElastiCache.ListTagsForResource
import Network.AWS.ElastiCache.ModifyCacheCluster
import Network.AWS.ElastiCache.ModifyCacheParameterGroup
import Network.AWS.ElastiCache.ModifyCacheSubnetGroup
import Network.AWS.ElastiCache.ModifyGlobalReplicationGroup
import Network.AWS.ElastiCache.ModifyReplicationGroup
import Network.AWS.ElastiCache.ModifyReplicationGroupShardConfiguration
import Network.AWS.ElastiCache.PurchaseReservedCacheNodesOffering
import Network.AWS.ElastiCache.RebalanceSlotsInGlobalReplicationGroup
import Network.AWS.ElastiCache.RebootCacheCluster
import Network.AWS.ElastiCache.RemoveTagsFromResource
import Network.AWS.ElastiCache.ResetCacheParameterGroup
import Network.AWS.ElastiCache.RevokeCacheSecurityGroupIngress
import Network.AWS.ElastiCache.StartMigration
import Network.AWS.ElastiCache.TestFailover
import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'ElastiCache'.
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

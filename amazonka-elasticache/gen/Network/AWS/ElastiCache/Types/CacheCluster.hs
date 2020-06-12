{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CacheCluster
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.CacheCluster where

import Network.AWS.ElastiCache.Types.CacheNode
import Network.AWS.ElastiCache.Types.CacheParameterGroupStatus
import Network.AWS.ElastiCache.Types.CacheSecurityGroupMembership
import Network.AWS.ElastiCache.Types.Endpoint
import Network.AWS.ElastiCache.Types.NotificationConfiguration
import Network.AWS.ElastiCache.Types.PendingModifiedValues
import Network.AWS.ElastiCache.Types.SecurityGroupMembership
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains all of the attributes of a specific cluster.
--
--
--
-- /See:/ 'cacheCluster' smart constructor.
data CacheCluster = CacheCluster'{_ccEngineVersion ::
                                  !(Maybe Text),
                                  _ccCacheNodeType :: !(Maybe Text),
                                  _ccCacheNodes :: !(Maybe [CacheNode]),
                                  _ccCacheClusterCreateTime :: !(Maybe ISO8601),
                                  _ccAtRestEncryptionEnabled :: !(Maybe Bool),
                                  _ccAutoMinorVersionUpgrade :: !(Maybe Bool),
                                  _ccSecurityGroups ::
                                  !(Maybe [SecurityGroupMembership]),
                                  _ccNotificationConfiguration ::
                                  !(Maybe NotificationConfiguration),
                                  _ccTransitEncryptionEnabled :: !(Maybe Bool),
                                  _ccSnapshotWindow :: !(Maybe Text),
                                  _ccCacheClusterId :: !(Maybe Text),
                                  _ccConfigurationEndpoint :: !(Maybe Endpoint),
                                  _ccEngine :: !(Maybe Text),
                                  _ccCacheSecurityGroups ::
                                  !(Maybe [CacheSecurityGroupMembership]),
                                  _ccAuthTokenEnabled :: !(Maybe Bool),
                                  _ccClientDownloadLandingPage :: !(Maybe Text),
                                  _ccPreferredMaintenanceWindow ::
                                  !(Maybe Text),
                                  _ccCacheSubnetGroupName :: !(Maybe Text),
                                  _ccPreferredAvailabilityZone :: !(Maybe Text),
                                  _ccCacheParameterGroup ::
                                  !(Maybe CacheParameterGroupStatus),
                                  _ccCacheClusterStatus :: !(Maybe Text),
                                  _ccSnapshotRetentionLimit :: !(Maybe Int),
                                  _ccReplicationGroupId :: !(Maybe Text),
                                  _ccPendingModifiedValues ::
                                  !(Maybe PendingModifiedValues),
                                  _ccNumCacheNodes :: !(Maybe Int)}
                      deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CacheCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccEngineVersion' - The version of the cache engine that is used in this cluster.
--
-- * 'ccCacheNodeType' - The name of the compute and memory capacity node type for the cluster. The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.     * General purpose:     * Current generation:  __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@  __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@  __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@      * Previous generation: (not recommended) __T1 node types:__ @cache.t1.micro@  __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@      * Compute optimized:     * Previous generation: (not recommended) __C1 node types:__ @cache.c1.xlarge@      * Memory optimized:     * Current generation:  __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@      * Previous generation: (not recommended) __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@  __Notes:__      * All T2 instances are created in an Amazon Virtual Private Cloud (Amazon VPC).     * Redis (cluster mode disabled): Redis backup/restore is not supported on T1 and T2 instances.      * Redis (cluster mode enabled): Backup/restore is not supported on T1 instances.     * Redis Append-only files (AOF) functionality is not supported for T1 or T2 instances. For a complete listing of node types and specifications, see <http://aws.amazon.com/elasticache/details Amazon ElastiCache Product Features and Details> and either <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Memcached.html#ParameterGroups.Memcached.NodeSpecific Cache Node Type-Specific Parameters for Memcached> or <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Redis.html#ParameterGroups.Redis.NodeSpecific Cache Node Type-Specific Parameters for Redis> .
--
-- * 'ccCacheNodes' - A list of cache nodes that are members of the cluster.
--
-- * 'ccCacheClusterCreateTime' - The date and time when the cluster was created.
--
-- * 'ccAtRestEncryptionEnabled' - A flag that enables encryption at-rest when set to @true@ . You cannot modify the value of @AtRestEncryptionEnabled@ after the cluster is created. To enable at-rest encryption on a cluster you must set @AtRestEncryptionEnabled@ to @true@ when you create a cluster. Default: @false@ 
--
-- * 'ccAutoMinorVersionUpgrade' - This parameter is currently disabled.
--
-- * 'ccSecurityGroups' - A list of VPC Security Groups associated with the cluster.
--
-- * 'ccNotificationConfiguration' - Describes a notification topic and its status. Notification topics are used for publishing ElastiCache events to subscribers using Amazon Simple Notification Service (SNS). 
--
-- * 'ccTransitEncryptionEnabled' - A flag that enables in-transit encryption when set to @true@ . You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to @true@ when you create a cluster. Default: @false@ 
--
-- * 'ccSnapshotWindow' - The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your cluster. Example: @05:00-09:00@ 
--
-- * 'ccCacheClusterId' - The user-supplied identifier of the cluster. This identifier is a unique key that identifies a cluster.
--
-- * 'ccConfigurationEndpoint' - Represents a Memcached cluster endpoint which, if Automatic Discovery is enabled on the cluster, can be used by an application to connect to any node in the cluster. The configuration endpoint will always have @.cfg@ in it. Example: @mem-3.9dvc4r/.cfg/ .usw2.cache.amazonaws.com:11211@ 
--
-- * 'ccEngine' - The name of the cache engine (@memcached@ or @redis@ ) to be used for this cluster.
--
-- * 'ccCacheSecurityGroups' - A list of cache security group elements, composed of name and status sub-elements.
--
-- * 'ccAuthTokenEnabled' - A flag that enables using an @AuthToken@ (password) when issuing Redis commands. Default: @false@ 
--
-- * 'ccClientDownloadLandingPage' - The URL of the web page where you can download the latest ElastiCache client library.
--
-- * 'ccPreferredMaintenanceWindow' - Specifies the weekly time range during which maintenance on the cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period. Valid values for @ddd@ are:     * @sun@      * @mon@      * @tue@      * @wed@      * @thu@      * @fri@      * @sat@  Example: @sun:23:00-mon:01:30@ 
--
-- * 'ccCacheSubnetGroupName' - The name of the cache subnet group associated with the cluster.
--
-- * 'ccPreferredAvailabilityZone' - The name of the Availability Zone in which the cluster is located or "Multiple" if the cache nodes are located in different Availability Zones.
--
-- * 'ccCacheParameterGroup' - Status of the cache parameter group.
--
-- * 'ccCacheClusterStatus' - The current state of this cluster, one of the following values: @available@ , @creating@ , @deleted@ , @deleting@ , @incompatible-network@ , @modifying@ , @rebooting cluster nodes@ , @restore-failed@ , or @snapshotting@ .
--
-- * 'ccSnapshotRetentionLimit' - The number of days for which ElastiCache retains automatic cluster snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted. /Important:/ If the value of SnapshotRetentionLimit is set to zero (0), backups are turned off.
--
-- * 'ccReplicationGroupId' - The replication group to which this cluster belongs. If this field is empty, the cluster is not associated with any replication group.
--
-- * 'ccPendingModifiedValues' - Undocumented member.
--
-- * 'ccNumCacheNodes' - The number of cache nodes in the cluster. For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
cacheCluster
    :: CacheCluster
cacheCluster
  = CacheCluster'{_ccEngineVersion = Nothing,
                  _ccCacheNodeType = Nothing, _ccCacheNodes = Nothing,
                  _ccCacheClusterCreateTime = Nothing,
                  _ccAtRestEncryptionEnabled = Nothing,
                  _ccAutoMinorVersionUpgrade = Nothing,
                  _ccSecurityGroups = Nothing,
                  _ccNotificationConfiguration = Nothing,
                  _ccTransitEncryptionEnabled = Nothing,
                  _ccSnapshotWindow = Nothing,
                  _ccCacheClusterId = Nothing,
                  _ccConfigurationEndpoint = Nothing,
                  _ccEngine = Nothing,
                  _ccCacheSecurityGroups = Nothing,
                  _ccAuthTokenEnabled = Nothing,
                  _ccClientDownloadLandingPage = Nothing,
                  _ccPreferredMaintenanceWindow = Nothing,
                  _ccCacheSubnetGroupName = Nothing,
                  _ccPreferredAvailabilityZone = Nothing,
                  _ccCacheParameterGroup = Nothing,
                  _ccCacheClusterStatus = Nothing,
                  _ccSnapshotRetentionLimit = Nothing,
                  _ccReplicationGroupId = Nothing,
                  _ccPendingModifiedValues = Nothing,
                  _ccNumCacheNodes = Nothing}

-- | The version of the cache engine that is used in this cluster.
ccEngineVersion :: Lens' CacheCluster (Maybe Text)
ccEngineVersion = lens _ccEngineVersion (\ s a -> s{_ccEngineVersion = a})

-- | The name of the compute and memory capacity node type for the cluster. The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.     * General purpose:     * Current generation:  __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@  __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@  __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@      * Previous generation: (not recommended) __T1 node types:__ @cache.t1.micro@  __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@      * Compute optimized:     * Previous generation: (not recommended) __C1 node types:__ @cache.c1.xlarge@      * Memory optimized:     * Current generation:  __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@      * Previous generation: (not recommended) __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@  __Notes:__      * All T2 instances are created in an Amazon Virtual Private Cloud (Amazon VPC).     * Redis (cluster mode disabled): Redis backup/restore is not supported on T1 and T2 instances.      * Redis (cluster mode enabled): Backup/restore is not supported on T1 instances.     * Redis Append-only files (AOF) functionality is not supported for T1 or T2 instances. For a complete listing of node types and specifications, see <http://aws.amazon.com/elasticache/details Amazon ElastiCache Product Features and Details> and either <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Memcached.html#ParameterGroups.Memcached.NodeSpecific Cache Node Type-Specific Parameters for Memcached> or <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Redis.html#ParameterGroups.Redis.NodeSpecific Cache Node Type-Specific Parameters for Redis> .
ccCacheNodeType :: Lens' CacheCluster (Maybe Text)
ccCacheNodeType = lens _ccCacheNodeType (\ s a -> s{_ccCacheNodeType = a})

-- | A list of cache nodes that are members of the cluster.
ccCacheNodes :: Lens' CacheCluster [CacheNode]
ccCacheNodes = lens _ccCacheNodes (\ s a -> s{_ccCacheNodes = a}) . _Default . _Coerce

-- | The date and time when the cluster was created.
ccCacheClusterCreateTime :: Lens' CacheCluster (Maybe UTCTime)
ccCacheClusterCreateTime = lens _ccCacheClusterCreateTime (\ s a -> s{_ccCacheClusterCreateTime = a}) . mapping _Time

-- | A flag that enables encryption at-rest when set to @true@ . You cannot modify the value of @AtRestEncryptionEnabled@ after the cluster is created. To enable at-rest encryption on a cluster you must set @AtRestEncryptionEnabled@ to @true@ when you create a cluster. Default: @false@ 
ccAtRestEncryptionEnabled :: Lens' CacheCluster (Maybe Bool)
ccAtRestEncryptionEnabled = lens _ccAtRestEncryptionEnabled (\ s a -> s{_ccAtRestEncryptionEnabled = a})

-- | This parameter is currently disabled.
ccAutoMinorVersionUpgrade :: Lens' CacheCluster (Maybe Bool)
ccAutoMinorVersionUpgrade = lens _ccAutoMinorVersionUpgrade (\ s a -> s{_ccAutoMinorVersionUpgrade = a})

-- | A list of VPC Security Groups associated with the cluster.
ccSecurityGroups :: Lens' CacheCluster [SecurityGroupMembership]
ccSecurityGroups = lens _ccSecurityGroups (\ s a -> s{_ccSecurityGroups = a}) . _Default . _Coerce

-- | Describes a notification topic and its status. Notification topics are used for publishing ElastiCache events to subscribers using Amazon Simple Notification Service (SNS). 
ccNotificationConfiguration :: Lens' CacheCluster (Maybe NotificationConfiguration)
ccNotificationConfiguration = lens _ccNotificationConfiguration (\ s a -> s{_ccNotificationConfiguration = a})

-- | A flag that enables in-transit encryption when set to @true@ . You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to @true@ when you create a cluster. Default: @false@ 
ccTransitEncryptionEnabled :: Lens' CacheCluster (Maybe Bool)
ccTransitEncryptionEnabled = lens _ccTransitEncryptionEnabled (\ s a -> s{_ccTransitEncryptionEnabled = a})

-- | The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your cluster. Example: @05:00-09:00@ 
ccSnapshotWindow :: Lens' CacheCluster (Maybe Text)
ccSnapshotWindow = lens _ccSnapshotWindow (\ s a -> s{_ccSnapshotWindow = a})

-- | The user-supplied identifier of the cluster. This identifier is a unique key that identifies a cluster.
ccCacheClusterId :: Lens' CacheCluster (Maybe Text)
ccCacheClusterId = lens _ccCacheClusterId (\ s a -> s{_ccCacheClusterId = a})

-- | Represents a Memcached cluster endpoint which, if Automatic Discovery is enabled on the cluster, can be used by an application to connect to any node in the cluster. The configuration endpoint will always have @.cfg@ in it. Example: @mem-3.9dvc4r/.cfg/ .usw2.cache.amazonaws.com:11211@ 
ccConfigurationEndpoint :: Lens' CacheCluster (Maybe Endpoint)
ccConfigurationEndpoint = lens _ccConfigurationEndpoint (\ s a -> s{_ccConfigurationEndpoint = a})

-- | The name of the cache engine (@memcached@ or @redis@ ) to be used for this cluster.
ccEngine :: Lens' CacheCluster (Maybe Text)
ccEngine = lens _ccEngine (\ s a -> s{_ccEngine = a})

-- | A list of cache security group elements, composed of name and status sub-elements.
ccCacheSecurityGroups :: Lens' CacheCluster [CacheSecurityGroupMembership]
ccCacheSecurityGroups = lens _ccCacheSecurityGroups (\ s a -> s{_ccCacheSecurityGroups = a}) . _Default . _Coerce

-- | A flag that enables using an @AuthToken@ (password) when issuing Redis commands. Default: @false@ 
ccAuthTokenEnabled :: Lens' CacheCluster (Maybe Bool)
ccAuthTokenEnabled = lens _ccAuthTokenEnabled (\ s a -> s{_ccAuthTokenEnabled = a})

-- | The URL of the web page where you can download the latest ElastiCache client library.
ccClientDownloadLandingPage :: Lens' CacheCluster (Maybe Text)
ccClientDownloadLandingPage = lens _ccClientDownloadLandingPage (\ s a -> s{_ccClientDownloadLandingPage = a})

-- | Specifies the weekly time range during which maintenance on the cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period. Valid values for @ddd@ are:     * @sun@      * @mon@      * @tue@      * @wed@      * @thu@      * @fri@      * @sat@  Example: @sun:23:00-mon:01:30@ 
ccPreferredMaintenanceWindow :: Lens' CacheCluster (Maybe Text)
ccPreferredMaintenanceWindow = lens _ccPreferredMaintenanceWindow (\ s a -> s{_ccPreferredMaintenanceWindow = a})

-- | The name of the cache subnet group associated with the cluster.
ccCacheSubnetGroupName :: Lens' CacheCluster (Maybe Text)
ccCacheSubnetGroupName = lens _ccCacheSubnetGroupName (\ s a -> s{_ccCacheSubnetGroupName = a})

-- | The name of the Availability Zone in which the cluster is located or "Multiple" if the cache nodes are located in different Availability Zones.
ccPreferredAvailabilityZone :: Lens' CacheCluster (Maybe Text)
ccPreferredAvailabilityZone = lens _ccPreferredAvailabilityZone (\ s a -> s{_ccPreferredAvailabilityZone = a})

-- | Status of the cache parameter group.
ccCacheParameterGroup :: Lens' CacheCluster (Maybe CacheParameterGroupStatus)
ccCacheParameterGroup = lens _ccCacheParameterGroup (\ s a -> s{_ccCacheParameterGroup = a})

-- | The current state of this cluster, one of the following values: @available@ , @creating@ , @deleted@ , @deleting@ , @incompatible-network@ , @modifying@ , @rebooting cluster nodes@ , @restore-failed@ , or @snapshotting@ .
ccCacheClusterStatus :: Lens' CacheCluster (Maybe Text)
ccCacheClusterStatus = lens _ccCacheClusterStatus (\ s a -> s{_ccCacheClusterStatus = a})

-- | The number of days for which ElastiCache retains automatic cluster snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted. /Important:/ If the value of SnapshotRetentionLimit is set to zero (0), backups are turned off.
ccSnapshotRetentionLimit :: Lens' CacheCluster (Maybe Int)
ccSnapshotRetentionLimit = lens _ccSnapshotRetentionLimit (\ s a -> s{_ccSnapshotRetentionLimit = a})

-- | The replication group to which this cluster belongs. If this field is empty, the cluster is not associated with any replication group.
ccReplicationGroupId :: Lens' CacheCluster (Maybe Text)
ccReplicationGroupId = lens _ccReplicationGroupId (\ s a -> s{_ccReplicationGroupId = a})

-- | Undocumented member.
ccPendingModifiedValues :: Lens' CacheCluster (Maybe PendingModifiedValues)
ccPendingModifiedValues = lens _ccPendingModifiedValues (\ s a -> s{_ccPendingModifiedValues = a})

-- | The number of cache nodes in the cluster. For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
ccNumCacheNodes :: Lens' CacheCluster (Maybe Int)
ccNumCacheNodes = lens _ccNumCacheNodes (\ s a -> s{_ccNumCacheNodes = a})

instance FromXML CacheCluster where
        parseXML x
          = CacheCluster' <$>
              (x .@? "EngineVersion") <*> (x .@? "CacheNodeType")
                <*>
                (x .@? "CacheNodes" .!@ mempty >>=
                   may (parseXMLList "CacheNode"))
                <*> (x .@? "CacheClusterCreateTime")
                <*> (x .@? "AtRestEncryptionEnabled")
                <*> (x .@? "AutoMinorVersionUpgrade")
                <*>
                (x .@? "SecurityGroups" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "NotificationConfiguration")
                <*> (x .@? "TransitEncryptionEnabled")
                <*> (x .@? "SnapshotWindow")
                <*> (x .@? "CacheClusterId")
                <*> (x .@? "ConfigurationEndpoint")
                <*> (x .@? "Engine")
                <*>
                (x .@? "CacheSecurityGroups" .!@ mempty >>=
                   may (parseXMLList "CacheSecurityGroup"))
                <*> (x .@? "AuthTokenEnabled")
                <*> (x .@? "ClientDownloadLandingPage")
                <*> (x .@? "PreferredMaintenanceWindow")
                <*> (x .@? "CacheSubnetGroupName")
                <*> (x .@? "PreferredAvailabilityZone")
                <*> (x .@? "CacheParameterGroup")
                <*> (x .@? "CacheClusterStatus")
                <*> (x .@? "SnapshotRetentionLimit")
                <*> (x .@? "ReplicationGroupId")
                <*> (x .@? "PendingModifiedValues")
                <*> (x .@? "NumCacheNodes")

instance Hashable CacheCluster where

instance NFData CacheCluster where

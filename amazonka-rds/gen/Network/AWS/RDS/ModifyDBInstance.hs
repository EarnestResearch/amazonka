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
-- Module      : Network.AWS.RDS.ModifyDBInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies settings for a DB instance. You can change one or more database configuration parameters by specifying these parameters and the new values in the request. To learn what modifications you can make to your DB instance, call @DescribeValidDBInstanceModifications@ before you call @ModifyDBInstance@ . 
--
--
module Network.AWS.RDS.ModifyDBInstance
    (
    -- * Creating a Request
      modifyDBInstance
    , ModifyDBInstance
    -- * Request Lenses
    , mdiEngineVersion
    , mdiDBSecurityGroups
    , mdiDBPortNumber
    , mdiDeletionProtection
    , mdiMasterUserPassword
    , mdiPubliclyAccessible
    , mdiAutoMinorVersionUpgrade
    , mdiDBSubnetGroupName
    , mdiMonitoringRoleARN
    , mdiIOPS
    , mdiAllowMajorVersionUpgrade
    , mdiNewDBInstanceIdentifier
    , mdiDomain
    , mdiMonitoringInterval
    , mdiCloudwatchLogsExportConfiguration
    , mdiCertificateRotationRestart
    , mdiTDECredentialPassword
    , mdiProcessorFeatures
    , mdiDBInstanceClass
    , mdiPromotionTier
    , mdiLicenseModel
    , mdiPreferredMaintenanceWindow
    , mdiPerformanceInsightsRetentionPeriod
    , mdiCACertificateIdentifier
    , mdiMaxAllocatedStorage
    , mdiEnablePerformanceInsights
    , mdiDBParameterGroupName
    , mdiPreferredBackupWindow
    , mdiBackupRetentionPeriod
    , mdiPerformanceInsightsKMSKeyId
    , mdiVPCSecurityGroupIds
    , mdiMultiAZ
    , mdiAllocatedStorage
    , mdiApplyImmediately
    , mdiOptionGroupName
    , mdiCopyTagsToSnapshot
    , mdiTDECredentialARN
    , mdiDomainIAMRoleName
    , mdiEnableIAMDatabaseAuthentication
    , mdiUseDefaultProcessorFeatures
    , mdiStorageType
    , mdiDBInstanceIdentifier

    -- * Destructuring the Response
    , modifyDBInstanceResponse
    , ModifyDBInstanceResponse
    -- * Response Lenses
    , mdirsDBInstance
    , mdirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | 
--
--
--
-- /See:/ 'modifyDBInstance' smart constructor.
data ModifyDBInstance = ModifyDBInstance'{_mdiEngineVersion
                                          :: !(Maybe Text),
                                          _mdiDBSecurityGroups ::
                                          !(Maybe [Text]),
                                          _mdiDBPortNumber :: !(Maybe Int),
                                          _mdiDeletionProtection ::
                                          !(Maybe Bool),
                                          _mdiMasterUserPassword ::
                                          !(Maybe Text),
                                          _mdiPubliclyAccessible ::
                                          !(Maybe Bool),
                                          _mdiAutoMinorVersionUpgrade ::
                                          !(Maybe Bool),
                                          _mdiDBSubnetGroupName ::
                                          !(Maybe Text),
                                          _mdiMonitoringRoleARN ::
                                          !(Maybe Text),
                                          _mdiIOPS :: !(Maybe Int),
                                          _mdiAllowMajorVersionUpgrade ::
                                          !(Maybe Bool),
                                          _mdiNewDBInstanceIdentifier ::
                                          !(Maybe Text),
                                          _mdiDomain :: !(Maybe Text),
                                          _mdiMonitoringInterval ::
                                          !(Maybe Int),
                                          _mdiCloudwatchLogsExportConfiguration
                                          ::
                                          !(Maybe
                                              CloudwatchLogsExportConfiguration),
                                          _mdiCertificateRotationRestart ::
                                          !(Maybe Bool),
                                          _mdiTDECredentialPassword ::
                                          !(Maybe Text),
                                          _mdiProcessorFeatures ::
                                          !(Maybe [ProcessorFeature]),
                                          _mdiDBInstanceClass :: !(Maybe Text),
                                          _mdiPromotionTier :: !(Maybe Int),
                                          _mdiLicenseModel :: !(Maybe Text),
                                          _mdiPreferredMaintenanceWindow ::
                                          !(Maybe Text),
                                          _mdiPerformanceInsightsRetentionPeriod
                                          :: !(Maybe Int),
                                          _mdiCACertificateIdentifier ::
                                          !(Maybe Text),
                                          _mdiMaxAllocatedStorage ::
                                          !(Maybe Int),
                                          _mdiEnablePerformanceInsights ::
                                          !(Maybe Bool),
                                          _mdiDBParameterGroupName ::
                                          !(Maybe Text),
                                          _mdiPreferredBackupWindow ::
                                          !(Maybe Text),
                                          _mdiBackupRetentionPeriod ::
                                          !(Maybe Int),
                                          _mdiPerformanceInsightsKMSKeyId ::
                                          !(Maybe Text),
                                          _mdiVPCSecurityGroupIds ::
                                          !(Maybe [Text]),
                                          _mdiMultiAZ :: !(Maybe Bool),
                                          _mdiAllocatedStorage :: !(Maybe Int),
                                          _mdiApplyImmediately :: !(Maybe Bool),
                                          _mdiOptionGroupName :: !(Maybe Text),
                                          _mdiCopyTagsToSnapshot ::
                                          !(Maybe Bool),
                                          _mdiTDECredentialARN :: !(Maybe Text),
                                          _mdiDomainIAMRoleName ::
                                          !(Maybe Text),
                                          _mdiEnableIAMDatabaseAuthentication ::
                                          !(Maybe Bool),
                                          _mdiUseDefaultProcessorFeatures ::
                                          !(Maybe Bool),
                                          _mdiStorageType :: !(Maybe Text),
                                          _mdiDBInstanceIdentifier :: !Text}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyDBInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdiEngineVersion' - The version number of the database engine to upgrade to. Changing this parameter results in an outage and the change is applied during the next maintenance window unless the @ApplyImmediately@ parameter is eanbled for this request.  For major version upgrades, if a nondefault DB parameter group is currently in use, a new DB parameter group in the DB parameter group family for the new engine version must be specified. The new DB parameter group can be the default for that DB parameter group family. For information about valid engine versions, see @CreateDBInstance@ , or call @DescribeDBEngineVersions@ .
--
-- * 'mdiDBSecurityGroups' - A list of DB security groups to authorize on this DB instance. Changing this setting doesn't result in an outage and the change is asynchronously applied as soon as possible. Constraints:     * If supplied, must match existing DBSecurityGroups.
--
-- * 'mdiDBPortNumber' - The port number on which the database accepts connections. The value of the @DBPortNumber@ parameter must not match any of the port values specified for options in the option group for the DB instance. Your database will restart when you change the @DBPortNumber@ value regardless of the value of the @ApplyImmediately@ parameter. __MySQL__  Default: @3306@  Valid values: @1150-65535@  __MariaDB__  Default: @3306@  Valid values: @1150-65535@  __PostgreSQL__  Default: @5432@  Valid values: @1150-65535@  Type: Integer __Oracle__  Default: @1521@  Valid values: @1150-65535@  __SQL Server__  Default: @1433@  Valid values: @1150-65535@ except @1234@ , @1434@ , @3260@ , @3343@ , @3389@ , @47001@ , and @49152-49156@ . __Amazon Aurora__  Default: @3306@  Valid values: @1150-65535@ 
--
-- * 'mdiDeletionProtection' - A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> . 
--
-- * 'mdiMasterUserPassword' - The new password for the master user. The password can include any printable ASCII character except "/", """, or "@". Changing this parameter doesn't result in an outage and the change is asynchronously applied as soon as possible. Between the time of the request and the completion of the request, the @MasterUserPassword@ element exists in the @PendingModifiedValues@ element of the operation response.  __Amazon Aurora__  Not applicable. The password for the master user is managed by the DB cluster. For more information, see @ModifyDBCluster@ .  Default: Uses existing setting __MariaDB__  Constraints: Must contain from 8 to 41 characters. __Microsoft SQL Server__  Constraints: Must contain from 8 to 128 characters. __MySQL__  Constraints: Must contain from 8 to 41 characters. __Oracle__  Constraints: Must contain from 8 to 30 characters. __PostgreSQL__  Constraints: Must contain from 8 to 128 characters.
--
-- * 'mdiPubliclyAccessible' - A value that indicates whether the DB instance is publicly accessible. When the DB instance is publicly accessible, it is an Internet-facing instance with a publicly resolvable DNS name, which resolves to a public IP address. When the DB instance isn't publicly accessible, it is an internal instance with a DNS name that resolves to a private IP address.  @PubliclyAccessible@ only applies to DB instances in a VPC. The DB instance must be part of a public subnet and @PubliclyAccessible@ must be enabled for it to be publicly accessible.  Changes to the @PubliclyAccessible@ parameter are applied immediately regardless of the value of the @ApplyImmediately@ parameter.
--
-- * 'mdiAutoMinorVersionUpgrade' - A value that indicates whether minor version upgrades are applied automatically to the DB instance during the maintenance window. Changing this parameter doesn't result in an outage except in the following case and the change is asynchronously applied as soon as possible. An outage results if this parameter is enabled during the maintenance window, and a newer minor version is available, and RDS has enabled auto patching for that engine version. 
--
-- * 'mdiDBSubnetGroupName' - The new DB subnet group for the DB instance. You can use this parameter to move your DB instance to a different VPC. If your DB instance isn't in a VPC, you can also use this parameter to move your DB instance into a VPC. For more information, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html#USER_VPC.Non-VPC2VPC Updating the VPC for a DB Instance> in the /Amazon RDS User Guide./  Changing the subnet group causes an outage during the change. The change is applied during the next maintenance window, unless you enable @ApplyImmediately@ .  Constraints: If supplied, must match the name of an existing DBSubnetGroup. Example: @mySubnetGroup@ 
--
-- * 'mdiMonitoringRoleARN' - The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to Amazon CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, go to <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring> in the /Amazon RDS User Guide./  If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
--
-- * 'mdiIOPS' - The new Provisioned IOPS (I/O operations per second) value for the RDS instance.  Changing this setting doesn't result in an outage and the change is applied during the next maintenance window unless the @ApplyImmediately@ parameter is enabled for this request. If you are migrating from Provisioned IOPS to standard storage, set this value to 0. The DB instance will require a reboot for the change in storage type to take effect.  If you choose to migrate your DB instance from using standard storage to using Provisioned IOPS, or from using Provisioned IOPS to using standard storage, the process can take time. The duration of the migration depends on several factors such as database load, storage size, storage type (standard or Provisioned IOPS), amount of IOPS provisioned (if any), and the number of prior scale storage operations. Typical migration times are under 24 hours, but the process can take up to several days in some cases. During the migration, the DB instance is available for use, but might experience performance degradation. While the migration takes place, nightly backups for the instance are suspended. No other Amazon RDS operations can take place for the instance, including modifying the instance, rebooting the instance, deleting the instance, creating a read replica for the instance, and creating a DB snapshot of the instance.  Constraints: For MariaDB, MySQL, Oracle, and PostgreSQL, the value supplied must be at least 10% greater than the current value. Values that are not at least 10% greater than the existing value are rounded up so that they are 10% greater than the current value.  Default: Uses existing setting
--
-- * 'mdiAllowMajorVersionUpgrade' - A value that indicates whether major version upgrades are allowed. Changing this parameter doesn't result in an outage and the change is asynchronously applied as soon as possible. Constraints: Major version upgrades must be allowed when specifying a value for the EngineVersion parameter that is a different major version than the DB instance's current version.
--
-- * 'mdiNewDBInstanceIdentifier' - The new DB instance identifier for the DB instance when renaming a DB instance. When you change the DB instance identifier, an instance reboot occurs immediately if you enable @ApplyImmediately@ , or will occur during the next maintenance window if you disable Apply Immediately. This value is stored as a lowercase string.  Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens.     * The first character must be a letter.     * Can't end with a hyphen or contain two consecutive hyphens. Example: @mydbinstance@ 
--
-- * 'mdiDomain' - The Active Directory directory ID to move the DB instance to. Specify @none@ to remove the instance from its current domain. The domain must be created prior to this operation. Currently, only Microsoft SQL Server and Oracle DB instances can be created in an Active Directory Domain.  For Microsoft SQL Server DB instances, Amazon RDS can use Windows Authentication to authenticate users that connect to the DB instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_SQLServerWinAuth.html Using Windows Authentication with an Amazon RDS DB Instance Running Microsoft SQL Server> in the /Amazon RDS User Guide/ . For Oracle DB instances, Amazon RDS can use Kerberos Authentication to authenticate users that connect to the DB instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-kerberos.html Using Kerberos Authentication with Amazon RDS for Oracle> in the /Amazon RDS User Guide/ .
--
-- * 'mdiMonitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance. To disable collecting Enhanced Monitoring metrics, specify 0. The default is 0. If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0. Valid Values: @0, 1, 5, 10, 15, 30, 60@ 
--
-- * 'mdiCloudwatchLogsExportConfiguration' - The configuration setting for the log types to be enabled for export to CloudWatch Logs for a specific DB instance. A change to the @CloudwatchLogsExportConfiguration@ parameter is always applied to the DB instance immediately. Therefore, the @ApplyImmediately@ parameter has no effect.
--
-- * 'mdiCertificateRotationRestart' - A value that indicates whether the DB instance is restarted when you rotate your SSL/TLS certificate. By default, the DB instance is restarted when you rotate your SSL/TLS certificate. The certificate is not updated until the DB instance is restarted. /Important:/ Set this parameter only if you are /not/ using SSL/TLS to connect to the DB instance. If you are using SSL/TLS to connect to the DB instance, follow the appropriate instructions for your DB engine to rotate your SSL/TLS certificate:     * For more information about rotating your SSL/TLS certificate for RDS DB engines, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.SSL-certificate-rotation.html Rotating Your SSL/TLS Certificate.> in the /Amazon RDS User Guide./      * For more information about rotating your SSL/TLS certificate for Aurora DB engines, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.SSL-certificate-rotation.html Rotating Your SSL/TLS Certificate> in the /Amazon Aurora User Guide./ 
--
-- * 'mdiTDECredentialPassword' - The password for the given ARN from the key store in order to access the device.
--
-- * 'mdiProcessorFeatures' - The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
--
-- * 'mdiDBInstanceClass' - The new compute and memory capacity of the DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./  If you modify the DB instance class, an outage occurs during the change. The change is applied during the next maintenance window, unless @ApplyImmediately@ is enabled for this request.  Default: Uses existing setting
--
-- * 'mdiPromotionTier' - A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> in the /Amazon Aurora User Guide/ .  Default: 1 Valid Values: 0 - 15
--
-- * 'mdiLicenseModel' - The license model for the DB instance. Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@ 
--
-- * 'mdiPreferredMaintenanceWindow' - The weekly time range (in UTC) during which system maintenance can occur, which might result in an outage. Changing this parameter doesn't result in an outage, except in the following situation, and the change is asynchronously applied as soon as possible. If there are pending actions that cause a reboot, and the maintenance window is changed to include the current time, then changing this parameter will cause a reboot of the DB instance. If moving this window to the current time, there must be at least 30 minutes between the current time and end of the window to ensure pending changes are applied. Default: Uses existing setting Format: ddd:hh24:mi-ddd:hh24:mi Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun Constraints: Must be at least 30 minutes
--
-- * 'mdiPerformanceInsightsRetentionPeriod' - The amount of time, in days, to retain Performance Insights data. Valid values are 7 or 731 (2 years). 
--
-- * 'mdiCACertificateIdentifier' - Indicates the certificate that needs to be associated with the instance.
--
-- * 'mdiMaxAllocatedStorage' - The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
--
-- * 'mdiEnablePerformanceInsights' - A value that indicates whether to enable Performance Insights for the DB instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights> in the /Amazon Relational Database Service User Guide/ . 
--
-- * 'mdiDBParameterGroupName' - The name of the DB parameter group to apply to the DB instance. Changing this setting doesn't result in an outage. The parameter group name itself is changed immediately, but the actual parameter changes are not applied until you reboot the instance without failover. In this case, the DB instance isn't rebooted automatically and the parameter changes isn't applied during the next maintenance window. Default: Uses existing setting Constraints: The DB parameter group must be in the same DB parameter group family as this DB instance.
--
-- * 'mdiPreferredBackupWindow' - The daily time range during which automated backups are created if automated backups are enabled, as determined by the @BackupRetentionPeriod@ parameter. Changing this parameter doesn't result in an outage and the change is asynchronously applied as soon as possible.  __Amazon Aurora__  Not applicable. The daily time range for creating automated backups is managed by the DB cluster. For more information, see @ModifyDBCluster@ . Constraints:     * Must be in the format hh24:mi-hh24:mi     * Must be in Universal Time Coordinated (UTC)     * Must not conflict with the preferred maintenance window     * Must be at least 30 minutes
--
-- * 'mdiBackupRetentionPeriod' - The number of days to retain automated backups. Setting this parameter to a positive number enables backups. Setting this parameter to 0 disables automated backups. Changing this parameter can result in an outage if you change from 0 to a non-zero value or from a non-zero value to 0. These changes are applied during the next maintenance window unless the @ApplyImmediately@ parameter is enabled for this request. If you change the parameter from one non-zero value to another non-zero value, the change is asynchronously applied as soon as possible. __Amazon Aurora__  Not applicable. The retention period for automated backups is managed by the DB cluster. For more information, see @ModifyDBCluster@ . Default: Uses existing setting Constraints:     * Must be a value from 0 to 35     * Can be specified for a MySQL read replica only if the source is running MySQL 5.6 or later     * Can be specified for a PostgreSQL read replica only if the source is running PostgreSQL 9.3.5     * Can't be set to 0 if the DB instance is a source to read replicas
--
-- * 'mdiPerformanceInsightsKMSKeyId' - The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key. If you do not specify a value for @PerformanceInsightsKMSKeyId@ , then Amazon RDS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- * 'mdiVPCSecurityGroupIds' - A list of EC2 VPC security groups to authorize on this DB instance. This change is asynchronously applied as soon as possible. __Amazon Aurora__  Not applicable. The associated list of EC2 VPC security groups is managed by the DB cluster. For more information, see @ModifyDBCluster@ . Constraints:     * If supplied, must match existing VpcSecurityGroupIds.
--
-- * 'mdiMultiAZ' - A value that indicates whether the DB instance is a Multi-AZ deployment. Changing this parameter doesn't result in an outage and the change is applied during the next maintenance window unless the @ApplyImmediately@ parameter is enabled for this request. 
--
-- * 'mdiAllocatedStorage' - The new amount of storage (in gibibytes) to allocate for the DB instance.  For MariaDB, MySQL, Oracle, and PostgreSQL, the value supplied must be at least 10% greater than the current value. Values that are not at least 10% greater than the existing value are rounded up so that they are 10% greater than the current value.  For the valid values for allocated storage for each engine, see @CreateDBInstance@ . 
--
-- * 'mdiApplyImmediately' - A value that indicates whether the modifications in this request and any pending modifications are asynchronously applied as soon as possible, regardless of the @PreferredMaintenanceWindow@ setting for the DB instance. By default, this parameter is disabled.  If this parameter is disabled, changes to the DB instance are applied during the next maintenance window. Some parameter changes can cause an outage and are applied on the next call to 'RebootDBInstance' , or the next failure reboot. Review the table of parameters in <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.DBInstance.Modifying.html Modifying a DB Instance> in the /Amazon RDS User Guide./ to see the impact of enabling or disabling @ApplyImmediately@ for each modified parameter and to determine when the changes are applied. 
--
-- * 'mdiOptionGroupName' - Indicates that the DB instance should be associated with the specified option group. Changing this parameter doesn't result in an outage except in the following case and the change is applied during the next maintenance window unless the @ApplyImmediately@ parameter is enabled for this request. If the parameter change results in an option group that enables OEM, this change can cause a brief (sub-second) period during which new connections are rejected but existing connections are not interrupted.  Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group, and that option group can't be removed from a DB instance once it is associated with a DB instance
--
-- * 'mdiCopyTagsToSnapshot' - A value that indicates whether to copy all tags from the DB instance to snapshots of the DB instance. By default, tags are not copied. __Amazon Aurora__  Not applicable. Copying tags to snapshots is managed by the DB cluster. Setting this value for an Aurora DB instance has no effect on the DB cluster setting. For more information, see @ModifyDBCluster@ .
--
-- * 'mdiTDECredentialARN' - The ARN from the key store with which to associate the instance for TDE encryption.
--
-- * 'mdiDomainIAMRoleName' - The name of the IAM role to use when making API calls to the Directory Service.
--
-- * 'mdiEnableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled. For information about the supported DB engines, see 'CreateDBInstance' . For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./ 
--
-- * 'mdiUseDefaultProcessorFeatures' - A value that indicates whether the DB instance class of the DB instance uses its default processor features.
--
-- * 'mdiStorageType' - Specifies the storage type to be associated with the DB instance.  If you specify Provisioned IOPS (@io1@ ), you must also include a value for the @Iops@ parameter.  If you choose to migrate your DB instance from using standard storage to using Provisioned IOPS, or from using Provisioned IOPS to using standard storage, the process can take time. The duration of the migration depends on several factors such as database load, storage size, storage type (standard or Provisioned IOPS), amount of IOPS provisioned (if any), and the number of prior scale storage operations. Typical migration times are under 24 hours, but the process can take up to several days in some cases. During the migration, the DB instance is available for use, but might experience performance degradation. While the migration takes place, nightly backups for the instance are suspended. No other Amazon RDS operations can take place for the instance, including modifying the instance, rebooting the instance, deleting the instance, creating a read replica for the instance, and creating a DB snapshot of the instance.  Valid values: @standard | gp2 | io1@  Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@ 
--
-- * 'mdiDBInstanceIdentifier' - The DB instance identifier. This value is stored as a lowercase string. Constraints:     * Must match the identifier of an existing DBInstance.
modifyDBInstance
    :: Text -- ^ 'mdiDBInstanceIdentifier'
    -> ModifyDBInstance
modifyDBInstance pDBInstanceIdentifier_
  = ModifyDBInstance'{_mdiEngineVersion = Nothing,
                      _mdiDBSecurityGroups = Nothing,
                      _mdiDBPortNumber = Nothing,
                      _mdiDeletionProtection = Nothing,
                      _mdiMasterUserPassword = Nothing,
                      _mdiPubliclyAccessible = Nothing,
                      _mdiAutoMinorVersionUpgrade = Nothing,
                      _mdiDBSubnetGroupName = Nothing,
                      _mdiMonitoringRoleARN = Nothing, _mdiIOPS = Nothing,
                      _mdiAllowMajorVersionUpgrade = Nothing,
                      _mdiNewDBInstanceIdentifier = Nothing,
                      _mdiDomain = Nothing,
                      _mdiMonitoringInterval = Nothing,
                      _mdiCloudwatchLogsExportConfiguration = Nothing,
                      _mdiCertificateRotationRestart = Nothing,
                      _mdiTDECredentialPassword = Nothing,
                      _mdiProcessorFeatures = Nothing,
                      _mdiDBInstanceClass = Nothing,
                      _mdiPromotionTier = Nothing,
                      _mdiLicenseModel = Nothing,
                      _mdiPreferredMaintenanceWindow = Nothing,
                      _mdiPerformanceInsightsRetentionPeriod = Nothing,
                      _mdiCACertificateIdentifier = Nothing,
                      _mdiMaxAllocatedStorage = Nothing,
                      _mdiEnablePerformanceInsights = Nothing,
                      _mdiDBParameterGroupName = Nothing,
                      _mdiPreferredBackupWindow = Nothing,
                      _mdiBackupRetentionPeriod = Nothing,
                      _mdiPerformanceInsightsKMSKeyId = Nothing,
                      _mdiVPCSecurityGroupIds = Nothing,
                      _mdiMultiAZ = Nothing,
                      _mdiAllocatedStorage = Nothing,
                      _mdiApplyImmediately = Nothing,
                      _mdiOptionGroupName = Nothing,
                      _mdiCopyTagsToSnapshot = Nothing,
                      _mdiTDECredentialARN = Nothing,
                      _mdiDomainIAMRoleName = Nothing,
                      _mdiEnableIAMDatabaseAuthentication = Nothing,
                      _mdiUseDefaultProcessorFeatures = Nothing,
                      _mdiStorageType = Nothing,
                      _mdiDBInstanceIdentifier = pDBInstanceIdentifier_}

-- | The version number of the database engine to upgrade to. Changing this parameter results in an outage and the change is applied during the next maintenance window unless the @ApplyImmediately@ parameter is eanbled for this request.  For major version upgrades, if a nondefault DB parameter group is currently in use, a new DB parameter group in the DB parameter group family for the new engine version must be specified. The new DB parameter group can be the default for that DB parameter group family. For information about valid engine versions, see @CreateDBInstance@ , or call @DescribeDBEngineVersions@ .
mdiEngineVersion :: Lens' ModifyDBInstance (Maybe Text)
mdiEngineVersion = lens _mdiEngineVersion (\ s a -> s{_mdiEngineVersion = a})

-- | A list of DB security groups to authorize on this DB instance. Changing this setting doesn't result in an outage and the change is asynchronously applied as soon as possible. Constraints:     * If supplied, must match existing DBSecurityGroups.
mdiDBSecurityGroups :: Lens' ModifyDBInstance [Text]
mdiDBSecurityGroups = lens _mdiDBSecurityGroups (\ s a -> s{_mdiDBSecurityGroups = a}) . _Default . _Coerce

-- | The port number on which the database accepts connections. The value of the @DBPortNumber@ parameter must not match any of the port values specified for options in the option group for the DB instance. Your database will restart when you change the @DBPortNumber@ value regardless of the value of the @ApplyImmediately@ parameter. __MySQL__  Default: @3306@  Valid values: @1150-65535@  __MariaDB__  Default: @3306@  Valid values: @1150-65535@  __PostgreSQL__  Default: @5432@  Valid values: @1150-65535@  Type: Integer __Oracle__  Default: @1521@  Valid values: @1150-65535@  __SQL Server__  Default: @1433@  Valid values: @1150-65535@ except @1234@ , @1434@ , @3260@ , @3343@ , @3389@ , @47001@ , and @49152-49156@ . __Amazon Aurora__  Default: @3306@  Valid values: @1150-65535@ 
mdiDBPortNumber :: Lens' ModifyDBInstance (Maybe Int)
mdiDBPortNumber = lens _mdiDBPortNumber (\ s a -> s{_mdiDBPortNumber = a})

-- | A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> . 
mdiDeletionProtection :: Lens' ModifyDBInstance (Maybe Bool)
mdiDeletionProtection = lens _mdiDeletionProtection (\ s a -> s{_mdiDeletionProtection = a})

-- | The new password for the master user. The password can include any printable ASCII character except "/", """, or "@". Changing this parameter doesn't result in an outage and the change is asynchronously applied as soon as possible. Between the time of the request and the completion of the request, the @MasterUserPassword@ element exists in the @PendingModifiedValues@ element of the operation response.  __Amazon Aurora__  Not applicable. The password for the master user is managed by the DB cluster. For more information, see @ModifyDBCluster@ .  Default: Uses existing setting __MariaDB__  Constraints: Must contain from 8 to 41 characters. __Microsoft SQL Server__  Constraints: Must contain from 8 to 128 characters. __MySQL__  Constraints: Must contain from 8 to 41 characters. __Oracle__  Constraints: Must contain from 8 to 30 characters. __PostgreSQL__  Constraints: Must contain from 8 to 128 characters.
mdiMasterUserPassword :: Lens' ModifyDBInstance (Maybe Text)
mdiMasterUserPassword = lens _mdiMasterUserPassword (\ s a -> s{_mdiMasterUserPassword = a})

-- | A value that indicates whether the DB instance is publicly accessible. When the DB instance is publicly accessible, it is an Internet-facing instance with a publicly resolvable DNS name, which resolves to a public IP address. When the DB instance isn't publicly accessible, it is an internal instance with a DNS name that resolves to a private IP address.  @PubliclyAccessible@ only applies to DB instances in a VPC. The DB instance must be part of a public subnet and @PubliclyAccessible@ must be enabled for it to be publicly accessible.  Changes to the @PubliclyAccessible@ parameter are applied immediately regardless of the value of the @ApplyImmediately@ parameter.
mdiPubliclyAccessible :: Lens' ModifyDBInstance (Maybe Bool)
mdiPubliclyAccessible = lens _mdiPubliclyAccessible (\ s a -> s{_mdiPubliclyAccessible = a})

-- | A value that indicates whether minor version upgrades are applied automatically to the DB instance during the maintenance window. Changing this parameter doesn't result in an outage except in the following case and the change is asynchronously applied as soon as possible. An outage results if this parameter is enabled during the maintenance window, and a newer minor version is available, and RDS has enabled auto patching for that engine version. 
mdiAutoMinorVersionUpgrade :: Lens' ModifyDBInstance (Maybe Bool)
mdiAutoMinorVersionUpgrade = lens _mdiAutoMinorVersionUpgrade (\ s a -> s{_mdiAutoMinorVersionUpgrade = a})

-- | The new DB subnet group for the DB instance. You can use this parameter to move your DB instance to a different VPC. If your DB instance isn't in a VPC, you can also use this parameter to move your DB instance into a VPC. For more information, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html#USER_VPC.Non-VPC2VPC Updating the VPC for a DB Instance> in the /Amazon RDS User Guide./  Changing the subnet group causes an outage during the change. The change is applied during the next maintenance window, unless you enable @ApplyImmediately@ .  Constraints: If supplied, must match the name of an existing DBSubnetGroup. Example: @mySubnetGroup@ 
mdiDBSubnetGroupName :: Lens' ModifyDBInstance (Maybe Text)
mdiDBSubnetGroupName = lens _mdiDBSubnetGroupName (\ s a -> s{_mdiDBSubnetGroupName = a})

-- | The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to Amazon CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, go to <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring> in the /Amazon RDS User Guide./  If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
mdiMonitoringRoleARN :: Lens' ModifyDBInstance (Maybe Text)
mdiMonitoringRoleARN = lens _mdiMonitoringRoleARN (\ s a -> s{_mdiMonitoringRoleARN = a})

-- | The new Provisioned IOPS (I/O operations per second) value for the RDS instance.  Changing this setting doesn't result in an outage and the change is applied during the next maintenance window unless the @ApplyImmediately@ parameter is enabled for this request. If you are migrating from Provisioned IOPS to standard storage, set this value to 0. The DB instance will require a reboot for the change in storage type to take effect.  If you choose to migrate your DB instance from using standard storage to using Provisioned IOPS, or from using Provisioned IOPS to using standard storage, the process can take time. The duration of the migration depends on several factors such as database load, storage size, storage type (standard or Provisioned IOPS), amount of IOPS provisioned (if any), and the number of prior scale storage operations. Typical migration times are under 24 hours, but the process can take up to several days in some cases. During the migration, the DB instance is available for use, but might experience performance degradation. While the migration takes place, nightly backups for the instance are suspended. No other Amazon RDS operations can take place for the instance, including modifying the instance, rebooting the instance, deleting the instance, creating a read replica for the instance, and creating a DB snapshot of the instance.  Constraints: For MariaDB, MySQL, Oracle, and PostgreSQL, the value supplied must be at least 10% greater than the current value. Values that are not at least 10% greater than the existing value are rounded up so that they are 10% greater than the current value.  Default: Uses existing setting
mdiIOPS :: Lens' ModifyDBInstance (Maybe Int)
mdiIOPS = lens _mdiIOPS (\ s a -> s{_mdiIOPS = a})

-- | A value that indicates whether major version upgrades are allowed. Changing this parameter doesn't result in an outage and the change is asynchronously applied as soon as possible. Constraints: Major version upgrades must be allowed when specifying a value for the EngineVersion parameter that is a different major version than the DB instance's current version.
mdiAllowMajorVersionUpgrade :: Lens' ModifyDBInstance (Maybe Bool)
mdiAllowMajorVersionUpgrade = lens _mdiAllowMajorVersionUpgrade (\ s a -> s{_mdiAllowMajorVersionUpgrade = a})

-- | The new DB instance identifier for the DB instance when renaming a DB instance. When you change the DB instance identifier, an instance reboot occurs immediately if you enable @ApplyImmediately@ , or will occur during the next maintenance window if you disable Apply Immediately. This value is stored as a lowercase string.  Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens.     * The first character must be a letter.     * Can't end with a hyphen or contain two consecutive hyphens. Example: @mydbinstance@ 
mdiNewDBInstanceIdentifier :: Lens' ModifyDBInstance (Maybe Text)
mdiNewDBInstanceIdentifier = lens _mdiNewDBInstanceIdentifier (\ s a -> s{_mdiNewDBInstanceIdentifier = a})

-- | The Active Directory directory ID to move the DB instance to. Specify @none@ to remove the instance from its current domain. The domain must be created prior to this operation. Currently, only Microsoft SQL Server and Oracle DB instances can be created in an Active Directory Domain.  For Microsoft SQL Server DB instances, Amazon RDS can use Windows Authentication to authenticate users that connect to the DB instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_SQLServerWinAuth.html Using Windows Authentication with an Amazon RDS DB Instance Running Microsoft SQL Server> in the /Amazon RDS User Guide/ . For Oracle DB instances, Amazon RDS can use Kerberos Authentication to authenticate users that connect to the DB instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-kerberos.html Using Kerberos Authentication with Amazon RDS for Oracle> in the /Amazon RDS User Guide/ .
mdiDomain :: Lens' ModifyDBInstance (Maybe Text)
mdiDomain = lens _mdiDomain (\ s a -> s{_mdiDomain = a})

-- | The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the DB instance. To disable collecting Enhanced Monitoring metrics, specify 0. The default is 0. If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0. Valid Values: @0, 1, 5, 10, 15, 30, 60@ 
mdiMonitoringInterval :: Lens' ModifyDBInstance (Maybe Int)
mdiMonitoringInterval = lens _mdiMonitoringInterval (\ s a -> s{_mdiMonitoringInterval = a})

-- | The configuration setting for the log types to be enabled for export to CloudWatch Logs for a specific DB instance. A change to the @CloudwatchLogsExportConfiguration@ parameter is always applied to the DB instance immediately. Therefore, the @ApplyImmediately@ parameter has no effect.
mdiCloudwatchLogsExportConfiguration :: Lens' ModifyDBInstance (Maybe CloudwatchLogsExportConfiguration)
mdiCloudwatchLogsExportConfiguration = lens _mdiCloudwatchLogsExportConfiguration (\ s a -> s{_mdiCloudwatchLogsExportConfiguration = a})

-- | A value that indicates whether the DB instance is restarted when you rotate your SSL/TLS certificate. By default, the DB instance is restarted when you rotate your SSL/TLS certificate. The certificate is not updated until the DB instance is restarted. /Important:/ Set this parameter only if you are /not/ using SSL/TLS to connect to the DB instance. If you are using SSL/TLS to connect to the DB instance, follow the appropriate instructions for your DB engine to rotate your SSL/TLS certificate:     * For more information about rotating your SSL/TLS certificate for RDS DB engines, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.SSL-certificate-rotation.html Rotating Your SSL/TLS Certificate.> in the /Amazon RDS User Guide./      * For more information about rotating your SSL/TLS certificate for Aurora DB engines, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.SSL-certificate-rotation.html Rotating Your SSL/TLS Certificate> in the /Amazon Aurora User Guide./ 
mdiCertificateRotationRestart :: Lens' ModifyDBInstance (Maybe Bool)
mdiCertificateRotationRestart = lens _mdiCertificateRotationRestart (\ s a -> s{_mdiCertificateRotationRestart = a})

-- | The password for the given ARN from the key store in order to access the device.
mdiTDECredentialPassword :: Lens' ModifyDBInstance (Maybe Text)
mdiTDECredentialPassword = lens _mdiTDECredentialPassword (\ s a -> s{_mdiTDECredentialPassword = a})

-- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
mdiProcessorFeatures :: Lens' ModifyDBInstance [ProcessorFeature]
mdiProcessorFeatures = lens _mdiProcessorFeatures (\ s a -> s{_mdiProcessorFeatures = a}) . _Default . _Coerce

-- | The new compute and memory capacity of the DB instance, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./  If you modify the DB instance class, an outage occurs during the change. The change is applied during the next maintenance window, unless @ApplyImmediately@ is enabled for this request.  Default: Uses existing setting
mdiDBInstanceClass :: Lens' ModifyDBInstance (Maybe Text)
mdiDBInstanceClass = lens _mdiDBInstanceClass (\ s a -> s{_mdiDBInstanceClass = a})

-- | A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> in the /Amazon Aurora User Guide/ .  Default: 1 Valid Values: 0 - 15
mdiPromotionTier :: Lens' ModifyDBInstance (Maybe Int)
mdiPromotionTier = lens _mdiPromotionTier (\ s a -> s{_mdiPromotionTier = a})

-- | The license model for the DB instance. Valid values: @license-included@ | @bring-your-own-license@ | @general-public-license@ 
mdiLicenseModel :: Lens' ModifyDBInstance (Maybe Text)
mdiLicenseModel = lens _mdiLicenseModel (\ s a -> s{_mdiLicenseModel = a})

-- | The weekly time range (in UTC) during which system maintenance can occur, which might result in an outage. Changing this parameter doesn't result in an outage, except in the following situation, and the change is asynchronously applied as soon as possible. If there are pending actions that cause a reboot, and the maintenance window is changed to include the current time, then changing this parameter will cause a reboot of the DB instance. If moving this window to the current time, there must be at least 30 minutes between the current time and end of the window to ensure pending changes are applied. Default: Uses existing setting Format: ddd:hh24:mi-ddd:hh24:mi Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun Constraints: Must be at least 30 minutes
mdiPreferredMaintenanceWindow :: Lens' ModifyDBInstance (Maybe Text)
mdiPreferredMaintenanceWindow = lens _mdiPreferredMaintenanceWindow (\ s a -> s{_mdiPreferredMaintenanceWindow = a})

-- | The amount of time, in days, to retain Performance Insights data. Valid values are 7 or 731 (2 years). 
mdiPerformanceInsightsRetentionPeriod :: Lens' ModifyDBInstance (Maybe Int)
mdiPerformanceInsightsRetentionPeriod = lens _mdiPerformanceInsightsRetentionPeriod (\ s a -> s{_mdiPerformanceInsightsRetentionPeriod = a})

-- | Indicates the certificate that needs to be associated with the instance.
mdiCACertificateIdentifier :: Lens' ModifyDBInstance (Maybe Text)
mdiCACertificateIdentifier = lens _mdiCACertificateIdentifier (\ s a -> s{_mdiCACertificateIdentifier = a})

-- | The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
mdiMaxAllocatedStorage :: Lens' ModifyDBInstance (Maybe Int)
mdiMaxAllocatedStorage = lens _mdiMaxAllocatedStorage (\ s a -> s{_mdiMaxAllocatedStorage = a})

-- | A value that indicates whether to enable Performance Insights for the DB instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights> in the /Amazon Relational Database Service User Guide/ . 
mdiEnablePerformanceInsights :: Lens' ModifyDBInstance (Maybe Bool)
mdiEnablePerformanceInsights = lens _mdiEnablePerformanceInsights (\ s a -> s{_mdiEnablePerformanceInsights = a})

-- | The name of the DB parameter group to apply to the DB instance. Changing this setting doesn't result in an outage. The parameter group name itself is changed immediately, but the actual parameter changes are not applied until you reboot the instance without failover. In this case, the DB instance isn't rebooted automatically and the parameter changes isn't applied during the next maintenance window. Default: Uses existing setting Constraints: The DB parameter group must be in the same DB parameter group family as this DB instance.
mdiDBParameterGroupName :: Lens' ModifyDBInstance (Maybe Text)
mdiDBParameterGroupName = lens _mdiDBParameterGroupName (\ s a -> s{_mdiDBParameterGroupName = a})

-- | The daily time range during which automated backups are created if automated backups are enabled, as determined by the @BackupRetentionPeriod@ parameter. Changing this parameter doesn't result in an outage and the change is asynchronously applied as soon as possible.  __Amazon Aurora__  Not applicable. The daily time range for creating automated backups is managed by the DB cluster. For more information, see @ModifyDBCluster@ . Constraints:     * Must be in the format hh24:mi-hh24:mi     * Must be in Universal Time Coordinated (UTC)     * Must not conflict with the preferred maintenance window     * Must be at least 30 minutes
mdiPreferredBackupWindow :: Lens' ModifyDBInstance (Maybe Text)
mdiPreferredBackupWindow = lens _mdiPreferredBackupWindow (\ s a -> s{_mdiPreferredBackupWindow = a})

-- | The number of days to retain automated backups. Setting this parameter to a positive number enables backups. Setting this parameter to 0 disables automated backups. Changing this parameter can result in an outage if you change from 0 to a non-zero value or from a non-zero value to 0. These changes are applied during the next maintenance window unless the @ApplyImmediately@ parameter is enabled for this request. If you change the parameter from one non-zero value to another non-zero value, the change is asynchronously applied as soon as possible. __Amazon Aurora__  Not applicable. The retention period for automated backups is managed by the DB cluster. For more information, see @ModifyDBCluster@ . Default: Uses existing setting Constraints:     * Must be a value from 0 to 35     * Can be specified for a MySQL read replica only if the source is running MySQL 5.6 or later     * Can be specified for a PostgreSQL read replica only if the source is running PostgreSQL 9.3.5     * Can't be set to 0 if the DB instance is a source to read replicas
mdiBackupRetentionPeriod :: Lens' ModifyDBInstance (Maybe Int)
mdiBackupRetentionPeriod = lens _mdiBackupRetentionPeriod (\ s a -> s{_mdiBackupRetentionPeriod = a})

-- | The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key. If you do not specify a value for @PerformanceInsightsKMSKeyId@ , then Amazon RDS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
mdiPerformanceInsightsKMSKeyId :: Lens' ModifyDBInstance (Maybe Text)
mdiPerformanceInsightsKMSKeyId = lens _mdiPerformanceInsightsKMSKeyId (\ s a -> s{_mdiPerformanceInsightsKMSKeyId = a})

-- | A list of EC2 VPC security groups to authorize on this DB instance. This change is asynchronously applied as soon as possible. __Amazon Aurora__  Not applicable. The associated list of EC2 VPC security groups is managed by the DB cluster. For more information, see @ModifyDBCluster@ . Constraints:     * If supplied, must match existing VpcSecurityGroupIds.
mdiVPCSecurityGroupIds :: Lens' ModifyDBInstance [Text]
mdiVPCSecurityGroupIds = lens _mdiVPCSecurityGroupIds (\ s a -> s{_mdiVPCSecurityGroupIds = a}) . _Default . _Coerce

-- | A value that indicates whether the DB instance is a Multi-AZ deployment. Changing this parameter doesn't result in an outage and the change is applied during the next maintenance window unless the @ApplyImmediately@ parameter is enabled for this request. 
mdiMultiAZ :: Lens' ModifyDBInstance (Maybe Bool)
mdiMultiAZ = lens _mdiMultiAZ (\ s a -> s{_mdiMultiAZ = a})

-- | The new amount of storage (in gibibytes) to allocate for the DB instance.  For MariaDB, MySQL, Oracle, and PostgreSQL, the value supplied must be at least 10% greater than the current value. Values that are not at least 10% greater than the existing value are rounded up so that they are 10% greater than the current value.  For the valid values for allocated storage for each engine, see @CreateDBInstance@ . 
mdiAllocatedStorage :: Lens' ModifyDBInstance (Maybe Int)
mdiAllocatedStorage = lens _mdiAllocatedStorage (\ s a -> s{_mdiAllocatedStorage = a})

-- | A value that indicates whether the modifications in this request and any pending modifications are asynchronously applied as soon as possible, regardless of the @PreferredMaintenanceWindow@ setting for the DB instance. By default, this parameter is disabled.  If this parameter is disabled, changes to the DB instance are applied during the next maintenance window. Some parameter changes can cause an outage and are applied on the next call to 'RebootDBInstance' , or the next failure reboot. Review the table of parameters in <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.DBInstance.Modifying.html Modifying a DB Instance> in the /Amazon RDS User Guide./ to see the impact of enabling or disabling @ApplyImmediately@ for each modified parameter and to determine when the changes are applied. 
mdiApplyImmediately :: Lens' ModifyDBInstance (Maybe Bool)
mdiApplyImmediately = lens _mdiApplyImmediately (\ s a -> s{_mdiApplyImmediately = a})

-- | Indicates that the DB instance should be associated with the specified option group. Changing this parameter doesn't result in an outage except in the following case and the change is applied during the next maintenance window unless the @ApplyImmediately@ parameter is enabled for this request. If the parameter change results in an option group that enables OEM, this change can cause a brief (sub-second) period during which new connections are rejected but existing connections are not interrupted.  Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group, and that option group can't be removed from a DB instance once it is associated with a DB instance
mdiOptionGroupName :: Lens' ModifyDBInstance (Maybe Text)
mdiOptionGroupName = lens _mdiOptionGroupName (\ s a -> s{_mdiOptionGroupName = a})

-- | A value that indicates whether to copy all tags from the DB instance to snapshots of the DB instance. By default, tags are not copied. __Amazon Aurora__  Not applicable. Copying tags to snapshots is managed by the DB cluster. Setting this value for an Aurora DB instance has no effect on the DB cluster setting. For more information, see @ModifyDBCluster@ .
mdiCopyTagsToSnapshot :: Lens' ModifyDBInstance (Maybe Bool)
mdiCopyTagsToSnapshot = lens _mdiCopyTagsToSnapshot (\ s a -> s{_mdiCopyTagsToSnapshot = a})

-- | The ARN from the key store with which to associate the instance for TDE encryption.
mdiTDECredentialARN :: Lens' ModifyDBInstance (Maybe Text)
mdiTDECredentialARN = lens _mdiTDECredentialARN (\ s a -> s{_mdiTDECredentialARN = a})

-- | The name of the IAM role to use when making API calls to the Directory Service.
mdiDomainIAMRoleName :: Lens' ModifyDBInstance (Maybe Text)
mdiDomainIAMRoleName = lens _mdiDomainIAMRoleName (\ s a -> s{_mdiDomainIAMRoleName = a})

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled. For information about the supported DB engines, see 'CreateDBInstance' . For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./ 
mdiEnableIAMDatabaseAuthentication :: Lens' ModifyDBInstance (Maybe Bool)
mdiEnableIAMDatabaseAuthentication = lens _mdiEnableIAMDatabaseAuthentication (\ s a -> s{_mdiEnableIAMDatabaseAuthentication = a})

-- | A value that indicates whether the DB instance class of the DB instance uses its default processor features.
mdiUseDefaultProcessorFeatures :: Lens' ModifyDBInstance (Maybe Bool)
mdiUseDefaultProcessorFeatures = lens _mdiUseDefaultProcessorFeatures (\ s a -> s{_mdiUseDefaultProcessorFeatures = a})

-- | Specifies the storage type to be associated with the DB instance.  If you specify Provisioned IOPS (@io1@ ), you must also include a value for the @Iops@ parameter.  If you choose to migrate your DB instance from using standard storage to using Provisioned IOPS, or from using Provisioned IOPS to using standard storage, the process can take time. The duration of the migration depends on several factors such as database load, storage size, storage type (standard or Provisioned IOPS), amount of IOPS provisioned (if any), and the number of prior scale storage operations. Typical migration times are under 24 hours, but the process can take up to several days in some cases. During the migration, the DB instance is available for use, but might experience performance degradation. While the migration takes place, nightly backups for the instance are suspended. No other Amazon RDS operations can take place for the instance, including modifying the instance, rebooting the instance, deleting the instance, creating a read replica for the instance, and creating a DB snapshot of the instance.  Valid values: @standard | gp2 | io1@  Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@ 
mdiStorageType :: Lens' ModifyDBInstance (Maybe Text)
mdiStorageType = lens _mdiStorageType (\ s a -> s{_mdiStorageType = a})

-- | The DB instance identifier. This value is stored as a lowercase string. Constraints:     * Must match the identifier of an existing DBInstance.
mdiDBInstanceIdentifier :: Lens' ModifyDBInstance Text
mdiDBInstanceIdentifier = lens _mdiDBInstanceIdentifier (\ s a -> s{_mdiDBInstanceIdentifier = a})

instance AWSRequest ModifyDBInstance where
        type Rs ModifyDBInstance = ModifyDBInstanceResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "ModifyDBInstanceResult"
              (\ s h x ->
                 ModifyDBInstanceResponse' <$>
                   (x .@? "DBInstance") <*> (pure (fromEnum s)))

instance Hashable ModifyDBInstance where

instance NFData ModifyDBInstance where

instance ToHeaders ModifyDBInstance where
        toHeaders = const mempty

instance ToPath ModifyDBInstance where
        toPath = const "/"

instance ToQuery ModifyDBInstance where
        toQuery ModifyDBInstance'{..}
          = mconcat
              ["Action" =: ("ModifyDBInstance" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "EngineVersion" =: _mdiEngineVersion,
               "DBSecurityGroups" =:
                 toQuery
                   (toQueryList "DBSecurityGroupName" <$>
                      _mdiDBSecurityGroups),
               "DBPortNumber" =: _mdiDBPortNumber,
               "DeletionProtection" =: _mdiDeletionProtection,
               "MasterUserPassword" =: _mdiMasterUserPassword,
               "PubliclyAccessible" =: _mdiPubliclyAccessible,
               "AutoMinorVersionUpgrade" =:
                 _mdiAutoMinorVersionUpgrade,
               "DBSubnetGroupName" =: _mdiDBSubnetGroupName,
               "MonitoringRoleArn" =: _mdiMonitoringRoleARN,
               "Iops" =: _mdiIOPS,
               "AllowMajorVersionUpgrade" =:
                 _mdiAllowMajorVersionUpgrade,
               "NewDBInstanceIdentifier" =:
                 _mdiNewDBInstanceIdentifier,
               "Domain" =: _mdiDomain,
               "MonitoringInterval" =: _mdiMonitoringInterval,
               "CloudwatchLogsExportConfiguration" =:
                 _mdiCloudwatchLogsExportConfiguration,
               "CertificateRotationRestart" =:
                 _mdiCertificateRotationRestart,
               "TdeCredentialPassword" =: _mdiTDECredentialPassword,
               "ProcessorFeatures" =:
                 toQuery
                   (toQueryList "ProcessorFeature" <$>
                      _mdiProcessorFeatures),
               "DBInstanceClass" =: _mdiDBInstanceClass,
               "PromotionTier" =: _mdiPromotionTier,
               "LicenseModel" =: _mdiLicenseModel,
               "PreferredMaintenanceWindow" =:
                 _mdiPreferredMaintenanceWindow,
               "PerformanceInsightsRetentionPeriod" =:
                 _mdiPerformanceInsightsRetentionPeriod,
               "CACertificateIdentifier" =:
                 _mdiCACertificateIdentifier,
               "MaxAllocatedStorage" =: _mdiMaxAllocatedStorage,
               "EnablePerformanceInsights" =:
                 _mdiEnablePerformanceInsights,
               "DBParameterGroupName" =: _mdiDBParameterGroupName,
               "PreferredBackupWindow" =: _mdiPreferredBackupWindow,
               "BackupRetentionPeriod" =: _mdiBackupRetentionPeriod,
               "PerformanceInsightsKMSKeyId" =:
                 _mdiPerformanceInsightsKMSKeyId,
               "VpcSecurityGroupIds" =:
                 toQuery
                   (toQueryList "VpcSecurityGroupId" <$>
                      _mdiVPCSecurityGroupIds),
               "MultiAZ" =: _mdiMultiAZ,
               "AllocatedStorage" =: _mdiAllocatedStorage,
               "ApplyImmediately" =: _mdiApplyImmediately,
               "OptionGroupName" =: _mdiOptionGroupName,
               "CopyTagsToSnapshot" =: _mdiCopyTagsToSnapshot,
               "TdeCredentialArn" =: _mdiTDECredentialARN,
               "DomainIAMRoleName" =: _mdiDomainIAMRoleName,
               "EnableIAMDatabaseAuthentication" =:
                 _mdiEnableIAMDatabaseAuthentication,
               "UseDefaultProcessorFeatures" =:
                 _mdiUseDefaultProcessorFeatures,
               "StorageType" =: _mdiStorageType,
               "DBInstanceIdentifier" =: _mdiDBInstanceIdentifier]

-- | /See:/ 'modifyDBInstanceResponse' smart constructor.
data ModifyDBInstanceResponse = ModifyDBInstanceResponse'{_mdirsDBInstance
                                                          ::
                                                          !(Maybe DBInstance),
                                                          _mdirsResponseStatus
                                                          :: !Int}
                                  deriving (Eq, Read, Show, Data, Typeable,
                                            Generic)

-- | Creates a value of 'ModifyDBInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdirsDBInstance' - Undocumented member.
--
-- * 'mdirsResponseStatus' - -- | The response status code.
modifyDBInstanceResponse
    :: Int -- ^ 'mdirsResponseStatus'
    -> ModifyDBInstanceResponse
modifyDBInstanceResponse pResponseStatus_
  = ModifyDBInstanceResponse'{_mdirsDBInstance =
                                Nothing,
                              _mdirsResponseStatus = pResponseStatus_}

-- | Undocumented member.
mdirsDBInstance :: Lens' ModifyDBInstanceResponse (Maybe DBInstance)
mdirsDBInstance = lens _mdirsDBInstance (\ s a -> s{_mdirsDBInstance = a})

-- | -- | The response status code.
mdirsResponseStatus :: Lens' ModifyDBInstanceResponse Int
mdirsResponseStatus = lens _mdirsResponseStatus (\ s a -> s{_mdirsResponseStatus = a})

instance NFData ModifyDBInstanceResponse where

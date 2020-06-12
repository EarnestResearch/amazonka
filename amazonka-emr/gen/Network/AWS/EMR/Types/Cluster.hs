{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Cluster
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.Cluster where

import Network.AWS.EMR.Types.Application
import Network.AWS.EMR.Types.ClusterStatus
import Network.AWS.EMR.Types.Configuration
import Network.AWS.EMR.Types.EC2InstanceAttributes
import Network.AWS.EMR.Types.InstanceCollectionType
import Network.AWS.EMR.Types.KerberosAttributes
import Network.AWS.EMR.Types.RepoUpgradeOnBoot
import Network.AWS.EMR.Types.ScaleDownBehavior
import Network.AWS.EMR.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The detailed description of the cluster.
--
--
--
-- /See:/ 'cluster' smart constructor.
data Cluster = Cluster'{_cluRequestedAMIVersion ::
                        !(Maybe Text),
                        _cluEBSRootVolumeSize :: !(Maybe Int),
                        _cluEC2InstanceAttributes ::
                        !(Maybe EC2InstanceAttributes),
                        _cluNormalizedInstanceHours :: !(Maybe Int),
                        _cluConfigurations :: !(Maybe [Configuration]),
                        _cluCustomAMIId :: !(Maybe Text),
                        _cluAutoScalingRole :: !(Maybe Text),
                        _cluSecurityConfiguration :: !(Maybe Text),
                        _cluScaleDownBehavior :: !(Maybe ScaleDownBehavior),
                        _cluInstanceCollectionType ::
                        !(Maybe InstanceCollectionType),
                        _cluReleaseLabel :: !(Maybe Text),
                        _cluRepoUpgradeOnBoot :: !(Maybe RepoUpgradeOnBoot),
                        _cluLogURI :: !(Maybe Text),
                        _cluKerberosAttributes ::
                        !(Maybe KerberosAttributes),
                        _cluRunningAMIVersion :: !(Maybe Text),
                        _cluMasterPublicDNSName :: !(Maybe Text),
                        _cluTerminationProtected :: !(Maybe Bool),
                        _cluVisibleToAllUsers :: !(Maybe Bool),
                        _cluAutoTerminate :: !(Maybe Bool),
                        _cluApplications :: !(Maybe [Application]),
                        _cluTags :: !(Maybe [Tag]),
                        _cluServiceRole :: !(Maybe Text), _cluId :: !Text,
                        _cluName :: !Text, _cluStatus :: !ClusterStatus}
                 deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Cluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cluRequestedAMIVersion' - The AMI version requested for this cluster.
--
-- * 'cluEBSRootVolumeSize' - The size, in GiB, of the EBS root device volume of the Linux AMI that is used for each EC2 instance. Available in Amazon EMR version 4.x and later.
--
-- * 'cluEC2InstanceAttributes' - Provides information about the EC2 instances in a cluster grouped by category. For example, key name, subnet ID, IAM instance profile, and so on.
--
-- * 'cluNormalizedInstanceHours' - An approximation of the cost of the cluster, represented in m1.small/hours. This value is incremented one time for every hour an m1.small instance runs. Larger instances are weighted more, so an EC2 instance that is roughly four times more expensive would result in the normalized instance hours being incremented by four. This result is only an approximation and does not reflect the actual billing rate.
--
-- * 'cluConfigurations' - Applies only to Amazon EMR releases 4.x and later. The list of Configurations supplied to the EMR cluster.
--
-- * 'cluCustomAMIId' - Available only in Amazon EMR version 5.7.0 and later. The ID of a custom Amazon EBS-backed Linux AMI if the cluster uses a custom AMI.
--
-- * 'cluAutoScalingRole' - An IAM role for automatic scaling policies. The default role is @EMR_AutoScaling_DefaultRole@ . The IAM role provides permissions that the automatic scaling feature requires to launch and terminate EC2 instances in an instance group.
--
-- * 'cluSecurityConfiguration' - The name of the security configuration applied to the cluster.
--
-- * 'cluScaleDownBehavior' - The way that individual Amazon EC2 instances terminate when an automatic scale-in activity occurs or an instance group is resized. @TERMINATE_AT_INSTANCE_HOUR@ indicates that Amazon EMR terminates nodes at the instance-hour boundary, regardless of when the request to terminate the instance was submitted. This option is only available with Amazon EMR 5.1.0 and later and is the default for clusters created using that version. @TERMINATE_AT_TASK_COMPLETION@ indicates that Amazon EMR blacklists and drains tasks from nodes before terminating the Amazon EC2 instances, regardless of the instance-hour boundary. With either behavior, Amazon EMR removes the least active nodes first and blocks instance termination if it could lead to HDFS corruption. @TERMINATE_AT_TASK_COMPLETION@ is available only in Amazon EMR version 4.1.0 and later, and is the default for versions of Amazon EMR earlier than 5.1.0.
--
-- * 'cluInstanceCollectionType' - The instance group configuration of the cluster. A value of @INSTANCE_GROUP@ indicates a uniform instance group configuration. A value of @INSTANCE_FLEET@ indicates an instance fleets configuration.
--
-- * 'cluReleaseLabel' - The release label for the Amazon EMR release.
--
-- * 'cluRepoUpgradeOnBoot' - Applies only when @CustomAmiID@ is used. Specifies the type of updates that are applied from the Amazon Linux AMI package repositories when an instance boots using the AMI.
--
-- * 'cluLogURI' - The path to the Amazon S3 location where logs for this cluster are stored.
--
-- * 'cluKerberosAttributes' - Attributes for Kerberos configuration when Kerberos authentication is enabled using a security configuration. For more information see <http://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication> in the /EMR Management Guide/ .
--
-- * 'cluRunningAMIVersion' - The AMI version running on this cluster.
--
-- * 'cluMasterPublicDNSName' - The DNS name of the master node. If the cluster is on a private subnet, this is the private DNS name. On a public subnet, this is the public DNS name.
--
-- * 'cluTerminationProtected' - Indicates whether Amazon EMR will lock the cluster to prevent the EC2 instances from being terminated by an API call or user intervention, or in the event of a cluster error.
--
-- * 'cluVisibleToAllUsers' - Indicates whether the cluster is visible to all IAM users of the AWS account associated with the cluster. If this value is set to @true@ , all IAM users of that AWS account can view and manage the cluster if they have the proper policy permissions set. If this value is @false@ , only the IAM user that created the cluster can view and manage it. This value can be changed using the 'SetVisibleToAllUsers' action.
--
-- * 'cluAutoTerminate' - Specifies whether the cluster should terminate after completing all steps.
--
-- * 'cluApplications' - The applications installed on this cluster.
--
-- * 'cluTags' - A list of tags associated with a cluster.
--
-- * 'cluServiceRole' - The IAM role that will be assumed by the Amazon EMR service to access AWS resources on your behalf.
--
-- * 'cluId' - The unique identifier for the cluster.
--
-- * 'cluName' - The name of the cluster.
--
-- * 'cluStatus' - The current status details about the cluster.
cluster
    :: Text -- ^ 'cluId'
    -> Text -- ^ 'cluName'
    -> ClusterStatus -- ^ 'cluStatus'
    -> Cluster
cluster pId_ pName_ pStatus_
  = Cluster'{_cluRequestedAMIVersion = Nothing,
             _cluEBSRootVolumeSize = Nothing,
             _cluEC2InstanceAttributes = Nothing,
             _cluNormalizedInstanceHours = Nothing,
             _cluConfigurations = Nothing,
             _cluCustomAMIId = Nothing,
             _cluAutoScalingRole = Nothing,
             _cluSecurityConfiguration = Nothing,
             _cluScaleDownBehavior = Nothing,
             _cluInstanceCollectionType = Nothing,
             _cluReleaseLabel = Nothing,
             _cluRepoUpgradeOnBoot = Nothing,
             _cluLogURI = Nothing,
             _cluKerberosAttributes = Nothing,
             _cluRunningAMIVersion = Nothing,
             _cluMasterPublicDNSName = Nothing,
             _cluTerminationProtected = Nothing,
             _cluVisibleToAllUsers = Nothing,
             _cluAutoTerminate = Nothing,
             _cluApplications = Nothing, _cluTags = Nothing,
             _cluServiceRole = Nothing, _cluId = pId_,
             _cluName = pName_, _cluStatus = pStatus_}

-- | The AMI version requested for this cluster.
cluRequestedAMIVersion :: Lens' Cluster (Maybe Text)
cluRequestedAMIVersion = lens _cluRequestedAMIVersion (\ s a -> s{_cluRequestedAMIVersion = a})

-- | The size, in GiB, of the EBS root device volume of the Linux AMI that is used for each EC2 instance. Available in Amazon EMR version 4.x and later.
cluEBSRootVolumeSize :: Lens' Cluster (Maybe Int)
cluEBSRootVolumeSize = lens _cluEBSRootVolumeSize (\ s a -> s{_cluEBSRootVolumeSize = a})

-- | Provides information about the EC2 instances in a cluster grouped by category. For example, key name, subnet ID, IAM instance profile, and so on.
cluEC2InstanceAttributes :: Lens' Cluster (Maybe EC2InstanceAttributes)
cluEC2InstanceAttributes = lens _cluEC2InstanceAttributes (\ s a -> s{_cluEC2InstanceAttributes = a})

-- | An approximation of the cost of the cluster, represented in m1.small/hours. This value is incremented one time for every hour an m1.small instance runs. Larger instances are weighted more, so an EC2 instance that is roughly four times more expensive would result in the normalized instance hours being incremented by four. This result is only an approximation and does not reflect the actual billing rate.
cluNormalizedInstanceHours :: Lens' Cluster (Maybe Int)
cluNormalizedInstanceHours = lens _cluNormalizedInstanceHours (\ s a -> s{_cluNormalizedInstanceHours = a})

-- | Applies only to Amazon EMR releases 4.x and later. The list of Configurations supplied to the EMR cluster.
cluConfigurations :: Lens' Cluster [Configuration]
cluConfigurations = lens _cluConfigurations (\ s a -> s{_cluConfigurations = a}) . _Default . _Coerce

-- | Available only in Amazon EMR version 5.7.0 and later. The ID of a custom Amazon EBS-backed Linux AMI if the cluster uses a custom AMI.
cluCustomAMIId :: Lens' Cluster (Maybe Text)
cluCustomAMIId = lens _cluCustomAMIId (\ s a -> s{_cluCustomAMIId = a})

-- | An IAM role for automatic scaling policies. The default role is @EMR_AutoScaling_DefaultRole@ . The IAM role provides permissions that the automatic scaling feature requires to launch and terminate EC2 instances in an instance group.
cluAutoScalingRole :: Lens' Cluster (Maybe Text)
cluAutoScalingRole = lens _cluAutoScalingRole (\ s a -> s{_cluAutoScalingRole = a})

-- | The name of the security configuration applied to the cluster.
cluSecurityConfiguration :: Lens' Cluster (Maybe Text)
cluSecurityConfiguration = lens _cluSecurityConfiguration (\ s a -> s{_cluSecurityConfiguration = a})

-- | The way that individual Amazon EC2 instances terminate when an automatic scale-in activity occurs or an instance group is resized. @TERMINATE_AT_INSTANCE_HOUR@ indicates that Amazon EMR terminates nodes at the instance-hour boundary, regardless of when the request to terminate the instance was submitted. This option is only available with Amazon EMR 5.1.0 and later and is the default for clusters created using that version. @TERMINATE_AT_TASK_COMPLETION@ indicates that Amazon EMR blacklists and drains tasks from nodes before terminating the Amazon EC2 instances, regardless of the instance-hour boundary. With either behavior, Amazon EMR removes the least active nodes first and blocks instance termination if it could lead to HDFS corruption. @TERMINATE_AT_TASK_COMPLETION@ is available only in Amazon EMR version 4.1.0 and later, and is the default for versions of Amazon EMR earlier than 5.1.0.
cluScaleDownBehavior :: Lens' Cluster (Maybe ScaleDownBehavior)
cluScaleDownBehavior = lens _cluScaleDownBehavior (\ s a -> s{_cluScaleDownBehavior = a})

-- | The instance group configuration of the cluster. A value of @INSTANCE_GROUP@ indicates a uniform instance group configuration. A value of @INSTANCE_FLEET@ indicates an instance fleets configuration.
cluInstanceCollectionType :: Lens' Cluster (Maybe InstanceCollectionType)
cluInstanceCollectionType = lens _cluInstanceCollectionType (\ s a -> s{_cluInstanceCollectionType = a})

-- | The release label for the Amazon EMR release.
cluReleaseLabel :: Lens' Cluster (Maybe Text)
cluReleaseLabel = lens _cluReleaseLabel (\ s a -> s{_cluReleaseLabel = a})

-- | Applies only when @CustomAmiID@ is used. Specifies the type of updates that are applied from the Amazon Linux AMI package repositories when an instance boots using the AMI.
cluRepoUpgradeOnBoot :: Lens' Cluster (Maybe RepoUpgradeOnBoot)
cluRepoUpgradeOnBoot = lens _cluRepoUpgradeOnBoot (\ s a -> s{_cluRepoUpgradeOnBoot = a})

-- | The path to the Amazon S3 location where logs for this cluster are stored.
cluLogURI :: Lens' Cluster (Maybe Text)
cluLogURI = lens _cluLogURI (\ s a -> s{_cluLogURI = a})

-- | Attributes for Kerberos configuration when Kerberos authentication is enabled using a security configuration. For more information see <http://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication> in the /EMR Management Guide/ .
cluKerberosAttributes :: Lens' Cluster (Maybe KerberosAttributes)
cluKerberosAttributes = lens _cluKerberosAttributes (\ s a -> s{_cluKerberosAttributes = a})

-- | The AMI version running on this cluster.
cluRunningAMIVersion :: Lens' Cluster (Maybe Text)
cluRunningAMIVersion = lens _cluRunningAMIVersion (\ s a -> s{_cluRunningAMIVersion = a})

-- | The DNS name of the master node. If the cluster is on a private subnet, this is the private DNS name. On a public subnet, this is the public DNS name.
cluMasterPublicDNSName :: Lens' Cluster (Maybe Text)
cluMasterPublicDNSName = lens _cluMasterPublicDNSName (\ s a -> s{_cluMasterPublicDNSName = a})

-- | Indicates whether Amazon EMR will lock the cluster to prevent the EC2 instances from being terminated by an API call or user intervention, or in the event of a cluster error.
cluTerminationProtected :: Lens' Cluster (Maybe Bool)
cluTerminationProtected = lens _cluTerminationProtected (\ s a -> s{_cluTerminationProtected = a})

-- | Indicates whether the cluster is visible to all IAM users of the AWS account associated with the cluster. If this value is set to @true@ , all IAM users of that AWS account can view and manage the cluster if they have the proper policy permissions set. If this value is @false@ , only the IAM user that created the cluster can view and manage it. This value can be changed using the 'SetVisibleToAllUsers' action.
cluVisibleToAllUsers :: Lens' Cluster (Maybe Bool)
cluVisibleToAllUsers = lens _cluVisibleToAllUsers (\ s a -> s{_cluVisibleToAllUsers = a})

-- | Specifies whether the cluster should terminate after completing all steps.
cluAutoTerminate :: Lens' Cluster (Maybe Bool)
cluAutoTerminate = lens _cluAutoTerminate (\ s a -> s{_cluAutoTerminate = a})

-- | The applications installed on this cluster.
cluApplications :: Lens' Cluster [Application]
cluApplications = lens _cluApplications (\ s a -> s{_cluApplications = a}) . _Default . _Coerce

-- | A list of tags associated with a cluster.
cluTags :: Lens' Cluster [Tag]
cluTags = lens _cluTags (\ s a -> s{_cluTags = a}) . _Default . _Coerce

-- | The IAM role that will be assumed by the Amazon EMR service to access AWS resources on your behalf.
cluServiceRole :: Lens' Cluster (Maybe Text)
cluServiceRole = lens _cluServiceRole (\ s a -> s{_cluServiceRole = a})

-- | The unique identifier for the cluster.
cluId :: Lens' Cluster Text
cluId = lens _cluId (\ s a -> s{_cluId = a})

-- | The name of the cluster.
cluName :: Lens' Cluster Text
cluName = lens _cluName (\ s a -> s{_cluName = a})

-- | The current status details about the cluster.
cluStatus :: Lens' Cluster ClusterStatus
cluStatus = lens _cluStatus (\ s a -> s{_cluStatus = a})

instance FromJSON Cluster where
        parseJSON
          = withObject "Cluster"
              (\ x ->
                 Cluster' <$>
                   (x .:? "RequestedAmiVersion") <*>
                     (x .:? "EbsRootVolumeSize")
                     <*> (x .:? "Ec2InstanceAttributes")
                     <*> (x .:? "NormalizedInstanceHours")
                     <*> (x .:? "Configurations" .!= mempty)
                     <*> (x .:? "CustomAmiId")
                     <*> (x .:? "AutoScalingRole")
                     <*> (x .:? "SecurityConfiguration")
                     <*> (x .:? "ScaleDownBehavior")
                     <*> (x .:? "InstanceCollectionType")
                     <*> (x .:? "ReleaseLabel")
                     <*> (x .:? "RepoUpgradeOnBoot")
                     <*> (x .:? "LogUri")
                     <*> (x .:? "KerberosAttributes")
                     <*> (x .:? "RunningAmiVersion")
                     <*> (x .:? "MasterPublicDnsName")
                     <*> (x .:? "TerminationProtected")
                     <*> (x .:? "VisibleToAllUsers")
                     <*> (x .:? "AutoTerminate")
                     <*> (x .:? "Applications" .!= mempty)
                     <*> (x .:? "Tags" .!= mempty)
                     <*> (x .:? "ServiceRole")
                     <*> (x .: "Id")
                     <*> (x .: "Name")
                     <*> (x .: "Status"))

instance Hashable Cluster where

instance NFData Cluster where

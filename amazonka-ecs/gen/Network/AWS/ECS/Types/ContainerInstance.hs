{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ContainerInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.ContainerInstance where

import Network.AWS.ECS.Types.AgentUpdateStatus
import Network.AWS.ECS.Types.Attachment
import Network.AWS.ECS.Types.Attribute
import Network.AWS.ECS.Types.Resource
import Network.AWS.ECS.Types.VersionInfo
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An EC2 instance that is running the Amazon ECS agent and has been registered with a cluster.
--
--
--
-- /See:/ 'containerInstance' smart constructor.
data ContainerInstance = ContainerInstance'{_ciStatus
                                            :: !(Maybe Text),
                                            _ciAttachments ::
                                            !(Maybe [Attachment]),
                                            _ciRunningTasksCount ::
                                            !(Maybe Int),
                                            _ciRemainingResources ::
                                            !(Maybe [Resource]),
                                            _ciEc2InstanceId :: !(Maybe Text),
                                            _ciContainerInstanceARN ::
                                            !(Maybe Text),
                                            _ciAgentConnected :: !(Maybe Bool),
                                            _ciVersionInfo ::
                                            !(Maybe VersionInfo),
                                            _ciAgentUpdateStatus ::
                                            !(Maybe AgentUpdateStatus),
                                            _ciAttributes ::
                                            !(Maybe [Attribute]),
                                            _ciVersion :: !(Maybe Integer),
                                            _ciPendingTasksCount ::
                                            !(Maybe Int),
                                            _ciRegisteredAt :: !(Maybe POSIX),
                                            _ciRegisteredResources ::
                                            !(Maybe [Resource])}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContainerInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciStatus' - The status of the container instance. The valid values are @ACTIVE@ , @INACTIVE@ , or @DRAINING@ . @ACTIVE@ indicates that the container instance can accept tasks. @DRAINING@ indicates that new tasks are not placed on the container instance and any service tasks running on the container instance are removed if possible. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/container-instance-draining.html Container Instance Draining> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'ciAttachments' - The Elastic Network Interfaces associated with the container instance.
--
-- * 'ciRunningTasksCount' - The number of tasks on the container instance that are in the @RUNNING@ status.
--
-- * 'ciRemainingResources' - For CPU and memory resource types, this parameter describes the remaining CPU and memory on the that has not already been allocated to tasks (and is therefore available for new tasks). For port resource types, this parameter describes the ports that were reserved by the Amazon ECS container agent (at instance registration time) and any task containers that have reserved port mappings on the host (with the @host@ or @bridge@ network mode). Any port that is not specified here is available for new tasks.
--
-- * 'ciEc2InstanceId' - The EC2 instance ID of the container instance.
--
-- * 'ciContainerInstanceARN' - The Amazon Resource Name (ARN) of the container instance. The ARN contains the @arn:aws:ecs@ namespace, followed by the region of the container instance, the AWS account ID of the container instance owner, the @container-instance@ namespace, and then the container instance ID. For example, @arn:aws:ecs:/region/ :/aws_account_id/ :container-instance//container_instance_ID/ @ .
--
-- * 'ciAgentConnected' - This parameter returns @true@ if the agent is connected to Amazon ECS. Registered instances with an agent that may be unhealthy or stopped return @false@ . Instances without a connected agent can't accept placement requests.
--
-- * 'ciVersionInfo' - The version information for the Amazon ECS container agent and Docker daemon running on the container instance.
--
-- * 'ciAgentUpdateStatus' - The status of the most recent agent update. If an update has never been requested, this value is @NULL@ .
--
-- * 'ciAttributes' - The attributes set for the container instance, either by the Amazon ECS container agent at instance registration or manually with the 'PutAttributes' operation.
--
-- * 'ciVersion' - The version counter for the container instance. Every time a container instance experiences a change that triggers a CloudWatch event, the version counter is incremented. If you are replicating your Amazon ECS container instance state with CloudWatch Events, you can compare the version of a container instance reported by the Amazon ECS APIs with the version reported in CloudWatch Events for the container instance (inside the @detail@ object) to verify that the version in your event stream is current.
--
-- * 'ciPendingTasksCount' - The number of tasks on the container instance that are in the @PENDING@ status.
--
-- * 'ciRegisteredAt' - The Unix time stamp for when the container instance was registered.
--
-- * 'ciRegisteredResources' - For CPU and memory resource types, this parameter describes the amount of each resource that was available on the container instance when the container agent registered it with Amazon ECS; this value represents the total amount of CPU and memory that can be allocated on this container instance to tasks. For port resource types, this parameter describes the ports that were reserved by the Amazon ECS container agent when it registered the container instance with Amazon ECS.
containerInstance
    :: ContainerInstance
containerInstance
  = ContainerInstance'{_ciStatus = Nothing,
                       _ciAttachments = Nothing,
                       _ciRunningTasksCount = Nothing,
                       _ciRemainingResources = Nothing,
                       _ciEc2InstanceId = Nothing,
                       _ciContainerInstanceARN = Nothing,
                       _ciAgentConnected = Nothing,
                       _ciVersionInfo = Nothing,
                       _ciAgentUpdateStatus = Nothing,
                       _ciAttributes = Nothing, _ciVersion = Nothing,
                       _ciPendingTasksCount = Nothing,
                       _ciRegisteredAt = Nothing,
                       _ciRegisteredResources = Nothing}

-- | The status of the container instance. The valid values are @ACTIVE@ , @INACTIVE@ , or @DRAINING@ . @ACTIVE@ indicates that the container instance can accept tasks. @DRAINING@ indicates that new tasks are not placed on the container instance and any service tasks running on the container instance are removed if possible. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/container-instance-draining.html Container Instance Draining> in the /Amazon Elastic Container Service Developer Guide/ .
ciStatus :: Lens' ContainerInstance (Maybe Text)
ciStatus = lens _ciStatus (\ s a -> s{_ciStatus = a})

-- | The Elastic Network Interfaces associated with the container instance.
ciAttachments :: Lens' ContainerInstance [Attachment]
ciAttachments = lens _ciAttachments (\ s a -> s{_ciAttachments = a}) . _Default . _Coerce

-- | The number of tasks on the container instance that are in the @RUNNING@ status.
ciRunningTasksCount :: Lens' ContainerInstance (Maybe Int)
ciRunningTasksCount = lens _ciRunningTasksCount (\ s a -> s{_ciRunningTasksCount = a})

-- | For CPU and memory resource types, this parameter describes the remaining CPU and memory on the that has not already been allocated to tasks (and is therefore available for new tasks). For port resource types, this parameter describes the ports that were reserved by the Amazon ECS container agent (at instance registration time) and any task containers that have reserved port mappings on the host (with the @host@ or @bridge@ network mode). Any port that is not specified here is available for new tasks.
ciRemainingResources :: Lens' ContainerInstance [Resource]
ciRemainingResources = lens _ciRemainingResources (\ s a -> s{_ciRemainingResources = a}) . _Default . _Coerce

-- | The EC2 instance ID of the container instance.
ciEc2InstanceId :: Lens' ContainerInstance (Maybe Text)
ciEc2InstanceId = lens _ciEc2InstanceId (\ s a -> s{_ciEc2InstanceId = a})

-- | The Amazon Resource Name (ARN) of the container instance. The ARN contains the @arn:aws:ecs@ namespace, followed by the region of the container instance, the AWS account ID of the container instance owner, the @container-instance@ namespace, and then the container instance ID. For example, @arn:aws:ecs:/region/ :/aws_account_id/ :container-instance//container_instance_ID/ @ .
ciContainerInstanceARN :: Lens' ContainerInstance (Maybe Text)
ciContainerInstanceARN = lens _ciContainerInstanceARN (\ s a -> s{_ciContainerInstanceARN = a})

-- | This parameter returns @true@ if the agent is connected to Amazon ECS. Registered instances with an agent that may be unhealthy or stopped return @false@ . Instances without a connected agent can't accept placement requests.
ciAgentConnected :: Lens' ContainerInstance (Maybe Bool)
ciAgentConnected = lens _ciAgentConnected (\ s a -> s{_ciAgentConnected = a})

-- | The version information for the Amazon ECS container agent and Docker daemon running on the container instance.
ciVersionInfo :: Lens' ContainerInstance (Maybe VersionInfo)
ciVersionInfo = lens _ciVersionInfo (\ s a -> s{_ciVersionInfo = a})

-- | The status of the most recent agent update. If an update has never been requested, this value is @NULL@ .
ciAgentUpdateStatus :: Lens' ContainerInstance (Maybe AgentUpdateStatus)
ciAgentUpdateStatus = lens _ciAgentUpdateStatus (\ s a -> s{_ciAgentUpdateStatus = a})

-- | The attributes set for the container instance, either by the Amazon ECS container agent at instance registration or manually with the 'PutAttributes' operation.
ciAttributes :: Lens' ContainerInstance [Attribute]
ciAttributes = lens _ciAttributes (\ s a -> s{_ciAttributes = a}) . _Default . _Coerce

-- | The version counter for the container instance. Every time a container instance experiences a change that triggers a CloudWatch event, the version counter is incremented. If you are replicating your Amazon ECS container instance state with CloudWatch Events, you can compare the version of a container instance reported by the Amazon ECS APIs with the version reported in CloudWatch Events for the container instance (inside the @detail@ object) to verify that the version in your event stream is current.
ciVersion :: Lens' ContainerInstance (Maybe Integer)
ciVersion = lens _ciVersion (\ s a -> s{_ciVersion = a})

-- | The number of tasks on the container instance that are in the @PENDING@ status.
ciPendingTasksCount :: Lens' ContainerInstance (Maybe Int)
ciPendingTasksCount = lens _ciPendingTasksCount (\ s a -> s{_ciPendingTasksCount = a})

-- | The Unix time stamp for when the container instance was registered.
ciRegisteredAt :: Lens' ContainerInstance (Maybe UTCTime)
ciRegisteredAt = lens _ciRegisteredAt (\ s a -> s{_ciRegisteredAt = a}) . mapping _Time

-- | For CPU and memory resource types, this parameter describes the amount of each resource that was available on the container instance when the container agent registered it with Amazon ECS; this value represents the total amount of CPU and memory that can be allocated on this container instance to tasks. For port resource types, this parameter describes the ports that were reserved by the Amazon ECS container agent when it registered the container instance with Amazon ECS.
ciRegisteredResources :: Lens' ContainerInstance [Resource]
ciRegisteredResources = lens _ciRegisteredResources (\ s a -> s{_ciRegisteredResources = a}) . _Default . _Coerce

instance FromJSON ContainerInstance where
        parseJSON
          = withObject "ContainerInstance"
              (\ x ->
                 ContainerInstance' <$>
                   (x .:? "status") <*> (x .:? "attachments" .!= mempty)
                     <*> (x .:? "runningTasksCount")
                     <*> (x .:? "remainingResources" .!= mempty)
                     <*> (x .:? "ec2InstanceId")
                     <*> (x .:? "containerInstanceArn")
                     <*> (x .:? "agentConnected")
                     <*> (x .:? "versionInfo")
                     <*> (x .:? "agentUpdateStatus")
                     <*> (x .:? "attributes" .!= mempty)
                     <*> (x .:? "version")
                     <*> (x .:? "pendingTasksCount")
                     <*> (x .:? "registeredAt")
                     <*> (x .:? "registeredResources" .!= mempty))

instance Hashable ContainerInstance where

instance NFData ContainerInstance where

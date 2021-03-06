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
-- Module      : Network.AWS.ApplicationAutoScaling.RegisterScalableTarget
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers or updates a scalable target. 
--
--
-- A scalable target is a resource that Application Auto Scaling can scale out and scale in. Scalable targets are uniquely identified by the combination of resource ID, scalable dimension, and namespace. 
--
-- When you register a new scalable target, you must specify values for minimum and maximum capacity. Application Auto Scaling scaling policies will not scale capacity to values that are outside of this range.
--
-- After you register a scalable target, you do not need to register it again to use other Application Auto Scaling operations. To see which resources have been registered, use <https://docs.aws.amazon.com/autoscaling/application/APIReference/API_DescribeScalableTargets.html DescribeScalableTargets> . You can also view the scaling policies for a service namespace by using <https://docs.aws.amazon.com/autoscaling/application/APIReference/API_DescribeScalableTargets.html DescribeScalableTargets> . If you no longer need a scalable target, you can deregister it by using <https://docs.aws.amazon.com/autoscaling/application/APIReference/API_DeregisterScalableTarget.html DeregisterScalableTarget> .
--
-- To update a scalable target, specify the parameters that you want to change. Include the parameters that identify the scalable target: resource ID, scalable dimension, and namespace. Any parameters that you don't specify are not changed by this update request. 
--
module Network.AWS.ApplicationAutoScaling.RegisterScalableTarget
    (
    -- * Creating a Request
      registerScalableTarget
    , RegisterScalableTarget
    -- * Request Lenses
    , rstSuspendedState
    , rstMaxCapacity
    , rstMinCapacity
    , rstRoleARN
    , rstServiceNamespace
    , rstResourceId
    , rstScalableDimension

    -- * Destructuring the Response
    , registerScalableTargetResponse
    , RegisterScalableTargetResponse
    -- * Response Lenses
    , rstrsResponseStatus
    ) where

import Network.AWS.ApplicationAutoScaling.Types
import Network.AWS.ApplicationAutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'registerScalableTarget' smart constructor.
data RegisterScalableTarget = RegisterScalableTarget'{_rstSuspendedState
                                                      ::
                                                      !(Maybe SuspendedState),
                                                      _rstMaxCapacity ::
                                                      !(Maybe Int),
                                                      _rstMinCapacity ::
                                                      !(Maybe Int),
                                                      _rstRoleARN ::
                                                      !(Maybe Text),
                                                      _rstServiceNamespace ::
                                                      !ServiceNamespace,
                                                      _rstResourceId :: !Text,
                                                      _rstScalableDimension ::
                                                      !ScalableDimension}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'RegisterScalableTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rstSuspendedState' - An embedded object that contains attributes and attribute values that are used to suspend and resume automatic scaling. Setting the value of an attribute to @true@ suspends the specified scaling activities. Setting it to @false@ (default) resumes the specified scaling activities.  __Suspension Outcomes__      * For @DynamicScalingInSuspended@ , while a suspension is in effect, all scale-in activities that are triggered by a scaling policy are suspended.     * For @DynamicScalingOutSuspended@ , while a suspension is in effect, all scale-out activities that are triggered by a scaling policy are suspended.     * For @ScheduledScalingSuspended@ , while a suspension is in effect, all scaling activities that involve scheduled actions are suspended.  For more information, see <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-suspend-resume-scaling.html Suspending and Resuming Scaling> in the /Application Auto Scaling User Guide/ .
--
-- * 'rstMaxCapacity' - The maximum value that you plan to scale out to. When a scaling policy is in effect, Application Auto Scaling can scale out (expand) as needed to the maximum capacity limit in response to changing demand.  This parameter is required if you are registering a scalable target.
--
-- * 'rstMinCapacity' - The minimum value that you plan to scale in to. When a scaling policy is in effect, Application Auto Scaling can scale in (contract) as needed to the minimum capacity limit in response to changing demand.  This parameter is required if you are registering a scalable target. For Lambda provisioned concurrency, the minimum value allowed is 0. For all other resources, the minimum value allowed is 1.
--
-- * 'rstRoleARN' - This parameter is required for services that do not support service-linked roles (such as Amazon EMR), and it must specify the ARN of an IAM role that allows Application Auto Scaling to modify the scalable target on your behalf.  If the service supports service-linked roles, Application Auto Scaling uses a service-linked role, which it creates if it does not yet exist. For more information, see <https://docs.aws.amazon.com/autoscaling/application/userguide/security_iam_service-with-iam.html#security_iam_service-with-iam-roles Application Auto Scaling IAM Roles> .
--
-- * 'rstServiceNamespace' - The namespace of the AWS service that provides the resource. For a resource provided by your own application or service, use @custom-resource@ instead.
--
-- * 'rstResourceId' - The identifier of the resource that is associated with the scalable target. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the table name. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the index name. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variant - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .     * Custom resources are not supported with a resource type. This parameter must specify the @OutputValue@ from the CloudFormation template stack used to access the resources. The unique identifier is defined by the service provider. More information is available in our <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository> .     * Amazon Comprehend document classification endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:document-classifier-endpoint/EXAMPLE@ .     * Lambda provisioned concurrency - The resource type is @function@ and the unique identifier is the function name with a function version or alias name suffix that is not @> LATEST@ . Example: @function:my-function:prod@ or @function:my-function:1@ .     * Amazon Keyspaces table - The resource type is @table@ and the unique identifier is the table name. Example: @keyspace/mykeyspace/table/mytable@ .
--
-- * 'rstScalableDimension' - The scalable dimension associated with the scalable target. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.     * @custom-resource:ResourceType:Property@ - The scalable dimension for a custom resource provided by your own application or service.     * @comprehend:document-classifier-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend document classification endpoint.     * @lambda:function:ProvisionedConcurrency@ - The provisioned concurrency for a Lambda function.     * @cassandra:table:ReadCapacityUnits@ - The provisioned read capacity for an Amazon Keyspaces table.     * @cassandra:table:WriteCapacityUnits@ - The provisioned write capacity for an Amazon Keyspaces table.
registerScalableTarget
    :: ServiceNamespace -- ^ 'rstServiceNamespace'
    -> Text -- ^ 'rstResourceId'
    -> ScalableDimension -- ^ 'rstScalableDimension'
    -> RegisterScalableTarget
registerScalableTarget pServiceNamespace_
  pResourceId_ pScalableDimension_
  = RegisterScalableTarget'{_rstSuspendedState =
                              Nothing,
                            _rstMaxCapacity = Nothing,
                            _rstMinCapacity = Nothing, _rstRoleARN = Nothing,
                            _rstServiceNamespace = pServiceNamespace_,
                            _rstResourceId = pResourceId_,
                            _rstScalableDimension = pScalableDimension_}

-- | An embedded object that contains attributes and attribute values that are used to suspend and resume automatic scaling. Setting the value of an attribute to @true@ suspends the specified scaling activities. Setting it to @false@ (default) resumes the specified scaling activities.  __Suspension Outcomes__      * For @DynamicScalingInSuspended@ , while a suspension is in effect, all scale-in activities that are triggered by a scaling policy are suspended.     * For @DynamicScalingOutSuspended@ , while a suspension is in effect, all scale-out activities that are triggered by a scaling policy are suspended.     * For @ScheduledScalingSuspended@ , while a suspension is in effect, all scaling activities that involve scheduled actions are suspended.  For more information, see <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-suspend-resume-scaling.html Suspending and Resuming Scaling> in the /Application Auto Scaling User Guide/ .
rstSuspendedState :: Lens' RegisterScalableTarget (Maybe SuspendedState)
rstSuspendedState = lens _rstSuspendedState (\ s a -> s{_rstSuspendedState = a})

-- | The maximum value that you plan to scale out to. When a scaling policy is in effect, Application Auto Scaling can scale out (expand) as needed to the maximum capacity limit in response to changing demand.  This parameter is required if you are registering a scalable target.
rstMaxCapacity :: Lens' RegisterScalableTarget (Maybe Int)
rstMaxCapacity = lens _rstMaxCapacity (\ s a -> s{_rstMaxCapacity = a})

-- | The minimum value that you plan to scale in to. When a scaling policy is in effect, Application Auto Scaling can scale in (contract) as needed to the minimum capacity limit in response to changing demand.  This parameter is required if you are registering a scalable target. For Lambda provisioned concurrency, the minimum value allowed is 0. For all other resources, the minimum value allowed is 1.
rstMinCapacity :: Lens' RegisterScalableTarget (Maybe Int)
rstMinCapacity = lens _rstMinCapacity (\ s a -> s{_rstMinCapacity = a})

-- | This parameter is required for services that do not support service-linked roles (such as Amazon EMR), and it must specify the ARN of an IAM role that allows Application Auto Scaling to modify the scalable target on your behalf.  If the service supports service-linked roles, Application Auto Scaling uses a service-linked role, which it creates if it does not yet exist. For more information, see <https://docs.aws.amazon.com/autoscaling/application/userguide/security_iam_service-with-iam.html#security_iam_service-with-iam-roles Application Auto Scaling IAM Roles> .
rstRoleARN :: Lens' RegisterScalableTarget (Maybe Text)
rstRoleARN = lens _rstRoleARN (\ s a -> s{_rstRoleARN = a})

-- | The namespace of the AWS service that provides the resource. For a resource provided by your own application or service, use @custom-resource@ instead.
rstServiceNamespace :: Lens' RegisterScalableTarget ServiceNamespace
rstServiceNamespace = lens _rstServiceNamespace (\ s a -> s{_rstServiceNamespace = a})

-- | The identifier of the resource that is associated with the scalable target. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the table name. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the index name. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variant - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .     * Custom resources are not supported with a resource type. This parameter must specify the @OutputValue@ from the CloudFormation template stack used to access the resources. The unique identifier is defined by the service provider. More information is available in our <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository> .     * Amazon Comprehend document classification endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:document-classifier-endpoint/EXAMPLE@ .     * Lambda provisioned concurrency - The resource type is @function@ and the unique identifier is the function name with a function version or alias name suffix that is not @> LATEST@ . Example: @function:my-function:prod@ or @function:my-function:1@ .     * Amazon Keyspaces table - The resource type is @table@ and the unique identifier is the table name. Example: @keyspace/mykeyspace/table/mytable@ .
rstResourceId :: Lens' RegisterScalableTarget Text
rstResourceId = lens _rstResourceId (\ s a -> s{_rstResourceId = a})

-- | The scalable dimension associated with the scalable target. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.     * @custom-resource:ResourceType:Property@ - The scalable dimension for a custom resource provided by your own application or service.     * @comprehend:document-classifier-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend document classification endpoint.     * @lambda:function:ProvisionedConcurrency@ - The provisioned concurrency for a Lambda function.     * @cassandra:table:ReadCapacityUnits@ - The provisioned read capacity for an Amazon Keyspaces table.     * @cassandra:table:WriteCapacityUnits@ - The provisioned write capacity for an Amazon Keyspaces table.
rstScalableDimension :: Lens' RegisterScalableTarget ScalableDimension
rstScalableDimension = lens _rstScalableDimension (\ s a -> s{_rstScalableDimension = a})

instance AWSRequest RegisterScalableTarget where
        type Rs RegisterScalableTarget =
             RegisterScalableTargetResponse
        request = postJSON applicationAutoScaling
        response
          = receiveEmpty
              (\ s h x ->
                 RegisterScalableTargetResponse' <$>
                   (pure (fromEnum s)))

instance Hashable RegisterScalableTarget where

instance NFData RegisterScalableTarget where

instance ToHeaders RegisterScalableTarget where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AnyScaleFrontendService.RegisterScalableTarget" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RegisterScalableTarget where
        toJSON RegisterScalableTarget'{..}
          = object
              (catMaybes
                 [("SuspendedState" .=) <$> _rstSuspendedState,
                  ("MaxCapacity" .=) <$> _rstMaxCapacity,
                  ("MinCapacity" .=) <$> _rstMinCapacity,
                  ("RoleARN" .=) <$> _rstRoleARN,
                  Just ("ServiceNamespace" .= _rstServiceNamespace),
                  Just ("ResourceId" .= _rstResourceId),
                  Just ("ScalableDimension" .= _rstScalableDimension)])

instance ToPath RegisterScalableTarget where
        toPath = const "/"

instance ToQuery RegisterScalableTarget where
        toQuery = const mempty

-- | /See:/ 'registerScalableTargetResponse' smart constructor.
newtype RegisterScalableTargetResponse = RegisterScalableTargetResponse'{_rstrsResponseStatus
                                                                         :: Int}
                                           deriving (Eq, Read, Show, Data,
                                                     Typeable, Generic)

-- | Creates a value of 'RegisterScalableTargetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rstrsResponseStatus' - -- | The response status code.
registerScalableTargetResponse
    :: Int -- ^ 'rstrsResponseStatus'
    -> RegisterScalableTargetResponse
registerScalableTargetResponse pResponseStatus_
  = RegisterScalableTargetResponse'{_rstrsResponseStatus
                                      = pResponseStatus_}

-- | -- | The response status code.
rstrsResponseStatus :: Lens' RegisterScalableTargetResponse Int
rstrsResponseStatus = lens _rstrsResponseStatus (\ s a -> s{_rstrsResponseStatus = a})

instance NFData RegisterScalableTargetResponse where

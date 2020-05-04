{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApplicationAutoScaling.Types.Product where

import Network.AWS.ApplicationAutoScaling.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a CloudWatch alarm associated with a scaling policy.
--
--
--
-- /See:/ 'alarm' smart constructor.
data Alarm =
  Alarm'
    { _aAlarmName :: !Text
    , _aAlarmARN  :: !Text
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Alarm' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aAlarmName' - The name of the alarm.
--
-- * 'aAlarmARN' - The Amazon Resource Name (ARN) of the alarm.
alarm
    :: Text -- ^ 'aAlarmName'
    -> Text -- ^ 'aAlarmARN'
    -> Alarm
alarm pAlarmName_ pAlarmARN_ =
  Alarm' {_aAlarmName = pAlarmName_, _aAlarmARN = pAlarmARN_}


-- | The name of the alarm.
aAlarmName :: Lens' Alarm Text
aAlarmName = lens _aAlarmName (\ s a -> s{_aAlarmName = a})

-- | The Amazon Resource Name (ARN) of the alarm.
aAlarmARN :: Lens' Alarm Text
aAlarmARN = lens _aAlarmARN (\ s a -> s{_aAlarmARN = a})

instance FromJSON Alarm where
        parseJSON
          = withObject "Alarm"
              (\ x ->
                 Alarm' <$> (x .: "AlarmName") <*> (x .: "AlarmARN"))

instance Hashable Alarm where

instance NFData Alarm where

-- | Represents a CloudWatch metric of your choosing for a target tracking scaling policy to use with Application Auto Scaling.
--
--
-- For information about the available metrics for a service, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/aws-services-cloudwatch-metrics.html AWS Services That Publish CloudWatch Metrics> in the /Amazon CloudWatch User Guide/ .
--
-- To create your customized metric specification:
--
--     * Add values for each required parameter from CloudWatch. You can use an existing metric, or a new metric that you create. To use your own metric, you must first publish the metric to CloudWatch. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html Publish Custom Metrics> in the /Amazon CloudWatch User Guide/ .
--
--     * Choose a metric that changes proportionally with capacity. The value of the metric should increase or decrease in inverse proportion to the number of capacity units. That is, the value of the metric should decrease when capacity increases, and increase when capacity decreases.
--
--
--
-- For more information about CloudWatch, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html Amazon CloudWatch Concepts> .
--
--
-- /See:/ 'customizedMetricSpecification' smart constructor.
data CustomizedMetricSpecification =
  CustomizedMetricSpecification'
    { _cmsDimensions :: !(Maybe [MetricDimension])
    , _cmsUnit       :: !(Maybe Text)
    , _cmsMetricName :: !Text
    , _cmsNamespace  :: !Text
    , _cmsStatistic  :: !MetricStatistic
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CustomizedMetricSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmsDimensions' - The dimensions of the metric.  Conditional: If you published your metric with dimensions, you must specify the same dimensions in your scaling policy.
--
-- * 'cmsUnit' - The unit of the metric.
--
-- * 'cmsMetricName' - The name of the metric.
--
-- * 'cmsNamespace' - The namespace of the metric.
--
-- * 'cmsStatistic' - The statistic of the metric.
customizedMetricSpecification
    :: Text -- ^ 'cmsMetricName'
    -> Text -- ^ 'cmsNamespace'
    -> MetricStatistic -- ^ 'cmsStatistic'
    -> CustomizedMetricSpecification
customizedMetricSpecification pMetricName_ pNamespace_ pStatistic_ =
  CustomizedMetricSpecification'
    { _cmsDimensions = Nothing
    , _cmsUnit = Nothing
    , _cmsMetricName = pMetricName_
    , _cmsNamespace = pNamespace_
    , _cmsStatistic = pStatistic_
    }


-- | The dimensions of the metric.  Conditional: If you published your metric with dimensions, you must specify the same dimensions in your scaling policy.
cmsDimensions :: Lens' CustomizedMetricSpecification [MetricDimension]
cmsDimensions = lens _cmsDimensions (\ s a -> s{_cmsDimensions = a}) . _Default . _Coerce

-- | The unit of the metric.
cmsUnit :: Lens' CustomizedMetricSpecification (Maybe Text)
cmsUnit = lens _cmsUnit (\ s a -> s{_cmsUnit = a})

-- | The name of the metric.
cmsMetricName :: Lens' CustomizedMetricSpecification Text
cmsMetricName = lens _cmsMetricName (\ s a -> s{_cmsMetricName = a})

-- | The namespace of the metric.
cmsNamespace :: Lens' CustomizedMetricSpecification Text
cmsNamespace = lens _cmsNamespace (\ s a -> s{_cmsNamespace = a})

-- | The statistic of the metric.
cmsStatistic :: Lens' CustomizedMetricSpecification MetricStatistic
cmsStatistic = lens _cmsStatistic (\ s a -> s{_cmsStatistic = a})

instance FromJSON CustomizedMetricSpecification where
        parseJSON
          = withObject "CustomizedMetricSpecification"
              (\ x ->
                 CustomizedMetricSpecification' <$>
                   (x .:? "Dimensions" .!= mempty) <*> (x .:? "Unit")
                     <*> (x .: "MetricName")
                     <*> (x .: "Namespace")
                     <*> (x .: "Statistic"))

instance Hashable CustomizedMetricSpecification where

instance NFData CustomizedMetricSpecification where

instance ToJSON CustomizedMetricSpecification where
        toJSON CustomizedMetricSpecification'{..}
          = object
              (catMaybes
                 [("Dimensions" .=) <$> _cmsDimensions,
                  ("Unit" .=) <$> _cmsUnit,
                  Just ("MetricName" .= _cmsMetricName),
                  Just ("Namespace" .= _cmsNamespace),
                  Just ("Statistic" .= _cmsStatistic)])

-- | Describes the dimension names and values associated with a metric.
--
--
--
-- /See:/ 'metricDimension' smart constructor.
data MetricDimension =
  MetricDimension'
    { _mdName  :: !Text
    , _mdValue :: !Text
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MetricDimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mdName' - The name of the dimension.
--
-- * 'mdValue' - The value of the dimension.
metricDimension
    :: Text -- ^ 'mdName'
    -> Text -- ^ 'mdValue'
    -> MetricDimension
metricDimension pName_ pValue_ =
  MetricDimension' {_mdName = pName_, _mdValue = pValue_}


-- | The name of the dimension.
mdName :: Lens' MetricDimension Text
mdName = lens _mdName (\ s a -> s{_mdName = a})

-- | The value of the dimension.
mdValue :: Lens' MetricDimension Text
mdValue = lens _mdValue (\ s a -> s{_mdValue = a})

instance FromJSON MetricDimension where
        parseJSON
          = withObject "MetricDimension"
              (\ x ->
                 MetricDimension' <$>
                   (x .: "Name") <*> (x .: "Value"))

instance Hashable MetricDimension where

instance NFData MetricDimension where

instance ToJSON MetricDimension where
        toJSON MetricDimension'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _mdName),
                  Just ("Value" .= _mdValue)])

-- | Represents a predefined metric for a target tracking scaling policy to use with Application Auto Scaling.
--
--
-- Only the AWS services that you're using send metrics to Amazon CloudWatch. To determine whether a desired metric already exists by looking up its namespace and dimension using the CloudWatch metrics dashboard in the console, follow the procedure in <https://docs.aws.amazon.com/autoscaling/application/userguide/monitoring-cloudwatch.html Building Dashboards with CloudWatch> in the /Application Auto Scaling User Guide/ .
--
--
-- /See:/ 'predefinedMetricSpecification' smart constructor.
data PredefinedMetricSpecification =
  PredefinedMetricSpecification'
    { _pmsResourceLabel        :: !(Maybe Text)
    , _pmsPredefinedMetricType :: !MetricType
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PredefinedMetricSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmsResourceLabel' - Identifies the resource associated with the metric type. You can't specify a resource label unless the metric type is @ALBRequestCountPerTarget@ and there is a target group attached to the Spot Fleet request or ECS service. The format is app/<load-balancer-name>/<load-balancer-id>/targetgroup/<target-group-name>/<target-group-id>, where:     * app/<load-balancer-name>/<load-balancer-id> is the final portion of the load balancer ARN     * targetgroup/<target-group-name>/<target-group-id> is the final portion of the target group ARN.
--
-- * 'pmsPredefinedMetricType' - The metric type. The @ALBRequestCountPerTarget@ metric type applies only to Spot Fleet requests and ECS services.
predefinedMetricSpecification
    :: MetricType -- ^ 'pmsPredefinedMetricType'
    -> PredefinedMetricSpecification
predefinedMetricSpecification pPredefinedMetricType_ =
  PredefinedMetricSpecification'
    { _pmsResourceLabel = Nothing
    , _pmsPredefinedMetricType = pPredefinedMetricType_
    }


-- | Identifies the resource associated with the metric type. You can't specify a resource label unless the metric type is @ALBRequestCountPerTarget@ and there is a target group attached to the Spot Fleet request or ECS service. The format is app/<load-balancer-name>/<load-balancer-id>/targetgroup/<target-group-name>/<target-group-id>, where:     * app/<load-balancer-name>/<load-balancer-id> is the final portion of the load balancer ARN     * targetgroup/<target-group-name>/<target-group-id> is the final portion of the target group ARN.
pmsResourceLabel :: Lens' PredefinedMetricSpecification (Maybe Text)
pmsResourceLabel = lens _pmsResourceLabel (\ s a -> s{_pmsResourceLabel = a})

-- | The metric type. The @ALBRequestCountPerTarget@ metric type applies only to Spot Fleet requests and ECS services.
pmsPredefinedMetricType :: Lens' PredefinedMetricSpecification MetricType
pmsPredefinedMetricType = lens _pmsPredefinedMetricType (\ s a -> s{_pmsPredefinedMetricType = a})

instance FromJSON PredefinedMetricSpecification where
        parseJSON
          = withObject "PredefinedMetricSpecification"
              (\ x ->
                 PredefinedMetricSpecification' <$>
                   (x .:? "ResourceLabel") <*>
                     (x .: "PredefinedMetricType"))

instance Hashable PredefinedMetricSpecification where

instance NFData PredefinedMetricSpecification where

instance ToJSON PredefinedMetricSpecification where
        toJSON PredefinedMetricSpecification'{..}
          = object
              (catMaybes
                 [("ResourceLabel" .=) <$> _pmsResourceLabel,
                  Just
                    ("PredefinedMetricType" .=
                       _pmsPredefinedMetricType)])

-- | Represents a scalable target.
--
--
--
-- /See:/ 'scalableTarget' smart constructor.
data ScalableTarget =
  ScalableTarget'
    { _stSuspendedState    :: !(Maybe SuspendedState)
    , _stServiceNamespace  :: !ServiceNamespace
    , _stResourceId        :: !Text
    , _stScalableDimension :: !ScalableDimension
    , _stMinCapacity       :: !Int
    , _stMaxCapacity       :: !Int
    , _stRoleARN           :: !Text
    , _stCreationTime      :: !POSIX
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScalableTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stSuspendedState' - Undocumented member.
--
-- * 'stServiceNamespace' - The namespace of the AWS service that provides the resource, or a @custom-resource@ .
--
-- * 'stResourceId' - The identifier of the resource associated with the scalable target. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the table name. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the index name. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variant - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .     * Custom resources are not supported with a resource type. This parameter must specify the @OutputValue@ from the CloudFormation template stack used to access the resources. The unique identifier is defined by the service provider. More information is available in our <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository> .     * Amazon Comprehend document classification endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:document-classifier-endpoint/EXAMPLE@ .     * Lambda provisioned concurrency - The resource type is @function@ and the unique identifier is the function name with a function version or alias name suffix that is not @> LATEST@ . Example: @function:my-function:prod@ or @function:my-function:1@ .     * Amazon Keyspaces table - The resource type is @table@ and the unique identifier is the table name. Example: @keyspace/mykeyspace/table/mytable@ .
--
-- * 'stScalableDimension' - The scalable dimension associated with the scalable target. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.     * @custom-resource:ResourceType:Property@ - The scalable dimension for a custom resource provided by your own application or service.     * @comprehend:document-classifier-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend document classification endpoint.     * @lambda:function:ProvisionedConcurrency@ - The provisioned concurrency for a Lambda function.     * @cassandra:table:ReadCapacityUnits@ - The provisioned read capacity for an Amazon Keyspaces table.     * @cassandra:table:WriteCapacityUnits@ - The provisioned write capacity for an Amazon Keyspaces table.
--
-- * 'stMinCapacity' - The minimum value to scale to in response to a scale-in event.
--
-- * 'stMaxCapacity' - The maximum value to scale to in response to a scale-out event.
--
-- * 'stRoleARN' - The ARN of an IAM role that allows Application Auto Scaling to modify the scalable target on your behalf.
--
-- * 'stCreationTime' - The Unix timestamp for when the scalable target was created.
scalableTarget
    :: ServiceNamespace -- ^ 'stServiceNamespace'
    -> Text -- ^ 'stResourceId'
    -> ScalableDimension -- ^ 'stScalableDimension'
    -> Int -- ^ 'stMinCapacity'
    -> Int -- ^ 'stMaxCapacity'
    -> Text -- ^ 'stRoleARN'
    -> UTCTime -- ^ 'stCreationTime'
    -> ScalableTarget
scalableTarget pServiceNamespace_ pResourceId_ pScalableDimension_ pMinCapacity_ pMaxCapacity_ pRoleARN_ pCreationTime_ =
  ScalableTarget'
    { _stSuspendedState = Nothing
    , _stServiceNamespace = pServiceNamespace_
    , _stResourceId = pResourceId_
    , _stScalableDimension = pScalableDimension_
    , _stMinCapacity = pMinCapacity_
    , _stMaxCapacity = pMaxCapacity_
    , _stRoleARN = pRoleARN_
    , _stCreationTime = _Time # pCreationTime_
    }


-- | Undocumented member.
stSuspendedState :: Lens' ScalableTarget (Maybe SuspendedState)
stSuspendedState = lens _stSuspendedState (\ s a -> s{_stSuspendedState = a})

-- | The namespace of the AWS service that provides the resource, or a @custom-resource@ .
stServiceNamespace :: Lens' ScalableTarget ServiceNamespace
stServiceNamespace = lens _stServiceNamespace (\ s a -> s{_stServiceNamespace = a})

-- | The identifier of the resource associated with the scalable target. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the table name. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the index name. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variant - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .     * Custom resources are not supported with a resource type. This parameter must specify the @OutputValue@ from the CloudFormation template stack used to access the resources. The unique identifier is defined by the service provider. More information is available in our <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository> .     * Amazon Comprehend document classification endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:document-classifier-endpoint/EXAMPLE@ .     * Lambda provisioned concurrency - The resource type is @function@ and the unique identifier is the function name with a function version or alias name suffix that is not @> LATEST@ . Example: @function:my-function:prod@ or @function:my-function:1@ .     * Amazon Keyspaces table - The resource type is @table@ and the unique identifier is the table name. Example: @keyspace/mykeyspace/table/mytable@ .
stResourceId :: Lens' ScalableTarget Text
stResourceId = lens _stResourceId (\ s a -> s{_stResourceId = a})

-- | The scalable dimension associated with the scalable target. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.     * @custom-resource:ResourceType:Property@ - The scalable dimension for a custom resource provided by your own application or service.     * @comprehend:document-classifier-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend document classification endpoint.     * @lambda:function:ProvisionedConcurrency@ - The provisioned concurrency for a Lambda function.     * @cassandra:table:ReadCapacityUnits@ - The provisioned read capacity for an Amazon Keyspaces table.     * @cassandra:table:WriteCapacityUnits@ - The provisioned write capacity for an Amazon Keyspaces table.
stScalableDimension :: Lens' ScalableTarget ScalableDimension
stScalableDimension = lens _stScalableDimension (\ s a -> s{_stScalableDimension = a})

-- | The minimum value to scale to in response to a scale-in event.
stMinCapacity :: Lens' ScalableTarget Int
stMinCapacity = lens _stMinCapacity (\ s a -> s{_stMinCapacity = a})

-- | The maximum value to scale to in response to a scale-out event.
stMaxCapacity :: Lens' ScalableTarget Int
stMaxCapacity = lens _stMaxCapacity (\ s a -> s{_stMaxCapacity = a})

-- | The ARN of an IAM role that allows Application Auto Scaling to modify the scalable target on your behalf.
stRoleARN :: Lens' ScalableTarget Text
stRoleARN = lens _stRoleARN (\ s a -> s{_stRoleARN = a})

-- | The Unix timestamp for when the scalable target was created.
stCreationTime :: Lens' ScalableTarget UTCTime
stCreationTime = lens _stCreationTime (\ s a -> s{_stCreationTime = a}) . _Time

instance FromJSON ScalableTarget where
        parseJSON
          = withObject "ScalableTarget"
              (\ x ->
                 ScalableTarget' <$>
                   (x .:? "SuspendedState") <*>
                     (x .: "ServiceNamespace")
                     <*> (x .: "ResourceId")
                     <*> (x .: "ScalableDimension")
                     <*> (x .: "MinCapacity")
                     <*> (x .: "MaxCapacity")
                     <*> (x .: "RoleARN")
                     <*> (x .: "CreationTime"))

instance Hashable ScalableTarget where

instance NFData ScalableTarget where

-- | Represents the minimum and maximum capacity for a scheduled action.
--
--
--
-- /See:/ 'scalableTargetAction' smart constructor.
data ScalableTargetAction =
  ScalableTargetAction'
    { _staMaxCapacity :: !(Maybe Int)
    , _staMinCapacity :: !(Maybe Int)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScalableTargetAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'staMaxCapacity' - The maximum capacity.
--
-- * 'staMinCapacity' - The minimum capacity. For Lambda provisioned concurrency, the minimum value allowed is 0. For all other resources, the minimum value allowed is 1.
scalableTargetAction
    :: ScalableTargetAction
scalableTargetAction =
  ScalableTargetAction' {_staMaxCapacity = Nothing, _staMinCapacity = Nothing}


-- | The maximum capacity.
staMaxCapacity :: Lens' ScalableTargetAction (Maybe Int)
staMaxCapacity = lens _staMaxCapacity (\ s a -> s{_staMaxCapacity = a})

-- | The minimum capacity. For Lambda provisioned concurrency, the minimum value allowed is 0. For all other resources, the minimum value allowed is 1.
staMinCapacity :: Lens' ScalableTargetAction (Maybe Int)
staMinCapacity = lens _staMinCapacity (\ s a -> s{_staMinCapacity = a})

instance FromJSON ScalableTargetAction where
        parseJSON
          = withObject "ScalableTargetAction"
              (\ x ->
                 ScalableTargetAction' <$>
                   (x .:? "MaxCapacity") <*> (x .:? "MinCapacity"))

instance Hashable ScalableTargetAction where

instance NFData ScalableTargetAction where

instance ToJSON ScalableTargetAction where
        toJSON ScalableTargetAction'{..}
          = object
              (catMaybes
                 [("MaxCapacity" .=) <$> _staMaxCapacity,
                  ("MinCapacity" .=) <$> _staMinCapacity])

-- | Represents a scaling activity.
--
--
--
-- /See:/ 'scalingActivity' smart constructor.
data ScalingActivity =
  ScalingActivity'
    { _sStatusMessage     :: !(Maybe Text)
    , _sEndTime           :: !(Maybe POSIX)
    , _sDetails           :: !(Maybe Text)
    , _sActivityId        :: !Text
    , _sServiceNamespace  :: !ServiceNamespace
    , _sResourceId        :: !Text
    , _sScalableDimension :: !ScalableDimension
    , _sDescription       :: !Text
    , _sCause             :: !Text
    , _sStartTime         :: !POSIX
    , _sStatusCode        :: !ScalingActivityStatusCode
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScalingActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sStatusMessage' - A simple message about the current status of the scaling activity.
--
-- * 'sEndTime' - The Unix timestamp for when the scaling activity ended.
--
-- * 'sDetails' - The details about the scaling activity.
--
-- * 'sActivityId' - The unique identifier of the scaling activity.
--
-- * 'sServiceNamespace' - The namespace of the AWS service that provides the resource, or a @custom-resource@ .
--
-- * 'sResourceId' - The identifier of the resource associated with the scaling activity. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the table name. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the index name. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variant - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .     * Custom resources are not supported with a resource type. This parameter must specify the @OutputValue@ from the CloudFormation template stack used to access the resources. The unique identifier is defined by the service provider. More information is available in our <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository> .     * Amazon Comprehend document classification endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:document-classifier-endpoint/EXAMPLE@ .     * Lambda provisioned concurrency - The resource type is @function@ and the unique identifier is the function name with a function version or alias name suffix that is not @> LATEST@ . Example: @function:my-function:prod@ or @function:my-function:1@ .     * Amazon Keyspaces table - The resource type is @table@ and the unique identifier is the table name. Example: @keyspace/mykeyspace/table/mytable@ .
--
-- * 'sScalableDimension' - The scalable dimension. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.     * @custom-resource:ResourceType:Property@ - The scalable dimension for a custom resource provided by your own application or service.     * @comprehend:document-classifier-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend document classification endpoint.     * @lambda:function:ProvisionedConcurrency@ - The provisioned concurrency for a Lambda function.     * @cassandra:table:ReadCapacityUnits@ - The provisioned read capacity for an Amazon Keyspaces table.     * @cassandra:table:WriteCapacityUnits@ - The provisioned write capacity for an Amazon Keyspaces table.
--
-- * 'sDescription' - A simple description of what action the scaling activity intends to accomplish.
--
-- * 'sCause' - A simple description of what caused the scaling activity to happen.
--
-- * 'sStartTime' - The Unix timestamp for when the scaling activity began.
--
-- * 'sStatusCode' - Indicates the status of the scaling activity.
scalingActivity
    :: Text -- ^ 'sActivityId'
    -> ServiceNamespace -- ^ 'sServiceNamespace'
    -> Text -- ^ 'sResourceId'
    -> ScalableDimension -- ^ 'sScalableDimension'
    -> Text -- ^ 'sDescription'
    -> Text -- ^ 'sCause'
    -> UTCTime -- ^ 'sStartTime'
    -> ScalingActivityStatusCode -- ^ 'sStatusCode'
    -> ScalingActivity
scalingActivity pActivityId_ pServiceNamespace_ pResourceId_ pScalableDimension_ pDescription_ pCause_ pStartTime_ pStatusCode_ =
  ScalingActivity'
    { _sStatusMessage = Nothing
    , _sEndTime = Nothing
    , _sDetails = Nothing
    , _sActivityId = pActivityId_
    , _sServiceNamespace = pServiceNamespace_
    , _sResourceId = pResourceId_
    , _sScalableDimension = pScalableDimension_
    , _sDescription = pDescription_
    , _sCause = pCause_
    , _sStartTime = _Time # pStartTime_
    , _sStatusCode = pStatusCode_
    }


-- | A simple message about the current status of the scaling activity.
sStatusMessage :: Lens' ScalingActivity (Maybe Text)
sStatusMessage = lens _sStatusMessage (\ s a -> s{_sStatusMessage = a})

-- | The Unix timestamp for when the scaling activity ended.
sEndTime :: Lens' ScalingActivity (Maybe UTCTime)
sEndTime = lens _sEndTime (\ s a -> s{_sEndTime = a}) . mapping _Time

-- | The details about the scaling activity.
sDetails :: Lens' ScalingActivity (Maybe Text)
sDetails = lens _sDetails (\ s a -> s{_sDetails = a})

-- | The unique identifier of the scaling activity.
sActivityId :: Lens' ScalingActivity Text
sActivityId = lens _sActivityId (\ s a -> s{_sActivityId = a})

-- | The namespace of the AWS service that provides the resource, or a @custom-resource@ .
sServiceNamespace :: Lens' ScalingActivity ServiceNamespace
sServiceNamespace = lens _sServiceNamespace (\ s a -> s{_sServiceNamespace = a})

-- | The identifier of the resource associated with the scaling activity. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the table name. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the index name. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variant - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .     * Custom resources are not supported with a resource type. This parameter must specify the @OutputValue@ from the CloudFormation template stack used to access the resources. The unique identifier is defined by the service provider. More information is available in our <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository> .     * Amazon Comprehend document classification endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:document-classifier-endpoint/EXAMPLE@ .     * Lambda provisioned concurrency - The resource type is @function@ and the unique identifier is the function name with a function version or alias name suffix that is not @> LATEST@ . Example: @function:my-function:prod@ or @function:my-function:1@ .     * Amazon Keyspaces table - The resource type is @table@ and the unique identifier is the table name. Example: @keyspace/mykeyspace/table/mytable@ .
sResourceId :: Lens' ScalingActivity Text
sResourceId = lens _sResourceId (\ s a -> s{_sResourceId = a})

-- | The scalable dimension. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.     * @custom-resource:ResourceType:Property@ - The scalable dimension for a custom resource provided by your own application or service.     * @comprehend:document-classifier-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend document classification endpoint.     * @lambda:function:ProvisionedConcurrency@ - The provisioned concurrency for a Lambda function.     * @cassandra:table:ReadCapacityUnits@ - The provisioned read capacity for an Amazon Keyspaces table.     * @cassandra:table:WriteCapacityUnits@ - The provisioned write capacity for an Amazon Keyspaces table.
sScalableDimension :: Lens' ScalingActivity ScalableDimension
sScalableDimension = lens _sScalableDimension (\ s a -> s{_sScalableDimension = a})

-- | A simple description of what action the scaling activity intends to accomplish.
sDescription :: Lens' ScalingActivity Text
sDescription = lens _sDescription (\ s a -> s{_sDescription = a})

-- | A simple description of what caused the scaling activity to happen.
sCause :: Lens' ScalingActivity Text
sCause = lens _sCause (\ s a -> s{_sCause = a})

-- | The Unix timestamp for when the scaling activity began.
sStartTime :: Lens' ScalingActivity UTCTime
sStartTime = lens _sStartTime (\ s a -> s{_sStartTime = a}) . _Time

-- | Indicates the status of the scaling activity.
sStatusCode :: Lens' ScalingActivity ScalingActivityStatusCode
sStatusCode = lens _sStatusCode (\ s a -> s{_sStatusCode = a})

instance FromJSON ScalingActivity where
        parseJSON
          = withObject "ScalingActivity"
              (\ x ->
                 ScalingActivity' <$>
                   (x .:? "StatusMessage") <*> (x .:? "EndTime") <*>
                     (x .:? "Details")
                     <*> (x .: "ActivityId")
                     <*> (x .: "ServiceNamespace")
                     <*> (x .: "ResourceId")
                     <*> (x .: "ScalableDimension")
                     <*> (x .: "Description")
                     <*> (x .: "Cause")
                     <*> (x .: "StartTime")
                     <*> (x .: "StatusCode"))

instance Hashable ScalingActivity where

instance NFData ScalingActivity where

-- | Represents a scaling policy to use with Application Auto Scaling.
--
--
--
-- /See:/ 'scalingPolicy' smart constructor.
data ScalingPolicy =
  ScalingPolicy'
    { _spTargetTrackingScalingPolicyConfiguration :: !(Maybe TargetTrackingScalingPolicyConfiguration)
    , _spStepScalingPolicyConfiguration :: !(Maybe StepScalingPolicyConfiguration)
    , _spAlarms :: !(Maybe [Alarm])
    , _spPolicyARN :: !Text
    , _spPolicyName :: !Text
    , _spServiceNamespace :: !ServiceNamespace
    , _spResourceId :: !Text
    , _spScalableDimension :: !ScalableDimension
    , _spPolicyType :: !PolicyType
    , _spCreationTime :: !POSIX
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spTargetTrackingScalingPolicyConfiguration' - A target tracking scaling policy.
--
-- * 'spStepScalingPolicyConfiguration' - A step scaling policy.
--
-- * 'spAlarms' - The CloudWatch alarms associated with the scaling policy.
--
-- * 'spPolicyARN' - The Amazon Resource Name (ARN) of the scaling policy.
--
-- * 'spPolicyName' - The name of the scaling policy.
--
-- * 'spServiceNamespace' - The namespace of the AWS service that provides the resource, or a @custom-resource@ .
--
-- * 'spResourceId' - The identifier of the resource associated with the scaling policy. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the table name. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the index name. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variant - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .     * Custom resources are not supported with a resource type. This parameter must specify the @OutputValue@ from the CloudFormation template stack used to access the resources. The unique identifier is defined by the service provider. More information is available in our <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository> .     * Amazon Comprehend document classification endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:document-classifier-endpoint/EXAMPLE@ .     * Lambda provisioned concurrency - The resource type is @function@ and the unique identifier is the function name with a function version or alias name suffix that is not @> LATEST@ . Example: @function:my-function:prod@ or @function:my-function:1@ .     * Amazon Keyspaces table - The resource type is @table@ and the unique identifier is the table name. Example: @keyspace/mykeyspace/table/mytable@ .
--
-- * 'spScalableDimension' - The scalable dimension. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.     * @custom-resource:ResourceType:Property@ - The scalable dimension for a custom resource provided by your own application or service.     * @comprehend:document-classifier-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend document classification endpoint.     * @lambda:function:ProvisionedConcurrency@ - The provisioned concurrency for a Lambda function.     * @cassandra:table:ReadCapacityUnits@ - The provisioned read capacity for an Amazon Keyspaces table.     * @cassandra:table:WriteCapacityUnits@ - The provisioned write capacity for an Amazon Keyspaces table.
--
-- * 'spPolicyType' - The scaling policy type.
--
-- * 'spCreationTime' - The Unix timestamp for when the scaling policy was created.
scalingPolicy
    :: Text -- ^ 'spPolicyARN'
    -> Text -- ^ 'spPolicyName'
    -> ServiceNamespace -- ^ 'spServiceNamespace'
    -> Text -- ^ 'spResourceId'
    -> ScalableDimension -- ^ 'spScalableDimension'
    -> PolicyType -- ^ 'spPolicyType'
    -> UTCTime -- ^ 'spCreationTime'
    -> ScalingPolicy
scalingPolicy pPolicyARN_ pPolicyName_ pServiceNamespace_ pResourceId_ pScalableDimension_ pPolicyType_ pCreationTime_ =
  ScalingPolicy'
    { _spTargetTrackingScalingPolicyConfiguration = Nothing
    , _spStepScalingPolicyConfiguration = Nothing
    , _spAlarms = Nothing
    , _spPolicyARN = pPolicyARN_
    , _spPolicyName = pPolicyName_
    , _spServiceNamespace = pServiceNamespace_
    , _spResourceId = pResourceId_
    , _spScalableDimension = pScalableDimension_
    , _spPolicyType = pPolicyType_
    , _spCreationTime = _Time # pCreationTime_
    }


-- | A target tracking scaling policy.
spTargetTrackingScalingPolicyConfiguration :: Lens' ScalingPolicy (Maybe TargetTrackingScalingPolicyConfiguration)
spTargetTrackingScalingPolicyConfiguration = lens _spTargetTrackingScalingPolicyConfiguration (\ s a -> s{_spTargetTrackingScalingPolicyConfiguration = a})

-- | A step scaling policy.
spStepScalingPolicyConfiguration :: Lens' ScalingPolicy (Maybe StepScalingPolicyConfiguration)
spStepScalingPolicyConfiguration = lens _spStepScalingPolicyConfiguration (\ s a -> s{_spStepScalingPolicyConfiguration = a})

-- | The CloudWatch alarms associated with the scaling policy.
spAlarms :: Lens' ScalingPolicy [Alarm]
spAlarms = lens _spAlarms (\ s a -> s{_spAlarms = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the scaling policy.
spPolicyARN :: Lens' ScalingPolicy Text
spPolicyARN = lens _spPolicyARN (\ s a -> s{_spPolicyARN = a})

-- | The name of the scaling policy.
spPolicyName :: Lens' ScalingPolicy Text
spPolicyName = lens _spPolicyName (\ s a -> s{_spPolicyName = a})

-- | The namespace of the AWS service that provides the resource, or a @custom-resource@ .
spServiceNamespace :: Lens' ScalingPolicy ServiceNamespace
spServiceNamespace = lens _spServiceNamespace (\ s a -> s{_spServiceNamespace = a})

-- | The identifier of the resource associated with the scaling policy. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the table name. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the index name. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variant - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .     * Custom resources are not supported with a resource type. This parameter must specify the @OutputValue@ from the CloudFormation template stack used to access the resources. The unique identifier is defined by the service provider. More information is available in our <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository> .     * Amazon Comprehend document classification endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:document-classifier-endpoint/EXAMPLE@ .     * Lambda provisioned concurrency - The resource type is @function@ and the unique identifier is the function name with a function version or alias name suffix that is not @> LATEST@ . Example: @function:my-function:prod@ or @function:my-function:1@ .     * Amazon Keyspaces table - The resource type is @table@ and the unique identifier is the table name. Example: @keyspace/mykeyspace/table/mytable@ .
spResourceId :: Lens' ScalingPolicy Text
spResourceId = lens _spResourceId (\ s a -> s{_spResourceId = a})

-- | The scalable dimension. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.     * @custom-resource:ResourceType:Property@ - The scalable dimension for a custom resource provided by your own application or service.     * @comprehend:document-classifier-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend document classification endpoint.     * @lambda:function:ProvisionedConcurrency@ - The provisioned concurrency for a Lambda function.     * @cassandra:table:ReadCapacityUnits@ - The provisioned read capacity for an Amazon Keyspaces table.     * @cassandra:table:WriteCapacityUnits@ - The provisioned write capacity for an Amazon Keyspaces table.
spScalableDimension :: Lens' ScalingPolicy ScalableDimension
spScalableDimension = lens _spScalableDimension (\ s a -> s{_spScalableDimension = a})

-- | The scaling policy type.
spPolicyType :: Lens' ScalingPolicy PolicyType
spPolicyType = lens _spPolicyType (\ s a -> s{_spPolicyType = a})

-- | The Unix timestamp for when the scaling policy was created.
spCreationTime :: Lens' ScalingPolicy UTCTime
spCreationTime = lens _spCreationTime (\ s a -> s{_spCreationTime = a}) . _Time

instance FromJSON ScalingPolicy where
        parseJSON
          = withObject "ScalingPolicy"
              (\ x ->
                 ScalingPolicy' <$>
                   (x .:? "TargetTrackingScalingPolicyConfiguration")
                     <*> (x .:? "StepScalingPolicyConfiguration")
                     <*> (x .:? "Alarms" .!= mempty)
                     <*> (x .: "PolicyARN")
                     <*> (x .: "PolicyName")
                     <*> (x .: "ServiceNamespace")
                     <*> (x .: "ResourceId")
                     <*> (x .: "ScalableDimension")
                     <*> (x .: "PolicyType")
                     <*> (x .: "CreationTime"))

instance Hashable ScalingPolicy where

instance NFData ScalingPolicy where

-- | Represents a scheduled action.
--
--
--
-- /See:/ 'scheduledAction' smart constructor.
data ScheduledAction =
  ScheduledAction'
    { _saScalableDimension    :: !(Maybe ScalableDimension)
    , _saStartTime            :: !(Maybe POSIX)
    , _saEndTime              :: !(Maybe POSIX)
    , _saScalableTargetAction :: !(Maybe ScalableTargetAction)
    , _saScheduledActionName  :: !Text
    , _saScheduledActionARN   :: !Text
    , _saServiceNamespace     :: !ServiceNamespace
    , _saSchedule             :: !Text
    , _saResourceId           :: !Text
    , _saCreationTime         :: !POSIX
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ScheduledAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saScalableDimension' - The scalable dimension. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.     * @custom-resource:ResourceType:Property@ - The scalable dimension for a custom resource provided by your own application or service.     * @comprehend:document-classifier-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend document classification endpoint.     * @lambda:function:ProvisionedConcurrency@ - The provisioned concurrency for a Lambda function.     * @cassandra:table:ReadCapacityUnits@ - The provisioned read capacity for an Amazon Keyspaces table.     * @cassandra:table:WriteCapacityUnits@ - The provisioned write capacity for an Amazon Keyspaces table.
--
-- * 'saStartTime' - The date and time that the action is scheduled to begin.
--
-- * 'saEndTime' - The date and time that the action is scheduled to end.
--
-- * 'saScalableTargetAction' - The new minimum and maximum capacity. You can set both values or just one. At the scheduled time, if the current capacity is below the minimum capacity, Application Auto Scaling scales out to the minimum capacity. If the current capacity is above the maximum capacity, Application Auto Scaling scales in to the maximum capacity.
--
-- * 'saScheduledActionName' - The name of the scheduled action.
--
-- * 'saScheduledActionARN' - The Amazon Resource Name (ARN) of the scheduled action.
--
-- * 'saServiceNamespace' - The namespace of the AWS service that provides the resource, or a @custom-resource@ .
--
-- * 'saSchedule' - The schedule for this action. The following formats are supported:     * At expressions - "@at(/yyyy/ -/mm/ -/dd/ T/hh/ :/mm/ :/ss/ )@ "     * Rate expressions - "@rate(/value/ /unit/ )@ "     * Cron expressions - "@cron(/fields/ )@ " At expressions are useful for one-time schedules. Specify the time in UTC. For rate expressions, /value/ is a positive integer and /unit/ is @minute@ | @minutes@ | @hour@ | @hours@ | @day@ | @days@ . For more information about cron expressions, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html#CronExpressions Cron Expressions> in the /Amazon CloudWatch Events User Guide/ . For examples of using these expressions, see <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-scheduled-scaling.html Scheduled Scaling> in the /Application Auto Scaling User Guide/ .
--
-- * 'saResourceId' - The identifier of the resource associated with the scaling policy. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the table name. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the index name. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variant - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .     * Custom resources are not supported with a resource type. This parameter must specify the @OutputValue@ from the CloudFormation template stack used to access the resources. The unique identifier is defined by the service provider. More information is available in our <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository> .     * Amazon Comprehend document classification endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:document-classifier-endpoint/EXAMPLE@ .     * Lambda provisioned concurrency - The resource type is @function@ and the unique identifier is the function name with a function version or alias name suffix that is not @> LATEST@ . Example: @function:my-function:prod@ or @function:my-function:1@ .     * Amazon Keyspaces table - The resource type is @table@ and the unique identifier is the table name. Example: @keyspace/mykeyspace/table/mytable@ .
--
-- * 'saCreationTime' - The date and time that the scheduled action was created.
scheduledAction
    :: Text -- ^ 'saScheduledActionName'
    -> Text -- ^ 'saScheduledActionARN'
    -> ServiceNamespace -- ^ 'saServiceNamespace'
    -> Text -- ^ 'saSchedule'
    -> Text -- ^ 'saResourceId'
    -> UTCTime -- ^ 'saCreationTime'
    -> ScheduledAction
scheduledAction pScheduledActionName_ pScheduledActionARN_ pServiceNamespace_ pSchedule_ pResourceId_ pCreationTime_ =
  ScheduledAction'
    { _saScalableDimension = Nothing
    , _saStartTime = Nothing
    , _saEndTime = Nothing
    , _saScalableTargetAction = Nothing
    , _saScheduledActionName = pScheduledActionName_
    , _saScheduledActionARN = pScheduledActionARN_
    , _saServiceNamespace = pServiceNamespace_
    , _saSchedule = pSchedule_
    , _saResourceId = pResourceId_
    , _saCreationTime = _Time # pCreationTime_
    }


-- | The scalable dimension. This string consists of the service namespace, resource type, and scaling property.     * @ecs:service:DesiredCount@ - The desired task count of an ECS service.     * @ec2:spot-fleet-request:TargetCapacity@ - The target capacity of a Spot Fleet request.     * @elasticmapreduce:instancegroup:InstanceCount@ - The instance count of an EMR Instance Group.     * @appstream:fleet:DesiredCapacity@ - The desired capacity of an AppStream 2.0 fleet.     * @dynamodb:table:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB table.     * @dynamodb:table:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB table.     * @dynamodb:index:ReadCapacityUnits@ - The provisioned read capacity for a DynamoDB global secondary index.     * @dynamodb:index:WriteCapacityUnits@ - The provisioned write capacity for a DynamoDB global secondary index.     * @rds:cluster:ReadReplicaCount@ - The count of Aurora Replicas in an Aurora DB cluster. Available for Aurora MySQL-compatible edition and Aurora PostgreSQL-compatible edition.     * @sagemaker:variant:DesiredInstanceCount@ - The number of EC2 instances for an Amazon SageMaker model endpoint variant.     * @custom-resource:ResourceType:Property@ - The scalable dimension for a custom resource provided by your own application or service.     * @comprehend:document-classifier-endpoint:DesiredInferenceUnits@ - The number of inference units for an Amazon Comprehend document classification endpoint.     * @lambda:function:ProvisionedConcurrency@ - The provisioned concurrency for a Lambda function.     * @cassandra:table:ReadCapacityUnits@ - The provisioned read capacity for an Amazon Keyspaces table.     * @cassandra:table:WriteCapacityUnits@ - The provisioned write capacity for an Amazon Keyspaces table.
saScalableDimension :: Lens' ScheduledAction (Maybe ScalableDimension)
saScalableDimension = lens _saScalableDimension (\ s a -> s{_saScalableDimension = a})

-- | The date and time that the action is scheduled to begin.
saStartTime :: Lens' ScheduledAction (Maybe UTCTime)
saStartTime = lens _saStartTime (\ s a -> s{_saStartTime = a}) . mapping _Time

-- | The date and time that the action is scheduled to end.
saEndTime :: Lens' ScheduledAction (Maybe UTCTime)
saEndTime = lens _saEndTime (\ s a -> s{_saEndTime = a}) . mapping _Time

-- | The new minimum and maximum capacity. You can set both values or just one. At the scheduled time, if the current capacity is below the minimum capacity, Application Auto Scaling scales out to the minimum capacity. If the current capacity is above the maximum capacity, Application Auto Scaling scales in to the maximum capacity.
saScalableTargetAction :: Lens' ScheduledAction (Maybe ScalableTargetAction)
saScalableTargetAction = lens _saScalableTargetAction (\ s a -> s{_saScalableTargetAction = a})

-- | The name of the scheduled action.
saScheduledActionName :: Lens' ScheduledAction Text
saScheduledActionName = lens _saScheduledActionName (\ s a -> s{_saScheduledActionName = a})

-- | The Amazon Resource Name (ARN) of the scheduled action.
saScheduledActionARN :: Lens' ScheduledAction Text
saScheduledActionARN = lens _saScheduledActionARN (\ s a -> s{_saScheduledActionARN = a})

-- | The namespace of the AWS service that provides the resource, or a @custom-resource@ .
saServiceNamespace :: Lens' ScheduledAction ServiceNamespace
saServiceNamespace = lens _saServiceNamespace (\ s a -> s{_saServiceNamespace = a})

-- | The schedule for this action. The following formats are supported:     * At expressions - "@at(/yyyy/ -/mm/ -/dd/ T/hh/ :/mm/ :/ss/ )@ "     * Rate expressions - "@rate(/value/ /unit/ )@ "     * Cron expressions - "@cron(/fields/ )@ " At expressions are useful for one-time schedules. Specify the time in UTC. For rate expressions, /value/ is a positive integer and /unit/ is @minute@ | @minutes@ | @hour@ | @hours@ | @day@ | @days@ . For more information about cron expressions, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html#CronExpressions Cron Expressions> in the /Amazon CloudWatch Events User Guide/ . For examples of using these expressions, see <https://docs.aws.amazon.com/autoscaling/application/userguide/application-auto-scaling-scheduled-scaling.html Scheduled Scaling> in the /Application Auto Scaling User Guide/ .
saSchedule :: Lens' ScheduledAction Text
saSchedule = lens _saSchedule (\ s a -> s{_saSchedule = a})

-- | The identifier of the resource associated with the scaling policy. This string consists of the resource type and unique identifier.     * ECS service - The resource type is @service@ and the unique identifier is the cluster name and service name. Example: @service/default/sample-webapp@ .     * Spot Fleet request - The resource type is @spot-fleet-request@ and the unique identifier is the Spot Fleet request ID. Example: @spot-fleet-request/sfr-73fbd2ce-aa30-494c-8788-1cee4EXAMPLE@ .     * EMR cluster - The resource type is @instancegroup@ and the unique identifier is the cluster ID and instance group ID. Example: @instancegroup/j-2EEZNYKUA1NTV/ig-1791Y4E1L8YI0@ .     * AppStream 2.0 fleet - The resource type is @fleet@ and the unique identifier is the fleet name. Example: @fleet/sample-fleet@ .     * DynamoDB table - The resource type is @table@ and the unique identifier is the table name. Example: @table/my-table@ .     * DynamoDB global secondary index - The resource type is @index@ and the unique identifier is the index name. Example: @table/my-table/index/my-table-index@ .     * Aurora DB cluster - The resource type is @cluster@ and the unique identifier is the cluster name. Example: @cluster:my-db-cluster@ .     * Amazon SageMaker endpoint variant - The resource type is @variant@ and the unique identifier is the resource ID. Example: @endpoint/my-end-point/variant/KMeansClustering@ .     * Custom resources are not supported with a resource type. This parameter must specify the @OutputValue@ from the CloudFormation template stack used to access the resources. The unique identifier is defined by the service provider. More information is available in our <https://github.com/aws/aws-auto-scaling-custom-resource GitHub repository> .     * Amazon Comprehend document classification endpoint - The resource type and unique identifier are specified using the endpoint ARN. Example: @arn:aws:comprehend:us-west-2:123456789012:document-classifier-endpoint/EXAMPLE@ .     * Lambda provisioned concurrency - The resource type is @function@ and the unique identifier is the function name with a function version or alias name suffix that is not @> LATEST@ . Example: @function:my-function:prod@ or @function:my-function:1@ .     * Amazon Keyspaces table - The resource type is @table@ and the unique identifier is the table name. Example: @keyspace/mykeyspace/table/mytable@ .
saResourceId :: Lens' ScheduledAction Text
saResourceId = lens _saResourceId (\ s a -> s{_saResourceId = a})

-- | The date and time that the scheduled action was created.
saCreationTime :: Lens' ScheduledAction UTCTime
saCreationTime = lens _saCreationTime (\ s a -> s{_saCreationTime = a}) . _Time

instance FromJSON ScheduledAction where
        parseJSON
          = withObject "ScheduledAction"
              (\ x ->
                 ScheduledAction' <$>
                   (x .:? "ScalableDimension") <*> (x .:? "StartTime")
                     <*> (x .:? "EndTime")
                     <*> (x .:? "ScalableTargetAction")
                     <*> (x .: "ScheduledActionName")
                     <*> (x .: "ScheduledActionARN")
                     <*> (x .: "ServiceNamespace")
                     <*> (x .: "Schedule")
                     <*> (x .: "ResourceId")
                     <*> (x .: "CreationTime"))

instance Hashable ScheduledAction where

instance NFData ScheduledAction where

-- | Represents a step adjustment for a <https://docs.aws.amazon.com/autoscaling/application/APIReference/API_StepScalingPolicyConfiguration.html StepScalingPolicyConfiguration> . Describes an adjustment based on the difference between the value of the aggregated CloudWatch metric and the breach threshold that you've defined for the alarm.
--
--
-- For the following examples, suppose that you have an alarm with a breach threshold of 50:
--
--     * To trigger the adjustment when the metric is greater than or equal to 50 and less than 60, specify a lower bound of 0 and an upper bound of 10.
--
--     * To trigger the adjustment when the metric is greater than 40 and less than or equal to 50, specify a lower bound of -10 and an upper bound of 0.
--
--
--
-- There are a few rules for the step adjustments for your step policy:
--
--     * The ranges of your step adjustments can't overlap or have a gap.
--
--     * At most one step adjustment can have a null lower bound. If one step adjustment has a negative lower bound, then there must be a step adjustment with a null lower bound.
--
--     * At most one step adjustment can have a null upper bound. If one step adjustment has a positive upper bound, then there must be a step adjustment with a null upper bound.
--
--     * The upper and lower bound can't be null in the same step adjustment.
--
--
--
--
-- /See:/ 'stepAdjustment' smart constructor.
data StepAdjustment =
  StepAdjustment'
    { _saMetricIntervalLowerBound :: !(Maybe Double)
    , _saMetricIntervalUpperBound :: !(Maybe Double)
    , _saScalingAdjustment        :: !Int
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StepAdjustment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saMetricIntervalLowerBound' - The lower bound for the difference between the alarm threshold and the CloudWatch metric. If the metric value is above the breach threshold, the lower bound is inclusive (the metric must be greater than or equal to the threshold plus the lower bound). Otherwise, it is exclusive (the metric must be greater than the threshold plus the lower bound). A null value indicates negative infinity.
--
-- * 'saMetricIntervalUpperBound' - The upper bound for the difference between the alarm threshold and the CloudWatch metric. If the metric value is above the breach threshold, the upper bound is exclusive (the metric must be less than the threshold plus the upper bound). Otherwise, it is inclusive (the metric must be less than or equal to the threshold plus the upper bound). A null value indicates positive infinity. The upper bound must be greater than the lower bound.
--
-- * 'saScalingAdjustment' - The amount by which to scale, based on the specified adjustment type. A positive value adds to the current capacity while a negative number removes from the current capacity.
stepAdjustment
    :: Int -- ^ 'saScalingAdjustment'
    -> StepAdjustment
stepAdjustment pScalingAdjustment_ =
  StepAdjustment'
    { _saMetricIntervalLowerBound = Nothing
    , _saMetricIntervalUpperBound = Nothing
    , _saScalingAdjustment = pScalingAdjustment_
    }


-- | The lower bound for the difference between the alarm threshold and the CloudWatch metric. If the metric value is above the breach threshold, the lower bound is inclusive (the metric must be greater than or equal to the threshold plus the lower bound). Otherwise, it is exclusive (the metric must be greater than the threshold plus the lower bound). A null value indicates negative infinity.
saMetricIntervalLowerBound :: Lens' StepAdjustment (Maybe Double)
saMetricIntervalLowerBound = lens _saMetricIntervalLowerBound (\ s a -> s{_saMetricIntervalLowerBound = a})

-- | The upper bound for the difference between the alarm threshold and the CloudWatch metric. If the metric value is above the breach threshold, the upper bound is exclusive (the metric must be less than the threshold plus the upper bound). Otherwise, it is inclusive (the metric must be less than or equal to the threshold plus the upper bound). A null value indicates positive infinity. The upper bound must be greater than the lower bound.
saMetricIntervalUpperBound :: Lens' StepAdjustment (Maybe Double)
saMetricIntervalUpperBound = lens _saMetricIntervalUpperBound (\ s a -> s{_saMetricIntervalUpperBound = a})

-- | The amount by which to scale, based on the specified adjustment type. A positive value adds to the current capacity while a negative number removes from the current capacity.
saScalingAdjustment :: Lens' StepAdjustment Int
saScalingAdjustment = lens _saScalingAdjustment (\ s a -> s{_saScalingAdjustment = a})

instance FromJSON StepAdjustment where
        parseJSON
          = withObject "StepAdjustment"
              (\ x ->
                 StepAdjustment' <$>
                   (x .:? "MetricIntervalLowerBound") <*>
                     (x .:? "MetricIntervalUpperBound")
                     <*> (x .: "ScalingAdjustment"))

instance Hashable StepAdjustment where

instance NFData StepAdjustment where

instance ToJSON StepAdjustment where
        toJSON StepAdjustment'{..}
          = object
              (catMaybes
                 [("MetricIntervalLowerBound" .=) <$>
                    _saMetricIntervalLowerBound,
                  ("MetricIntervalUpperBound" .=) <$>
                    _saMetricIntervalUpperBound,
                  Just ("ScalingAdjustment" .= _saScalingAdjustment)])

-- | Represents a step scaling policy configuration to use with Application Auto Scaling.
--
--
--
-- /See:/ 'stepScalingPolicyConfiguration' smart constructor.
data StepScalingPolicyConfiguration =
  StepScalingPolicyConfiguration'
    { _sspcStepAdjustments        :: !(Maybe [StepAdjustment])
    , _sspcAdjustmentType         :: !(Maybe AdjustmentType)
    , _sspcCooldown               :: !(Maybe Int)
    , _sspcMetricAggregationType  :: !(Maybe MetricAggregationType)
    , _sspcMinAdjustmentMagnitude :: !(Maybe Int)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StepScalingPolicyConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sspcStepAdjustments' - A set of adjustments that enable you to scale based on the size of the alarm breach. At least one step adjustment is required if you are adding a new step scaling policy configuration.
--
-- * 'sspcAdjustmentType' - Specifies whether the @ScalingAdjustment@ value in a <https://docs.aws.amazon.com/autoscaling/application/APIReference/API_StepAdjustment.html StepAdjustment> is an absolute number or a percentage of the current capacity.  @AdjustmentType@ is required if you are adding a new step scaling policy configuration.
--
-- * 'sspcCooldown' - The amount of time, in seconds, after a scaling activity completes where previous trigger-related scaling activities can influence future scaling events. For scale-out policies, while the cooldown period is in effect, the capacity that has been added by the previous scale-out action that initiated the cooldown is calculated as part of the desired capacity for the next scale out. The intention is to continuously (but not excessively) scale out. For example, an alarm triggers a step scaling policy to scale out an Amazon ECS service by 2 tasks, the scaling activity completes successfully, and a cooldown period of 5 minutes starts. During the cooldown period, if the alarm triggers the same policy again but at a more aggressive step adjustment to scale out the service by 3 tasks, the 2 tasks that were added in the previous scale-out action are considered part of that capacity and only 1 additional task is added to the desired count. For scale-in policies, the cooldown period is used to block subsequent scale-in requests until it has expired. The intention is to scale in conservatively to protect your application's availability. However, if another alarm triggers a scale-out policy during the cooldown period after a scale-in, Application Auto Scaling scales out your scalable target immediately. Application Auto Scaling provides a default value of 300 for the following scalable targets:     * ECS services     * Spot Fleet requests     * EMR clusters     * AppStream 2.0 fleets     * Aurora DB clusters     * Amazon SageMaker endpoint variants     * Custom resources For all other scalable targets, the default value is 0:     * DynamoDB tables     * DynamoDB global secondary indexes     * Amazon Comprehend document classification endpoints     * Lambda provisioned concurrency     * Amazon Keyspaces tables
--
-- * 'sspcMetricAggregationType' - The aggregation type for the CloudWatch metrics. Valid values are @Minimum@ , @Maximum@ , and @Average@ . If the aggregation type is null, the value is treated as @Average@ .
--
-- * 'sspcMinAdjustmentMagnitude' - The minimum value to scale by when scaling by percentages. For example, suppose that you create a step scaling policy to scale out an Amazon ECS service by 25 percent and you specify a @MinAdjustmentMagnitude@ of 2. If the service has 4 tasks and the scaling policy is performed, 25 percent of 4 is 1. However, because you specified a @MinAdjustmentMagnitude@ of 2, Application Auto Scaling scales out the service by 2 tasks. Valid only if the adjustment type is @PercentChangeInCapacity@ .
stepScalingPolicyConfiguration
    :: StepScalingPolicyConfiguration
stepScalingPolicyConfiguration =
  StepScalingPolicyConfiguration'
    { _sspcStepAdjustments = Nothing
    , _sspcAdjustmentType = Nothing
    , _sspcCooldown = Nothing
    , _sspcMetricAggregationType = Nothing
    , _sspcMinAdjustmentMagnitude = Nothing
    }


-- | A set of adjustments that enable you to scale based on the size of the alarm breach. At least one step adjustment is required if you are adding a new step scaling policy configuration.
sspcStepAdjustments :: Lens' StepScalingPolicyConfiguration [StepAdjustment]
sspcStepAdjustments = lens _sspcStepAdjustments (\ s a -> s{_sspcStepAdjustments = a}) . _Default . _Coerce

-- | Specifies whether the @ScalingAdjustment@ value in a <https://docs.aws.amazon.com/autoscaling/application/APIReference/API_StepAdjustment.html StepAdjustment> is an absolute number or a percentage of the current capacity.  @AdjustmentType@ is required if you are adding a new step scaling policy configuration.
sspcAdjustmentType :: Lens' StepScalingPolicyConfiguration (Maybe AdjustmentType)
sspcAdjustmentType = lens _sspcAdjustmentType (\ s a -> s{_sspcAdjustmentType = a})

-- | The amount of time, in seconds, after a scaling activity completes where previous trigger-related scaling activities can influence future scaling events. For scale-out policies, while the cooldown period is in effect, the capacity that has been added by the previous scale-out action that initiated the cooldown is calculated as part of the desired capacity for the next scale out. The intention is to continuously (but not excessively) scale out. For example, an alarm triggers a step scaling policy to scale out an Amazon ECS service by 2 tasks, the scaling activity completes successfully, and a cooldown period of 5 minutes starts. During the cooldown period, if the alarm triggers the same policy again but at a more aggressive step adjustment to scale out the service by 3 tasks, the 2 tasks that were added in the previous scale-out action are considered part of that capacity and only 1 additional task is added to the desired count. For scale-in policies, the cooldown period is used to block subsequent scale-in requests until it has expired. The intention is to scale in conservatively to protect your application's availability. However, if another alarm triggers a scale-out policy during the cooldown period after a scale-in, Application Auto Scaling scales out your scalable target immediately. Application Auto Scaling provides a default value of 300 for the following scalable targets:     * ECS services     * Spot Fleet requests     * EMR clusters     * AppStream 2.0 fleets     * Aurora DB clusters     * Amazon SageMaker endpoint variants     * Custom resources For all other scalable targets, the default value is 0:     * DynamoDB tables     * DynamoDB global secondary indexes     * Amazon Comprehend document classification endpoints     * Lambda provisioned concurrency     * Amazon Keyspaces tables
sspcCooldown :: Lens' StepScalingPolicyConfiguration (Maybe Int)
sspcCooldown = lens _sspcCooldown (\ s a -> s{_sspcCooldown = a})

-- | The aggregation type for the CloudWatch metrics. Valid values are @Minimum@ , @Maximum@ , and @Average@ . If the aggregation type is null, the value is treated as @Average@ .
sspcMetricAggregationType :: Lens' StepScalingPolicyConfiguration (Maybe MetricAggregationType)
sspcMetricAggregationType = lens _sspcMetricAggregationType (\ s a -> s{_sspcMetricAggregationType = a})

-- | The minimum value to scale by when scaling by percentages. For example, suppose that you create a step scaling policy to scale out an Amazon ECS service by 25 percent and you specify a @MinAdjustmentMagnitude@ of 2. If the service has 4 tasks and the scaling policy is performed, 25 percent of 4 is 1. However, because you specified a @MinAdjustmentMagnitude@ of 2, Application Auto Scaling scales out the service by 2 tasks. Valid only if the adjustment type is @PercentChangeInCapacity@ .
sspcMinAdjustmentMagnitude :: Lens' StepScalingPolicyConfiguration (Maybe Int)
sspcMinAdjustmentMagnitude = lens _sspcMinAdjustmentMagnitude (\ s a -> s{_sspcMinAdjustmentMagnitude = a})

instance FromJSON StepScalingPolicyConfiguration
         where
        parseJSON
          = withObject "StepScalingPolicyConfiguration"
              (\ x ->
                 StepScalingPolicyConfiguration' <$>
                   (x .:? "StepAdjustments" .!= mempty) <*>
                     (x .:? "AdjustmentType")
                     <*> (x .:? "Cooldown")
                     <*> (x .:? "MetricAggregationType")
                     <*> (x .:? "MinAdjustmentMagnitude"))

instance Hashable StepScalingPolicyConfiguration
         where

instance NFData StepScalingPolicyConfiguration where

instance ToJSON StepScalingPolicyConfiguration where
        toJSON StepScalingPolicyConfiguration'{..}
          = object
              (catMaybes
                 [("StepAdjustments" .=) <$> _sspcStepAdjustments,
                  ("AdjustmentType" .=) <$> _sspcAdjustmentType,
                  ("Cooldown" .=) <$> _sspcCooldown,
                  ("MetricAggregationType" .=) <$>
                    _sspcMetricAggregationType,
                  ("MinAdjustmentMagnitude" .=) <$>
                    _sspcMinAdjustmentMagnitude])

-- | Specifies whether the scaling activities for a scalable target are in a suspended state.
--
--
--
-- /See:/ 'suspendedState' smart constructor.
data SuspendedState =
  SuspendedState'
    { _ssDynamicScalingInSuspended  :: !(Maybe Bool)
    , _ssScheduledScalingSuspended  :: !(Maybe Bool)
    , _ssDynamicScalingOutSuspended :: !(Maybe Bool)
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SuspendedState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssDynamicScalingInSuspended' - Whether scale in by a target tracking scaling policy or a step scaling policy is suspended. Set the value to @true@ if you don't want Application Auto Scaling to remove capacity when a scaling policy is triggered. The default is @false@ .
--
-- * 'ssScheduledScalingSuspended' - Whether scheduled scaling is suspended. Set the value to @true@ if you don't want Application Auto Scaling to add or remove capacity by initiating scheduled actions. The default is @false@ .
--
-- * 'ssDynamicScalingOutSuspended' - Whether scale out by a target tracking scaling policy or a step scaling policy is suspended. Set the value to @true@ if you don't want Application Auto Scaling to add capacity when a scaling policy is triggered. The default is @false@ .
suspendedState
    :: SuspendedState
suspendedState =
  SuspendedState'
    { _ssDynamicScalingInSuspended = Nothing
    , _ssScheduledScalingSuspended = Nothing
    , _ssDynamicScalingOutSuspended = Nothing
    }


-- | Whether scale in by a target tracking scaling policy or a step scaling policy is suspended. Set the value to @true@ if you don't want Application Auto Scaling to remove capacity when a scaling policy is triggered. The default is @false@ .
ssDynamicScalingInSuspended :: Lens' SuspendedState (Maybe Bool)
ssDynamicScalingInSuspended = lens _ssDynamicScalingInSuspended (\ s a -> s{_ssDynamicScalingInSuspended = a})

-- | Whether scheduled scaling is suspended. Set the value to @true@ if you don't want Application Auto Scaling to add or remove capacity by initiating scheduled actions. The default is @false@ .
ssScheduledScalingSuspended :: Lens' SuspendedState (Maybe Bool)
ssScheduledScalingSuspended = lens _ssScheduledScalingSuspended (\ s a -> s{_ssScheduledScalingSuspended = a})

-- | Whether scale out by a target tracking scaling policy or a step scaling policy is suspended. Set the value to @true@ if you don't want Application Auto Scaling to add capacity when a scaling policy is triggered. The default is @false@ .
ssDynamicScalingOutSuspended :: Lens' SuspendedState (Maybe Bool)
ssDynamicScalingOutSuspended = lens _ssDynamicScalingOutSuspended (\ s a -> s{_ssDynamicScalingOutSuspended = a})

instance FromJSON SuspendedState where
        parseJSON
          = withObject "SuspendedState"
              (\ x ->
                 SuspendedState' <$>
                   (x .:? "DynamicScalingInSuspended") <*>
                     (x .:? "ScheduledScalingSuspended")
                     <*> (x .:? "DynamicScalingOutSuspended"))

instance Hashable SuspendedState where

instance NFData SuspendedState where

instance ToJSON SuspendedState where
        toJSON SuspendedState'{..}
          = object
              (catMaybes
                 [("DynamicScalingInSuspended" .=) <$>
                    _ssDynamicScalingInSuspended,
                  ("ScheduledScalingSuspended" .=) <$>
                    _ssScheduledScalingSuspended,
                  ("DynamicScalingOutSuspended" .=) <$>
                    _ssDynamicScalingOutSuspended])

-- | Represents a target tracking scaling policy configuration to use with Application Auto Scaling.
--
--
--
-- /See:/ 'targetTrackingScalingPolicyConfiguration' smart constructor.
data TargetTrackingScalingPolicyConfiguration =
  TargetTrackingScalingPolicyConfiguration'
    { _ttspcPredefinedMetricSpecification :: !(Maybe PredefinedMetricSpecification)
    , _ttspcScaleInCooldown :: !(Maybe Int)
    , _ttspcCustomizedMetricSpecification :: !(Maybe CustomizedMetricSpecification)
    , _ttspcDisableScaleIn :: !(Maybe Bool)
    , _ttspcScaleOutCooldown :: !(Maybe Int)
    , _ttspcTargetValue :: !Double
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TargetTrackingScalingPolicyConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttspcPredefinedMetricSpecification' - A predefined metric. You can specify either a predefined metric or a customized metric.
--
-- * 'ttspcScaleInCooldown' - The amount of time, in seconds, after a scale-in activity completes before another scale in activity can start. The cooldown period is used to block subsequent scale-in requests until it has expired. The intention is to scale in conservatively to protect your application's availability. However, if another alarm triggers a scale-out policy during the cooldown period after a scale-in, Application Auto Scaling scales out your scalable target immediately. Application Auto Scaling provides a default value of 300 for the following scalable targets:     * ECS services     * Spot Fleet requests     * EMR clusters     * AppStream 2.0 fleets     * Aurora DB clusters     * Amazon SageMaker endpoint variants     * Custom resources For all other scalable targets, the default value is 0:     * DynamoDB tables     * DynamoDB global secondary indexes     * Amazon Comprehend document classification endpoints     * Lambda provisioned concurrency     * Amazon Keyspaces tables
--
-- * 'ttspcCustomizedMetricSpecification' - A customized metric. You can specify either a predefined metric or a customized metric.
--
-- * 'ttspcDisableScaleIn' - Indicates whether scale in by the target tracking scaling policy is disabled. If the value is @true@ , scale in is disabled and the target tracking scaling policy won't remove capacity from the scalable target. Otherwise, scale in is enabled and the target tracking scaling policy can remove capacity from the scalable target. The default value is @false@ .
--
-- * 'ttspcScaleOutCooldown' - The amount of time, in seconds, after a scale-out activity completes before another scale-out activity can start. While the cooldown period is in effect, the capacity that has been added by the previous scale-out action that initiated the cooldown is calculated as part of the desired capacity for the next scale out. The intention is to continuously (but not excessively) scale out. Application Auto Scaling provides a default value of 300 for the following scalable targets:     * ECS services     * Spot Fleet requests     * EMR clusters     * AppStream 2.0 fleets     * Aurora DB clusters     * Amazon SageMaker endpoint variants     * Custom resources For all other scalable targets, the default value is 0:     * DynamoDB tables     * DynamoDB global secondary indexes     * Amazon Comprehend document classification endpoints     * Lambda provisioned concurrency     * Amazon Keyspaces tables
--
-- * 'ttspcTargetValue' - The target value for the metric. The range is 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
targetTrackingScalingPolicyConfiguration
    :: Double -- ^ 'ttspcTargetValue'
    -> TargetTrackingScalingPolicyConfiguration
targetTrackingScalingPolicyConfiguration pTargetValue_ =
  TargetTrackingScalingPolicyConfiguration'
    { _ttspcPredefinedMetricSpecification = Nothing
    , _ttspcScaleInCooldown = Nothing
    , _ttspcCustomizedMetricSpecification = Nothing
    , _ttspcDisableScaleIn = Nothing
    , _ttspcScaleOutCooldown = Nothing
    , _ttspcTargetValue = pTargetValue_
    }


-- | A predefined metric. You can specify either a predefined metric or a customized metric.
ttspcPredefinedMetricSpecification :: Lens' TargetTrackingScalingPolicyConfiguration (Maybe PredefinedMetricSpecification)
ttspcPredefinedMetricSpecification = lens _ttspcPredefinedMetricSpecification (\ s a -> s{_ttspcPredefinedMetricSpecification = a})

-- | The amount of time, in seconds, after a scale-in activity completes before another scale in activity can start. The cooldown period is used to block subsequent scale-in requests until it has expired. The intention is to scale in conservatively to protect your application's availability. However, if another alarm triggers a scale-out policy during the cooldown period after a scale-in, Application Auto Scaling scales out your scalable target immediately. Application Auto Scaling provides a default value of 300 for the following scalable targets:     * ECS services     * Spot Fleet requests     * EMR clusters     * AppStream 2.0 fleets     * Aurora DB clusters     * Amazon SageMaker endpoint variants     * Custom resources For all other scalable targets, the default value is 0:     * DynamoDB tables     * DynamoDB global secondary indexes     * Amazon Comprehend document classification endpoints     * Lambda provisioned concurrency     * Amazon Keyspaces tables
ttspcScaleInCooldown :: Lens' TargetTrackingScalingPolicyConfiguration (Maybe Int)
ttspcScaleInCooldown = lens _ttspcScaleInCooldown (\ s a -> s{_ttspcScaleInCooldown = a})

-- | A customized metric. You can specify either a predefined metric or a customized metric.
ttspcCustomizedMetricSpecification :: Lens' TargetTrackingScalingPolicyConfiguration (Maybe CustomizedMetricSpecification)
ttspcCustomizedMetricSpecification = lens _ttspcCustomizedMetricSpecification (\ s a -> s{_ttspcCustomizedMetricSpecification = a})

-- | Indicates whether scale in by the target tracking scaling policy is disabled. If the value is @true@ , scale in is disabled and the target tracking scaling policy won't remove capacity from the scalable target. Otherwise, scale in is enabled and the target tracking scaling policy can remove capacity from the scalable target. The default value is @false@ .
ttspcDisableScaleIn :: Lens' TargetTrackingScalingPolicyConfiguration (Maybe Bool)
ttspcDisableScaleIn = lens _ttspcDisableScaleIn (\ s a -> s{_ttspcDisableScaleIn = a})

-- | The amount of time, in seconds, after a scale-out activity completes before another scale-out activity can start. While the cooldown period is in effect, the capacity that has been added by the previous scale-out action that initiated the cooldown is calculated as part of the desired capacity for the next scale out. The intention is to continuously (but not excessively) scale out. Application Auto Scaling provides a default value of 300 for the following scalable targets:     * ECS services     * Spot Fleet requests     * EMR clusters     * AppStream 2.0 fleets     * Aurora DB clusters     * Amazon SageMaker endpoint variants     * Custom resources For all other scalable targets, the default value is 0:     * DynamoDB tables     * DynamoDB global secondary indexes     * Amazon Comprehend document classification endpoints     * Lambda provisioned concurrency     * Amazon Keyspaces tables
ttspcScaleOutCooldown :: Lens' TargetTrackingScalingPolicyConfiguration (Maybe Int)
ttspcScaleOutCooldown = lens _ttspcScaleOutCooldown (\ s a -> s{_ttspcScaleOutCooldown = a})

-- | The target value for the metric. The range is 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2).
ttspcTargetValue :: Lens' TargetTrackingScalingPolicyConfiguration Double
ttspcTargetValue = lens _ttspcTargetValue (\ s a -> s{_ttspcTargetValue = a})

instance FromJSON
           TargetTrackingScalingPolicyConfiguration
         where
        parseJSON
          = withObject
              "TargetTrackingScalingPolicyConfiguration"
              (\ x ->
                 TargetTrackingScalingPolicyConfiguration' <$>
                   (x .:? "PredefinedMetricSpecification") <*>
                     (x .:? "ScaleInCooldown")
                     <*> (x .:? "CustomizedMetricSpecification")
                     <*> (x .:? "DisableScaleIn")
                     <*> (x .:? "ScaleOutCooldown")
                     <*> (x .: "TargetValue"))

instance Hashable
           TargetTrackingScalingPolicyConfiguration
         where

instance NFData
           TargetTrackingScalingPolicyConfiguration
         where

instance ToJSON
           TargetTrackingScalingPolicyConfiguration
         where
        toJSON TargetTrackingScalingPolicyConfiguration'{..}
          = object
              (catMaybes
                 [("PredefinedMetricSpecification" .=) <$>
                    _ttspcPredefinedMetricSpecification,
                  ("ScaleInCooldown" .=) <$> _ttspcScaleInCooldown,
                  ("CustomizedMetricSpecification" .=) <$>
                    _ttspcCustomizedMetricSpecification,
                  ("DisableScaleIn" .=) <$> _ttspcDisableScaleIn,
                  ("ScaleOutCooldown" .=) <$> _ttspcScaleOutCooldown,
                  Just ("TargetValue" .= _ttspcTargetValue)])

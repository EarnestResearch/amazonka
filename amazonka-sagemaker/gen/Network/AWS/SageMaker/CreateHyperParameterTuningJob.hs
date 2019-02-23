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
-- Module      : Network.AWS.SageMaker.CreateHyperParameterTuningJob
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a hyperparameter tuning job. A hyperparameter tuning job finds the best version of a model by running many training jobs on your dataset using the algorithm you choose and values for hyperparameters within ranges that you specify. It then chooses the hyperparameter values that result in a model that performs the best, as measured by an objective metric that you choose.
--
--
module Network.AWS.SageMaker.CreateHyperParameterTuningJob
    (
    -- * Creating a Request
      createHyperParameterTuningJob
    , CreateHyperParameterTuningJob
    -- * Request Lenses
    , chptjWarmStartConfig
    , chptjTags
    , chptjHyperParameterTuningJobName
    , chptjHyperParameterTuningJobConfig
    , chptjTrainingJobDefinition

    -- * Destructuring the Response
    , createHyperParameterTuningJobResponse
    , CreateHyperParameterTuningJobResponse
    -- * Response Lenses
    , chptjrsResponseStatus
    , chptjrsHyperParameterTuningJobARN
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'createHyperParameterTuningJob' smart constructor.
data CreateHyperParameterTuningJob = CreateHyperParameterTuningJob'
  { _chptjWarmStartConfig :: !(Maybe HyperParameterTuningJobWarmStartConfig)
  , _chptjTags :: !(Maybe [Tag])
  , _chptjHyperParameterTuningJobName :: !Text
  , _chptjHyperParameterTuningJobConfig :: !HyperParameterTuningJobConfig
  , _chptjTrainingJobDefinition :: !HyperParameterTrainingJobDefinition
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateHyperParameterTuningJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chptjWarmStartConfig' - Specifies the configuration for starting the hyperparameter tuning job using one or more previous tuning jobs as a starting point. The results of previous tuning jobs are used to inform which combinations of hyperparameters to search over in the new tuning job. All training jobs launched by the new hyperparameter tuning job are evaluated by using the objective metric. If you specify @IDENTICAL_DATA_AND_ALGORITHM@ as the @WarmStartType@ value for the warm start configuration, the training job that performs the best in the new tuning job is compared to the best training jobs from the parent tuning jobs. From these, the training job that performs the best as measured by the objective metric is returned as the overall best training job.
--
-- * 'chptjTags' - An array of key-value pairs. You can use tags to categorize your AWS resources in different ways, for example, by purpose, owner, or environment. For more information, see <https://aws.amazon.com/answers/account-management/aws-tagging-strategies/ AWS Tagging Strategies> . Tags that you specify for the tuning job are also added to all training jobs that the tuning job launches.
--
-- * 'chptjHyperParameterTuningJobName' - The name of the tuning job. This name is the prefix for the names of all training jobs that this tuning job launches. The name must be unique within the same AWS account and AWS Region. The name must have { } to { } characters. Valid characters are a-z, A-Z, 0-9, and : + = @ _ % - (hyphen). The name is not case sensitive.
--
-- * 'chptjHyperParameterTuningJobConfig' - The 'HyperParameterTuningJobConfig' object that describes the tuning job, including the search strategy, the objective metric used to evaluate training jobs, ranges of parameters to search, and resource limits for the tuning job. For more information, see 'automatic-model-tuning'
--
-- * 'chptjTrainingJobDefinition' - The 'HyperParameterTrainingJobDefinition' object that describes the training jobs that this tuning job launches, including static hyperparameters, input data configuration, output data configuration, resource configuration, and stopping condition.
createHyperParameterTuningJob
    :: Text -- ^ 'chptjHyperParameterTuningJobName'
    -> HyperParameterTuningJobConfig -- ^ 'chptjHyperParameterTuningJobConfig'
    -> HyperParameterTrainingJobDefinition -- ^ 'chptjTrainingJobDefinition'
    -> CreateHyperParameterTuningJob
createHyperParameterTuningJob pHyperParameterTuningJobName_ pHyperParameterTuningJobConfig_ pTrainingJobDefinition_ =
  CreateHyperParameterTuningJob'
    { _chptjWarmStartConfig = Nothing
    , _chptjTags = Nothing
    , _chptjHyperParameterTuningJobName = pHyperParameterTuningJobName_
    , _chptjHyperParameterTuningJobConfig = pHyperParameterTuningJobConfig_
    , _chptjTrainingJobDefinition = pTrainingJobDefinition_
    }


-- | Specifies the configuration for starting the hyperparameter tuning job using one or more previous tuning jobs as a starting point. The results of previous tuning jobs are used to inform which combinations of hyperparameters to search over in the new tuning job. All training jobs launched by the new hyperparameter tuning job are evaluated by using the objective metric. If you specify @IDENTICAL_DATA_AND_ALGORITHM@ as the @WarmStartType@ value for the warm start configuration, the training job that performs the best in the new tuning job is compared to the best training jobs from the parent tuning jobs. From these, the training job that performs the best as measured by the objective metric is returned as the overall best training job.
chptjWarmStartConfig :: Lens' CreateHyperParameterTuningJob (Maybe HyperParameterTuningJobWarmStartConfig)
chptjWarmStartConfig = lens _chptjWarmStartConfig (\ s a -> s{_chptjWarmStartConfig = a})

-- | An array of key-value pairs. You can use tags to categorize your AWS resources in different ways, for example, by purpose, owner, or environment. For more information, see <https://aws.amazon.com/answers/account-management/aws-tagging-strategies/ AWS Tagging Strategies> . Tags that you specify for the tuning job are also added to all training jobs that the tuning job launches.
chptjTags :: Lens' CreateHyperParameterTuningJob [Tag]
chptjTags = lens _chptjTags (\ s a -> s{_chptjTags = a}) . _Default . _Coerce

-- | The name of the tuning job. This name is the prefix for the names of all training jobs that this tuning job launches. The name must be unique within the same AWS account and AWS Region. The name must have { } to { } characters. Valid characters are a-z, A-Z, 0-9, and : + = @ _ % - (hyphen). The name is not case sensitive.
chptjHyperParameterTuningJobName :: Lens' CreateHyperParameterTuningJob Text
chptjHyperParameterTuningJobName = lens _chptjHyperParameterTuningJobName (\ s a -> s{_chptjHyperParameterTuningJobName = a})

-- | The 'HyperParameterTuningJobConfig' object that describes the tuning job, including the search strategy, the objective metric used to evaluate training jobs, ranges of parameters to search, and resource limits for the tuning job. For more information, see 'automatic-model-tuning'
chptjHyperParameterTuningJobConfig :: Lens' CreateHyperParameterTuningJob HyperParameterTuningJobConfig
chptjHyperParameterTuningJobConfig = lens _chptjHyperParameterTuningJobConfig (\ s a -> s{_chptjHyperParameterTuningJobConfig = a})

-- | The 'HyperParameterTrainingJobDefinition' object that describes the training jobs that this tuning job launches, including static hyperparameters, input data configuration, output data configuration, resource configuration, and stopping condition.
chptjTrainingJobDefinition :: Lens' CreateHyperParameterTuningJob HyperParameterTrainingJobDefinition
chptjTrainingJobDefinition = lens _chptjTrainingJobDefinition (\ s a -> s{_chptjTrainingJobDefinition = a})

instance AWSRequest CreateHyperParameterTuningJob
         where
        type Rs CreateHyperParameterTuningJob =
             CreateHyperParameterTuningJobResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 CreateHyperParameterTuningJobResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .:> "HyperParameterTuningJobArn"))

instance Hashable CreateHyperParameterTuningJob where

instance NFData CreateHyperParameterTuningJob where

instance ToHeaders CreateHyperParameterTuningJob
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.CreateHyperParameterTuningJob" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateHyperParameterTuningJob where
        toJSON CreateHyperParameterTuningJob'{..}
          = object
              (catMaybes
                 [("WarmStartConfig" .=) <$> _chptjWarmStartConfig,
                  ("Tags" .=) <$> _chptjTags,
                  Just
                    ("HyperParameterTuningJobName" .=
                       _chptjHyperParameterTuningJobName),
                  Just
                    ("HyperParameterTuningJobConfig" .=
                       _chptjHyperParameterTuningJobConfig),
                  Just
                    ("TrainingJobDefinition" .=
                       _chptjTrainingJobDefinition)])

instance ToPath CreateHyperParameterTuningJob where
        toPath = const "/"

instance ToQuery CreateHyperParameterTuningJob where
        toQuery = const mempty

-- | /See:/ 'createHyperParameterTuningJobResponse' smart constructor.
data CreateHyperParameterTuningJobResponse = CreateHyperParameterTuningJobResponse'
  { _chptjrsResponseStatus             :: !Int
  , _chptjrsHyperParameterTuningJobARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateHyperParameterTuningJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chptjrsResponseStatus' - -- | The response status code.
--
-- * 'chptjrsHyperParameterTuningJobARN' - The Amazon Resource Name (ARN) of the tuning job. Amazon SageMaker assigns an ARN to a hyperparameter tuning job when you create it.
createHyperParameterTuningJobResponse
    :: Int -- ^ 'chptjrsResponseStatus'
    -> Text -- ^ 'chptjrsHyperParameterTuningJobARN'
    -> CreateHyperParameterTuningJobResponse
createHyperParameterTuningJobResponse pResponseStatus_ pHyperParameterTuningJobARN_ =
  CreateHyperParameterTuningJobResponse'
    { _chptjrsResponseStatus = pResponseStatus_
    , _chptjrsHyperParameterTuningJobARN = pHyperParameterTuningJobARN_
    }


-- | -- | The response status code.
chptjrsResponseStatus :: Lens' CreateHyperParameterTuningJobResponse Int
chptjrsResponseStatus = lens _chptjrsResponseStatus (\ s a -> s{_chptjrsResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the tuning job. Amazon SageMaker assigns an ARN to a hyperparameter tuning job when you create it.
chptjrsHyperParameterTuningJobARN :: Lens' CreateHyperParameterTuningJobResponse Text
chptjrsHyperParameterTuningJobARN = lens _chptjrsHyperParameterTuningJobARN (\ s a -> s{_chptjrsHyperParameterTuningJobARN = a})

instance NFData CreateHyperParameterTuningJobResponse
         where

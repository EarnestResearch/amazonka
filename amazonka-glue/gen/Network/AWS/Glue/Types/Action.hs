{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Action
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.Action where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Defines an action to be initiated by a trigger.
--
--
--
-- /See:/ 'action' smart constructor.
data Action = Action'{_aArguments ::
                      !(Maybe (Map Text Text)),
                      _aJobName :: !(Maybe Text),
                      _aTimeout :: !(Maybe Nat)}
                deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Action' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aArguments' - Arguments to be passed to the job. You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes. For information about how to specify and consume your own Job arguments, see the <http://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide. For information about the key-value pairs that AWS Glue consumes to set up your job, see the <http://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
--
-- * 'aJobName' - The name of a job to be executed.
--
-- * 'aTimeout' - The job run timeout in minutes. It overrides the timeout value of the job.
action
    :: Action
action
  = Action'{_aArguments = Nothing, _aJobName = Nothing,
            _aTimeout = Nothing}

-- | Arguments to be passed to the job. You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes. For information about how to specify and consume your own Job arguments, see the <http://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide. For information about the key-value pairs that AWS Glue consumes to set up your job, see the <http://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
aArguments :: Lens' Action (HashMap Text Text)
aArguments = lens _aArguments (\ s a -> s{_aArguments = a}) . _Default . _Map

-- | The name of a job to be executed.
aJobName :: Lens' Action (Maybe Text)
aJobName = lens _aJobName (\ s a -> s{_aJobName = a})

-- | The job run timeout in minutes. It overrides the timeout value of the job.
aTimeout :: Lens' Action (Maybe Natural)
aTimeout = lens _aTimeout (\ s a -> s{_aTimeout = a}) . mapping _Nat

instance FromJSON Action where
        parseJSON
          = withObject "Action"
              (\ x ->
                 Action' <$>
                   (x .:? "Arguments" .!= mempty) <*> (x .:? "JobName")
                     <*> (x .:? "Timeout"))

instance Hashable Action where

instance NFData Action where

instance ToJSON Action where
        toJSON Action'{..}
          = object
              (catMaybes
                 [("Arguments" .=) <$> _aArguments,
                  ("JobName" .=) <$> _aJobName,
                  ("Timeout" .=) <$> _aTimeout])

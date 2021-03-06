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
-- Module      : Network.AWS.StepFunctions.SendTaskFailure
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used by activity workers and task states using the <https://docs.aws.amazon.com/step-functions/latest/dg/connect-to-resource.html#connect-wait-token callback> pattern to report that the task identified by the @taskToken@ failed.
--
--
module Network.AWS.StepFunctions.SendTaskFailure
    (
    -- * Creating a Request
      sendTaskFailure
    , SendTaskFailure
    -- * Request Lenses
    , stfError
    , stfCause
    , stfTaskToken

    -- * Destructuring the Response
    , sendTaskFailureResponse
    , SendTaskFailureResponse
    -- * Response Lenses
    , stfrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StepFunctions.Types
import Network.AWS.StepFunctions.Types.Product

-- | /See:/ 'sendTaskFailure' smart constructor.
data SendTaskFailure = SendTaskFailure'{_stfError ::
                                        !(Maybe (Sensitive Text)),
                                        _stfCause :: !(Maybe (Sensitive Text)),
                                        _stfTaskToken :: !Text}
                         deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'SendTaskFailure' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stfError' - The error code of the failure.
--
-- * 'stfCause' - A more detailed explanation of the cause of the failure.
--
-- * 'stfTaskToken' - The token that represents this task. Task tokens are generated by Step Functions when tasks are assigned to a worker, or in the <https://docs.aws.amazon.com/step-functions/latest/dg/input-output-contextobject.html context object> when a workflow enters a task state. See 'GetActivityTaskOutput$taskToken' .
sendTaskFailure
    :: Text -- ^ 'stfTaskToken'
    -> SendTaskFailure
sendTaskFailure pTaskToken_
  = SendTaskFailure'{_stfError = Nothing,
                     _stfCause = Nothing, _stfTaskToken = pTaskToken_}

-- | The error code of the failure.
stfError :: Lens' SendTaskFailure (Maybe Text)
stfError = lens _stfError (\ s a -> s{_stfError = a}) . mapping _Sensitive

-- | A more detailed explanation of the cause of the failure.
stfCause :: Lens' SendTaskFailure (Maybe Text)
stfCause = lens _stfCause (\ s a -> s{_stfCause = a}) . mapping _Sensitive

-- | The token that represents this task. Task tokens are generated by Step Functions when tasks are assigned to a worker, or in the <https://docs.aws.amazon.com/step-functions/latest/dg/input-output-contextobject.html context object> when a workflow enters a task state. See 'GetActivityTaskOutput$taskToken' .
stfTaskToken :: Lens' SendTaskFailure Text
stfTaskToken = lens _stfTaskToken (\ s a -> s{_stfTaskToken = a})

instance AWSRequest SendTaskFailure where
        type Rs SendTaskFailure = SendTaskFailureResponse
        request = postJSON stepFunctions
        response
          = receiveEmpty
              (\ s h x ->
                 SendTaskFailureResponse' <$> (pure (fromEnum s)))

instance Hashable SendTaskFailure where

instance NFData SendTaskFailure where

instance ToHeaders SendTaskFailure where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSStepFunctions.SendTaskFailure" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON SendTaskFailure where
        toJSON SendTaskFailure'{..}
          = object
              (catMaybes
                 [("error" .=) <$> _stfError,
                  ("cause" .=) <$> _stfCause,
                  Just ("taskToken" .= _stfTaskToken)])

instance ToPath SendTaskFailure where
        toPath = const "/"

instance ToQuery SendTaskFailure where
        toQuery = const mempty

-- | /See:/ 'sendTaskFailureResponse' smart constructor.
newtype SendTaskFailureResponse = SendTaskFailureResponse'{_stfrsResponseStatus
                                                           :: Int}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'SendTaskFailureResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stfrsResponseStatus' - -- | The response status code.
sendTaskFailureResponse
    :: Int -- ^ 'stfrsResponseStatus'
    -> SendTaskFailureResponse
sendTaskFailureResponse pResponseStatus_
  = SendTaskFailureResponse'{_stfrsResponseStatus =
                               pResponseStatus_}

-- | -- | The response status code.
stfrsResponseStatus :: Lens' SendTaskFailureResponse Int
stfrsResponseStatus = lens _stfrsResponseStatus (\ s a -> s{_stfrsResponseStatus = a})

instance NFData SendTaskFailureResponse where

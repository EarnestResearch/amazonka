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
-- Module      : Network.AWS.SWF.RespondActivityTaskCanceled
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used by workers to tell the service that the 'ActivityTask' identified by the @taskToken@ was successfully canceled. Additional @details@ can be provided using the @details@ argument.
--
--
-- These @details@ (if provided) appear in the @ActivityTaskCanceled@ event added to the workflow history.
--
-- /Important:/ Only use this operation if the @canceled@ flag of a 'RecordActivityTaskHeartbeat' request returns @true@ and if the activity can be safely undone or abandoned.
--
-- A task is considered open from the time that it is scheduled until it is closed. Therefore a task is reported as open while a worker is processing it. A task is closed after it has been specified in a call to 'RespondActivityTaskCompleted' , RespondActivityTaskCanceled, 'RespondActivityTaskFailed' , or the task has <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dg-basic.html#swf-dev-timeout-types timed out> .
--
-- __Access Control__ 
--
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--     * You cannot use an IAM policy to constrain this action's parameters.
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
module Network.AWS.SWF.RespondActivityTaskCanceled
    (
    -- * Creating a Request
      respondActivityTaskCanceled
    , RespondActivityTaskCanceled
    -- * Request Lenses
    , rDetails
    , rTaskToken

    -- * Destructuring the Response
    , respondActivityTaskCanceledResponse
    , RespondActivityTaskCanceledResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SWF.Types

-- | /See:/ 'respondActivityTaskCanceled' smart constructor.
data RespondActivityTaskCanceled = RespondActivityTaskCanceled'{_rDetails
                                                                ::
                                                                !(Maybe Text),
                                                                _rTaskToken ::
                                                                !Text}
                                     deriving (Eq, Read, Show, Data, Typeable,
                                               Generic)

-- | Creates a value of 'RespondActivityTaskCanceled' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rDetails' - Information about the cancellation.
--
-- * 'rTaskToken' - The @taskToken@ of the 'ActivityTask' . /Important:/ @taskToken@ is generated by the service and should be treated as an opaque value. If the task is passed to another process, its @taskToken@ must also be passed. This enables it to provide its progress and respond with results.
respondActivityTaskCanceled
    :: Text -- ^ 'rTaskToken'
    -> RespondActivityTaskCanceled
respondActivityTaskCanceled pTaskToken_
  = RespondActivityTaskCanceled'{_rDetails = Nothing,
                                 _rTaskToken = pTaskToken_}

-- | Information about the cancellation.
rDetails :: Lens' RespondActivityTaskCanceled (Maybe Text)
rDetails = lens _rDetails (\ s a -> s{_rDetails = a})

-- | The @taskToken@ of the 'ActivityTask' . /Important:/ @taskToken@ is generated by the service and should be treated as an opaque value. If the task is passed to another process, its @taskToken@ must also be passed. This enables it to provide its progress and respond with results.
rTaskToken :: Lens' RespondActivityTaskCanceled Text
rTaskToken = lens _rTaskToken (\ s a -> s{_rTaskToken = a})

instance AWSRequest RespondActivityTaskCanceled where
        type Rs RespondActivityTaskCanceled =
             RespondActivityTaskCanceledResponse
        request = postJSON swf
        response
          = receiveNull RespondActivityTaskCanceledResponse'

instance Hashable RespondActivityTaskCanceled where

instance NFData RespondActivityTaskCanceled where

instance ToHeaders RespondActivityTaskCanceled where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.RespondActivityTaskCanceled"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON RespondActivityTaskCanceled where
        toJSON RespondActivityTaskCanceled'{..}
          = object
              (catMaybes
                 [("details" .=) <$> _rDetails,
                  Just ("taskToken" .= _rTaskToken)])

instance ToPath RespondActivityTaskCanceled where
        toPath = const "/"

instance ToQuery RespondActivityTaskCanceled where
        toQuery = const mempty

-- | /See:/ 'respondActivityTaskCanceledResponse' smart constructor.
data RespondActivityTaskCanceledResponse = RespondActivityTaskCanceledResponse'
                                             deriving (Eq, Read, Show, Data,
                                                       Typeable, Generic)

-- | Creates a value of 'RespondActivityTaskCanceledResponse' with the minimum fields required to make a request.
--
respondActivityTaskCanceledResponse
    :: RespondActivityTaskCanceledResponse
respondActivityTaskCanceledResponse
  = RespondActivityTaskCanceledResponse'

instance NFData RespondActivityTaskCanceledResponse
         where

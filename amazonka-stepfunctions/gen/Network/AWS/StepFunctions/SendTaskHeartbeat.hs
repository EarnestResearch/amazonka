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
-- Module      : Network.AWS.StepFunctions.SendTaskHeartbeat
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used by workers to report to the service that the task represented by the specified @taskToken@ is still making progress. This action resets the @Heartbeat@ clock. The @Heartbeat@ threshold is specified in the state machine's Amazon States Language definition. This action does not in itself create an event in the execution history. However, if the task times out, the execution history contains an @ActivityTimedOut@ event.
--
--
module Network.AWS.StepFunctions.SendTaskHeartbeat
    -- * Creating a Request
  ( sendTaskHeartbeat
  , SendTaskHeartbeat
    -- * Request Lenses
  , sthTaskToken
    -- * Destructuring the Response
  , sendTaskHeartbeatResponse
  , SendTaskHeartbeatResponse
    -- * Response Lenses
  , sthrsResponseStatus
  ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StepFunctions.Types
import Network.AWS.StepFunctions.Types.Product

-- | /See:/ 'sendTaskHeartbeat' smart constructor.
newtype SendTaskHeartbeat =
  SendTaskHeartbeat'
    { _sthTaskToken :: Text
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SendTaskHeartbeat' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sthTaskToken' - The token that represents this task. Task tokens are generated by the service when the tasks are assigned to a worker (see 'GetActivityTaskOutput$taskToken' ).
sendTaskHeartbeat ::
     Text -- ^ 'sthTaskToken'
  -> SendTaskHeartbeat
sendTaskHeartbeat pTaskToken_ = SendTaskHeartbeat' {_sthTaskToken = pTaskToken_}

-- | The token that represents this task. Task tokens are generated by the service when the tasks are assigned to a worker (see 'GetActivityTaskOutput$taskToken' ).
sthTaskToken :: Lens' SendTaskHeartbeat Text
sthTaskToken = lens _sthTaskToken (\s a -> s {_sthTaskToken = a})

instance AWSRequest SendTaskHeartbeat where
  type Rs SendTaskHeartbeat = SendTaskHeartbeatResponse
  request = postJSON stepFunctions
  response =
    receiveEmpty (\s h x -> SendTaskHeartbeatResponse' <$> (pure (fromEnum s)))

instance Hashable SendTaskHeartbeat

instance NFData SendTaskHeartbeat

instance ToHeaders SendTaskHeartbeat where
  toHeaders =
    const
      (mconcat
         [ "X-Amz-Target" =#
           ("AWSStepFunctions.SendTaskHeartbeat" :: ByteString)
         , "Content-Type" =# ("application/x-amz-json-1.0" :: ByteString)
         ])

instance ToJSON SendTaskHeartbeat where
  toJSON SendTaskHeartbeat' {..} =
    object (catMaybes [Just ("taskToken" .= _sthTaskToken)])

instance ToPath SendTaskHeartbeat where
  toPath = const "/"

instance ToQuery SendTaskHeartbeat where
  toQuery = const mempty

-- | /See:/ 'sendTaskHeartbeatResponse' smart constructor.
newtype SendTaskHeartbeatResponse =
  SendTaskHeartbeatResponse'
    { _sthrsResponseStatus :: Int
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SendTaskHeartbeatResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sthrsResponseStatus' - -- | The response status code.
sendTaskHeartbeatResponse ::
     Int -- ^ 'sthrsResponseStatus'
  -> SendTaskHeartbeatResponse
sendTaskHeartbeatResponse pResponseStatus_ =
  SendTaskHeartbeatResponse' {_sthrsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
sthrsResponseStatus :: Lens' SendTaskHeartbeatResponse Int
sthrsResponseStatus =
  lens _sthrsResponseStatus (\s a -> s {_sthrsResponseStatus = a})

instance NFData SendTaskHeartbeatResponse

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
-- Module      : Network.AWS.StepFunctions.SendTaskSuccess
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used by workers to report that the task identified by the @taskToken@ completed successfully.
--
--
module Network.AWS.StepFunctions.SendTaskSuccess
    (
    -- * Creating a Request
      sendTaskSuccess
    , SendTaskSuccess
    -- * Request Lenses
    , stsTaskToken
    , stsOutput

    -- * Destructuring the Response
    , sendTaskSuccessResponse
    , SendTaskSuccessResponse
    -- * Response Lenses
    , stsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StepFunctions.Types

-- | /See:/ 'sendTaskSuccess' smart constructor.
data SendTaskSuccess = SendTaskSuccess'{_stsTaskToken
                                        :: !Text,
                                        _stsOutput :: !Text}
                         deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SendTaskSuccess' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stsTaskToken' - The token that represents this task. Task tokens are generated by the service when the tasks are assigned to a worker (see 'GetActivityTaskOutput$taskToken' ).
--
-- * 'stsOutput' - The JSON output of the task.
sendTaskSuccess
    :: Text -- ^ 'stsTaskToken'
    -> Text -- ^ 'stsOutput'
    -> SendTaskSuccess
sendTaskSuccess pTaskToken_ pOutput_
  = SendTaskSuccess'{_stsTaskToken = pTaskToken_,
                     _stsOutput = pOutput_}

-- | The token that represents this task. Task tokens are generated by the service when the tasks are assigned to a worker (see 'GetActivityTaskOutput$taskToken' ).
stsTaskToken :: Lens' SendTaskSuccess Text
stsTaskToken = lens _stsTaskToken (\ s a -> s{_stsTaskToken = a})

-- | The JSON output of the task.
stsOutput :: Lens' SendTaskSuccess Text
stsOutput = lens _stsOutput (\ s a -> s{_stsOutput = a})

instance AWSRequest SendTaskSuccess where
        type Rs SendTaskSuccess = SendTaskSuccessResponse
        request = postJSON stepFunctions
        response
          = receiveEmpty
              (\ s h x ->
                 SendTaskSuccessResponse' <$> (pure (fromEnum s)))

instance Hashable SendTaskSuccess where

instance NFData SendTaskSuccess where

instance ToHeaders SendTaskSuccess where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSStepFunctions.SendTaskSuccess" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON SendTaskSuccess where
        toJSON SendTaskSuccess'{..}
          = object
              (catMaybes
                 [Just ("taskToken" .= _stsTaskToken),
                  Just ("output" .= _stsOutput)])

instance ToPath SendTaskSuccess where
        toPath = const "/"

instance ToQuery SendTaskSuccess where
        toQuery = const mempty

-- | /See:/ 'sendTaskSuccessResponse' smart constructor.
newtype SendTaskSuccessResponse = SendTaskSuccessResponse'{_stsrsResponseStatus
                                                           :: Int}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'SendTaskSuccessResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stsrsResponseStatus' - -- | The response status code.
sendTaskSuccessResponse
    :: Int -- ^ 'stsrsResponseStatus'
    -> SendTaskSuccessResponse
sendTaskSuccessResponse pResponseStatus_
  = SendTaskSuccessResponse'{_stsrsResponseStatus =
                               pResponseStatus_}

-- | -- | The response status code.
stsrsResponseStatus :: Lens' SendTaskSuccessResponse Int
stsrsResponseStatus = lens _stsrsResponseStatus (\ s a -> s{_stsrsResponseStatus = a})

instance NFData SendTaskSuccessResponse where

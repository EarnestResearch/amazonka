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
-- Module      : Network.AWS.CodePipeline.PutJobSuccessResult
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents the success of a job as returned to the pipeline by a job worker. Only used for custom actions.
--
--
module Network.AWS.CodePipeline.PutJobSuccessResult
    -- * Creating a Request
  ( putJobSuccessResult
  , PutJobSuccessResult
    -- * Request Lenses
  , pjsrContinuationToken
  , pjsrExecutionDetails
  , pjsrCurrentRevision
  , pjsrJobId
    -- * Destructuring the Response
  , putJobSuccessResultResponse
  , PutJobSuccessResultResponse
  ) where

import Network.AWS.CodePipeline.Types
import Network.AWS.CodePipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a PutJobSuccessResult action.
--
--
--
-- /See:/ 'putJobSuccessResult' smart constructor.
data PutJobSuccessResult =
  PutJobSuccessResult'
    { _pjsrContinuationToken :: !(Maybe Text)
    , _pjsrExecutionDetails  :: !(Maybe ExecutionDetails)
    , _pjsrCurrentRevision   :: !(Maybe CurrentRevision)
    , _pjsrJobId             :: !Text
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutJobSuccessResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pjsrContinuationToken' - A token generated by a job worker, such as an AWS CodeDeploy deployment ID, that a successful job provides to identify a custom action in progress. Future jobs will use this token in order to identify the running instance of the action. It can be reused to return additional information about the progress of the custom action. When the action is complete, no continuation token should be supplied.
--
-- * 'pjsrExecutionDetails' - The execution details of the successful job, such as the actions taken by the job worker.
--
-- * 'pjsrCurrentRevision' - The ID of the current revision of the artifact successfully worked upon by the job.
--
-- * 'pjsrJobId' - The unique system-generated ID of the job that succeeded. This is the same ID returned from PollForJobs.
putJobSuccessResult ::
     Text -- ^ 'pjsrJobId'
  -> PutJobSuccessResult
putJobSuccessResult pJobId_ =
  PutJobSuccessResult'
    { _pjsrContinuationToken = Nothing
    , _pjsrExecutionDetails = Nothing
    , _pjsrCurrentRevision = Nothing
    , _pjsrJobId = pJobId_
    }

-- | A token generated by a job worker, such as an AWS CodeDeploy deployment ID, that a successful job provides to identify a custom action in progress. Future jobs will use this token in order to identify the running instance of the action. It can be reused to return additional information about the progress of the custom action. When the action is complete, no continuation token should be supplied.
pjsrContinuationToken :: Lens' PutJobSuccessResult (Maybe Text)
pjsrContinuationToken =
  lens _pjsrContinuationToken (\s a -> s {_pjsrContinuationToken = a})

-- | The execution details of the successful job, such as the actions taken by the job worker.
pjsrExecutionDetails :: Lens' PutJobSuccessResult (Maybe ExecutionDetails)
pjsrExecutionDetails =
  lens _pjsrExecutionDetails (\s a -> s {_pjsrExecutionDetails = a})

-- | The ID of the current revision of the artifact successfully worked upon by the job.
pjsrCurrentRevision :: Lens' PutJobSuccessResult (Maybe CurrentRevision)
pjsrCurrentRevision =
  lens _pjsrCurrentRevision (\s a -> s {_pjsrCurrentRevision = a})

-- | The unique system-generated ID of the job that succeeded. This is the same ID returned from PollForJobs.
pjsrJobId :: Lens' PutJobSuccessResult Text
pjsrJobId = lens _pjsrJobId (\s a -> s {_pjsrJobId = a})

instance AWSRequest PutJobSuccessResult where
  type Rs PutJobSuccessResult = PutJobSuccessResultResponse
  request = postJSON codePipeline
  response = receiveNull PutJobSuccessResultResponse'

instance Hashable PutJobSuccessResult

instance NFData PutJobSuccessResult

instance ToHeaders PutJobSuccessResult where
  toHeaders =
    const
      (mconcat
         [ "X-Amz-Target" =#
           ("CodePipeline_20150709.PutJobSuccessResult" :: ByteString)
         , "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
         ])

instance ToJSON PutJobSuccessResult where
  toJSON PutJobSuccessResult' {..} =
    object
      (catMaybes
         [ ("continuationToken" .=) <$> _pjsrContinuationToken
         , ("executionDetails" .=) <$> _pjsrExecutionDetails
         , ("currentRevision" .=) <$> _pjsrCurrentRevision
         , Just ("jobId" .= _pjsrJobId)
         ])

instance ToPath PutJobSuccessResult where
  toPath = const "/"

instance ToQuery PutJobSuccessResult where
  toQuery = const mempty

-- | /See:/ 'putJobSuccessResultResponse' smart constructor.
data PutJobSuccessResultResponse =
  PutJobSuccessResultResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutJobSuccessResultResponse' with the minimum fields required to make a request.
--
putJobSuccessResultResponse :: PutJobSuccessResultResponse
putJobSuccessResultResponse = PutJobSuccessResultResponse'

instance NFData PutJobSuccessResultResponse

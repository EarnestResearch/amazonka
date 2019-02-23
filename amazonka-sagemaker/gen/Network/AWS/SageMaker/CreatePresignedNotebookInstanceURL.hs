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
-- Module      : Network.AWS.SageMaker.CreatePresignedNotebookInstanceURL
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a URL that you can use to connect to the Jupyter server from a notebook instance. In the Amazon SageMaker console, when you choose @Open@ next to a notebook instance, Amazon SageMaker opens a new tab showing the Jupyter server home page from the notebook instance. The console uses this API to get the URL and show the page.
--
--
-- You can restrict access to this API and to the URL that it returns to a list of IP addresses that you specify. To restrict access, attach an IAM policy that denies access to this API unless the call comes from an IP address in the specified list to every AWS Identity and Access Management user, group, or role used to access the notebook instance. Use the @NotIpAddress@ condition operator and the @aws:SourceIP@ condition context key to specify the list of IP addresses that you want to have access to the notebook instance. For more information, see <http://docs.aws.amazon.com/sagemaker/latest/dg/howitworks-access-ws.html#nbi-ip-filter Limit Access to a Notebook Instance by IP Address> .
--
module Network.AWS.SageMaker.CreatePresignedNotebookInstanceURL
    (
    -- * Creating a Request
      createPresignedNotebookInstanceURL
    , CreatePresignedNotebookInstanceURL
    -- * Request Lenses
    , cpniuSessionExpirationDurationInSeconds
    , cpniuNotebookInstanceName

    -- * Destructuring the Response
    , createPresignedNotebookInstanceURLResponse
    , CreatePresignedNotebookInstanceURLResponse
    -- * Response Lenses
    , cpniursAuthorizedURL
    , cpniursResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'createPresignedNotebookInstanceURL' smart constructor.
data CreatePresignedNotebookInstanceURL = CreatePresignedNotebookInstanceURL'
  { _cpniuSessionExpirationDurationInSeconds :: !(Maybe Nat)
  , _cpniuNotebookInstanceName               :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePresignedNotebookInstanceURL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpniuSessionExpirationDurationInSeconds' - The duration of the session, in seconds. The default is 12 hours.
--
-- * 'cpniuNotebookInstanceName' - The name of the notebook instance.
createPresignedNotebookInstanceURL
    :: Text -- ^ 'cpniuNotebookInstanceName'
    -> CreatePresignedNotebookInstanceURL
createPresignedNotebookInstanceURL pNotebookInstanceName_ =
  CreatePresignedNotebookInstanceURL'
    { _cpniuSessionExpirationDurationInSeconds = Nothing
    , _cpniuNotebookInstanceName = pNotebookInstanceName_
    }


-- | The duration of the session, in seconds. The default is 12 hours.
cpniuSessionExpirationDurationInSeconds :: Lens' CreatePresignedNotebookInstanceURL (Maybe Natural)
cpniuSessionExpirationDurationInSeconds = lens _cpniuSessionExpirationDurationInSeconds (\ s a -> s{_cpniuSessionExpirationDurationInSeconds = a}) . mapping _Nat

-- | The name of the notebook instance.
cpniuNotebookInstanceName :: Lens' CreatePresignedNotebookInstanceURL Text
cpniuNotebookInstanceName = lens _cpniuNotebookInstanceName (\ s a -> s{_cpniuNotebookInstanceName = a})

instance AWSRequest
           CreatePresignedNotebookInstanceURL
         where
        type Rs CreatePresignedNotebookInstanceURL =
             CreatePresignedNotebookInstanceURLResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 CreatePresignedNotebookInstanceURLResponse' <$>
                   (x .?> "AuthorizedUrl") <*> (pure (fromEnum s)))

instance Hashable CreatePresignedNotebookInstanceURL
         where

instance NFData CreatePresignedNotebookInstanceURL
         where

instance ToHeaders CreatePresignedNotebookInstanceURL
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.CreatePresignedNotebookInstanceUrl" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreatePresignedNotebookInstanceURL
         where
        toJSON CreatePresignedNotebookInstanceURL'{..}
          = object
              (catMaybes
                 [("SessionExpirationDurationInSeconds" .=) <$>
                    _cpniuSessionExpirationDurationInSeconds,
                  Just
                    ("NotebookInstanceName" .=
                       _cpniuNotebookInstanceName)])

instance ToPath CreatePresignedNotebookInstanceURL
         where
        toPath = const "/"

instance ToQuery CreatePresignedNotebookInstanceURL
         where
        toQuery = const mempty

-- | /See:/ 'createPresignedNotebookInstanceURLResponse' smart constructor.
data CreatePresignedNotebookInstanceURLResponse = CreatePresignedNotebookInstanceURLResponse'
  { _cpniursAuthorizedURL  :: !(Maybe Text)
  , _cpniursResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePresignedNotebookInstanceURLResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpniursAuthorizedURL' - A JSON object that contains the URL string.
--
-- * 'cpniursResponseStatus' - -- | The response status code.
createPresignedNotebookInstanceURLResponse
    :: Int -- ^ 'cpniursResponseStatus'
    -> CreatePresignedNotebookInstanceURLResponse
createPresignedNotebookInstanceURLResponse pResponseStatus_ =
  CreatePresignedNotebookInstanceURLResponse'
    {_cpniursAuthorizedURL = Nothing, _cpniursResponseStatus = pResponseStatus_}


-- | A JSON object that contains the URL string.
cpniursAuthorizedURL :: Lens' CreatePresignedNotebookInstanceURLResponse (Maybe Text)
cpniursAuthorizedURL = lens _cpniursAuthorizedURL (\ s a -> s{_cpniursAuthorizedURL = a})

-- | -- | The response status code.
cpniursResponseStatus :: Lens' CreatePresignedNotebookInstanceURLResponse Int
cpniursResponseStatus = lens _cpniursResponseStatus (\ s a -> s{_cpniursResponseStatus = a})

instance NFData
           CreatePresignedNotebookInstanceURLResponse
         where

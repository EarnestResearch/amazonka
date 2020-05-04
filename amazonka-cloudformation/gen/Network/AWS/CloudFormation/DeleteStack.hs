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
-- Module      : Network.AWS.CloudFormation.DeleteStack
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified stack. Once the call completes successfully, stack deletion starts. Deleted stacks do not show up in the 'DescribeStacks' API if the deletion has been completed successfully.
--
--
module Network.AWS.CloudFormation.DeleteStack
    -- * Creating a Request
  ( deleteStack
  , DeleteStack
    -- * Request Lenses
  , dsRetainResources
  , dsClientRequestToken
  , dsRoleARN
  , dsStackName
    -- * Destructuring the Response
  , deleteStackResponse
  , DeleteStackResponse
  ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for 'DeleteStack' action.
--
--
--
-- /See:/ 'deleteStack' smart constructor.
data DeleteStack =
  DeleteStack'
    { _dsRetainResources    :: !(Maybe [Text])
    , _dsClientRequestToken :: !(Maybe Text)
    , _dsRoleARN            :: !(Maybe Text)
    , _dsStackName          :: !Text
    }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteStack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsRetainResources' - For stacks in the @DELETE_FAILED@ state, a list of resource logical IDs that are associated with the resources you want to retain. During deletion, AWS CloudFormation deletes the stack but does not delete the retained resources. Retaining resources is useful when you cannot delete a resource, such as a non-empty S3 bucket, but you want to delete the stack.
--
-- * 'dsClientRequestToken' - A unique identifier for this @DeleteStack@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to delete a stack with the same name. You might retry @DeleteStack@ requests to ensure that AWS CloudFormation successfully received them. All events triggered by a given stack operation are assigned the same client request token, which you can use to track operations. For example, if you execute a @CreateStack@ operation with the token @token1@ , then all the @StackEvents@ generated by that operation will have @ClientRequestToken@ set as @token1@ . In the console, stack operations display the client request token on the Events tab. Stack operations that are initiated from the console use the token format /Console-StackOperation-ID/ , which helps you easily identify the stack operation . For example, if you create a stack using the console, each stack event would be assigned the same token in the following format: @Console-CreateStack-7f59c3cf-00d2-40c7-b2ff-e75db0987002@ .
--
-- * 'dsRoleARN' - The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that AWS CloudFormation assumes to delete the stack. AWS CloudFormation uses the role's credentials to make calls on your behalf. If you don't specify a value, AWS CloudFormation uses the role that was previously associated with the stack. If no role is available, AWS CloudFormation uses a temporary session that is generated from your user credentials.
--
-- * 'dsStackName' - The name or the unique stack ID that is associated with the stack.
deleteStack ::
     Text -- ^ 'dsStackName'
  -> DeleteStack
deleteStack pStackName_ =
  DeleteStack'
    { _dsRetainResources = Nothing
    , _dsClientRequestToken = Nothing
    , _dsRoleARN = Nothing
    , _dsStackName = pStackName_
    }

-- | For stacks in the @DELETE_FAILED@ state, a list of resource logical IDs that are associated with the resources you want to retain. During deletion, AWS CloudFormation deletes the stack but does not delete the retained resources. Retaining resources is useful when you cannot delete a resource, such as a non-empty S3 bucket, but you want to delete the stack.
dsRetainResources :: Lens' DeleteStack [Text]
dsRetainResources =
  lens _dsRetainResources (\s a -> s {_dsRetainResources = a}) .
  _Default . _Coerce

-- | A unique identifier for this @DeleteStack@ request. Specify this token if you plan to retry requests so that AWS CloudFormation knows that you're not attempting to delete a stack with the same name. You might retry @DeleteStack@ requests to ensure that AWS CloudFormation successfully received them. All events triggered by a given stack operation are assigned the same client request token, which you can use to track operations. For example, if you execute a @CreateStack@ operation with the token @token1@ , then all the @StackEvents@ generated by that operation will have @ClientRequestToken@ set as @token1@ . In the console, stack operations display the client request token on the Events tab. Stack operations that are initiated from the console use the token format /Console-StackOperation-ID/ , which helps you easily identify the stack operation . For example, if you create a stack using the console, each stack event would be assigned the same token in the following format: @Console-CreateStack-7f59c3cf-00d2-40c7-b2ff-e75db0987002@ .
dsClientRequestToken :: Lens' DeleteStack (Maybe Text)
dsClientRequestToken =
  lens _dsClientRequestToken (\s a -> s {_dsClientRequestToken = a})

-- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that AWS CloudFormation assumes to delete the stack. AWS CloudFormation uses the role's credentials to make calls on your behalf. If you don't specify a value, AWS CloudFormation uses the role that was previously associated with the stack. If no role is available, AWS CloudFormation uses a temporary session that is generated from your user credentials.
dsRoleARN :: Lens' DeleteStack (Maybe Text)
dsRoleARN = lens _dsRoleARN (\s a -> s {_dsRoleARN = a})

-- | The name or the unique stack ID that is associated with the stack.
dsStackName :: Lens' DeleteStack Text
dsStackName = lens _dsStackName (\s a -> s {_dsStackName = a})

instance AWSRequest DeleteStack where
  type Rs DeleteStack = DeleteStackResponse
  request = postQuery cloudFormation
  response = receiveNull DeleteStackResponse'

instance Hashable DeleteStack

instance NFData DeleteStack

instance ToHeaders DeleteStack where
  toHeaders = const mempty

instance ToPath DeleteStack where
  toPath = const "/"

instance ToQuery DeleteStack where
  toQuery DeleteStack' {..} =
    mconcat
      [ "Action" =: ("DeleteStack" :: ByteString)
      , "Version" =: ("2010-05-15" :: ByteString)
      , "RetainResources" =:
        toQuery (toQueryList "member" <$> _dsRetainResources)
      , "ClientRequestToken" =: _dsClientRequestToken
      , "RoleARN" =: _dsRoleARN
      , "StackName" =: _dsStackName
      ]

-- | /See:/ 'deleteStackResponse' smart constructor.
data DeleteStackResponse =
  DeleteStackResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteStackResponse' with the minimum fields required to make a request.
--
deleteStackResponse :: DeleteStackResponse
deleteStackResponse = DeleteStackResponse'

instance NFData DeleteStackResponse

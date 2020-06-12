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
-- Module      : Network.AWS.MechanicalTurk.CreateHITWithHITType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @CreateHITWithHITType@ operation creates a new Human Intelligence Task (HIT) using an existing HITTypeID generated by the @CreateHITType@ operation. 
--
--
-- This is an alternative way to create HITs from the @CreateHIT@ operation. This is the recommended best practice for Requesters who are creating large numbers of HITs. 
--
-- CreateHITWithHITType also supports several ways to provide question data: by providing a value for the @Question@ parameter that fully specifies the contents of the HIT, or by providing a @HitLayoutId@ and associated @HitLayoutParameters@ . 
--
module Network.AWS.MechanicalTurk.CreateHITWithHITType
    (
    -- * Creating a Request
      createHITWithHITType
    , CreateHITWithHITType
    -- * Request Lenses
    , chitwhittHITReviewPolicy
    , chitwhittUniqueRequestToken
    , chitwhittRequesterAnnotation
    , chitwhittMaxAssignments
    , chitwhittHITLayoutId
    , chitwhittHITLayoutParameters
    , chitwhittQuestion
    , chitwhittAssignmentReviewPolicy
    , chitwhittHITTypeId
    , chitwhittLifetimeInSeconds

    -- * Destructuring the Response
    , createHITWithHITTypeResponse
    , CreateHITWithHITTypeResponse
    -- * Response Lenses
    , chitwhittrsHIT
    , chitwhittrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createHITWithHITType' smart constructor.
data CreateHITWithHITType = CreateHITWithHITType'{_chitwhittHITReviewPolicy
                                                  :: !(Maybe ReviewPolicy),
                                                  _chitwhittUniqueRequestToken
                                                  :: !(Maybe Text),
                                                  _chitwhittRequesterAnnotation
                                                  :: !(Maybe Text),
                                                  _chitwhittMaxAssignments ::
                                                  !(Maybe Int),
                                                  _chitwhittHITLayoutId ::
                                                  !(Maybe Text),
                                                  _chitwhittHITLayoutParameters
                                                  ::
                                                  !(Maybe [HITLayoutParameter]),
                                                  _chitwhittQuestion ::
                                                  !(Maybe Text),
                                                  _chitwhittAssignmentReviewPolicy
                                                  :: !(Maybe ReviewPolicy),
                                                  _chitwhittHITTypeId :: !Text,
                                                  _chitwhittLifetimeInSeconds ::
                                                  !Integer}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateHITWithHITType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chitwhittHITReviewPolicy' - The HIT-level Review Policy applies to the HIT. You can specify for Mechanical Turk to take various actions based on the policy. 
--
-- * 'chitwhittUniqueRequestToken' - A unique identifier for this request which allows you to retry the call on error without creating duplicate HITs. This is useful in cases such as network timeouts where it is unclear whether or not the call succeeded on the server. If the HIT already exists in the system from a previous call using the same UniqueRequestToken, subsequent calls will return a AWS.MechanicalTurk.HitAlreadyExists error with a message containing the HITId. 
--
-- * 'chitwhittRequesterAnnotation' - An arbitrary data field. The RequesterAnnotation parameter lets your application attach arbitrary data to the HIT for tracking purposes. For example, this parameter could be an identifier internal to the Requester's application that corresponds with the HIT.  The RequesterAnnotation parameter for a HIT is only visible to the Requester who created the HIT. It is not shown to the Worker, or any other Requester.  The RequesterAnnotation parameter may be different for each HIT you submit. It does not affect how your HITs are grouped. 
--
-- * 'chitwhittMaxAssignments' - The number of times the HIT can be accepted and completed before the HIT becomes unavailable. 
--
-- * 'chitwhittHITLayoutId' - The HITLayoutId allows you to use a pre-existing HIT design with placeholder values and create an additional HIT by providing those values as HITLayoutParameters.  Constraints: Either a Question parameter or a HITLayoutId parameter must be provided. 
--
-- * 'chitwhittHITLayoutParameters' - If the HITLayoutId is provided, any placeholder values must be filled in with values using the HITLayoutParameter structure. For more information, see HITLayout. 
--
-- * 'chitwhittQuestion' - The data the person completing the HIT uses to produce the results.  Constraints: Must be a QuestionForm data structure, an ExternalQuestion data structure, or an HTMLQuestion data structure. The XML question data must not be larger than 64 kilobytes (65,535 bytes) in size, including whitespace.  Either a Question parameter or a HITLayoutId parameter must be provided.
--
-- * 'chitwhittAssignmentReviewPolicy' - The Assignment-level Review Policy applies to the assignments under the HIT. You can specify for Mechanical Turk to take various actions based on the policy. 
--
-- * 'chitwhittHITTypeId' - The HIT type ID you want to create this HIT with.
--
-- * 'chitwhittLifetimeInSeconds' - An amount of time, in seconds, after which the HIT is no longer available for users to accept. After the lifetime of the HIT elapses, the HIT no longer appears in HIT searches, even if not all of the assignments for the HIT have been accepted. 
createHITWithHITType
    :: Text -- ^ 'chitwhittHITTypeId'
    -> Integer -- ^ 'chitwhittLifetimeInSeconds'
    -> CreateHITWithHITType
createHITWithHITType pHITTypeId_ pLifetimeInSeconds_
  = CreateHITWithHITType'{_chitwhittHITReviewPolicy =
                            Nothing,
                          _chitwhittUniqueRequestToken = Nothing,
                          _chitwhittRequesterAnnotation = Nothing,
                          _chitwhittMaxAssignments = Nothing,
                          _chitwhittHITLayoutId = Nothing,
                          _chitwhittHITLayoutParameters = Nothing,
                          _chitwhittQuestion = Nothing,
                          _chitwhittAssignmentReviewPolicy = Nothing,
                          _chitwhittHITTypeId = pHITTypeId_,
                          _chitwhittLifetimeInSeconds = pLifetimeInSeconds_}

-- | The HIT-level Review Policy applies to the HIT. You can specify for Mechanical Turk to take various actions based on the policy. 
chitwhittHITReviewPolicy :: Lens' CreateHITWithHITType (Maybe ReviewPolicy)
chitwhittHITReviewPolicy = lens _chitwhittHITReviewPolicy (\ s a -> s{_chitwhittHITReviewPolicy = a})

-- | A unique identifier for this request which allows you to retry the call on error without creating duplicate HITs. This is useful in cases such as network timeouts where it is unclear whether or not the call succeeded on the server. If the HIT already exists in the system from a previous call using the same UniqueRequestToken, subsequent calls will return a AWS.MechanicalTurk.HitAlreadyExists error with a message containing the HITId. 
chitwhittUniqueRequestToken :: Lens' CreateHITWithHITType (Maybe Text)
chitwhittUniqueRequestToken = lens _chitwhittUniqueRequestToken (\ s a -> s{_chitwhittUniqueRequestToken = a})

-- | An arbitrary data field. The RequesterAnnotation parameter lets your application attach arbitrary data to the HIT for tracking purposes. For example, this parameter could be an identifier internal to the Requester's application that corresponds with the HIT.  The RequesterAnnotation parameter for a HIT is only visible to the Requester who created the HIT. It is not shown to the Worker, or any other Requester.  The RequesterAnnotation parameter may be different for each HIT you submit. It does not affect how your HITs are grouped. 
chitwhittRequesterAnnotation :: Lens' CreateHITWithHITType (Maybe Text)
chitwhittRequesterAnnotation = lens _chitwhittRequesterAnnotation (\ s a -> s{_chitwhittRequesterAnnotation = a})

-- | The number of times the HIT can be accepted and completed before the HIT becomes unavailable. 
chitwhittMaxAssignments :: Lens' CreateHITWithHITType (Maybe Int)
chitwhittMaxAssignments = lens _chitwhittMaxAssignments (\ s a -> s{_chitwhittMaxAssignments = a})

-- | The HITLayoutId allows you to use a pre-existing HIT design with placeholder values and create an additional HIT by providing those values as HITLayoutParameters.  Constraints: Either a Question parameter or a HITLayoutId parameter must be provided. 
chitwhittHITLayoutId :: Lens' CreateHITWithHITType (Maybe Text)
chitwhittHITLayoutId = lens _chitwhittHITLayoutId (\ s a -> s{_chitwhittHITLayoutId = a})

-- | If the HITLayoutId is provided, any placeholder values must be filled in with values using the HITLayoutParameter structure. For more information, see HITLayout. 
chitwhittHITLayoutParameters :: Lens' CreateHITWithHITType [HITLayoutParameter]
chitwhittHITLayoutParameters = lens _chitwhittHITLayoutParameters (\ s a -> s{_chitwhittHITLayoutParameters = a}) . _Default . _Coerce

-- | The data the person completing the HIT uses to produce the results.  Constraints: Must be a QuestionForm data structure, an ExternalQuestion data structure, or an HTMLQuestion data structure. The XML question data must not be larger than 64 kilobytes (65,535 bytes) in size, including whitespace.  Either a Question parameter or a HITLayoutId parameter must be provided.
chitwhittQuestion :: Lens' CreateHITWithHITType (Maybe Text)
chitwhittQuestion = lens _chitwhittQuestion (\ s a -> s{_chitwhittQuestion = a})

-- | The Assignment-level Review Policy applies to the assignments under the HIT. You can specify for Mechanical Turk to take various actions based on the policy. 
chitwhittAssignmentReviewPolicy :: Lens' CreateHITWithHITType (Maybe ReviewPolicy)
chitwhittAssignmentReviewPolicy = lens _chitwhittAssignmentReviewPolicy (\ s a -> s{_chitwhittAssignmentReviewPolicy = a})

-- | The HIT type ID you want to create this HIT with.
chitwhittHITTypeId :: Lens' CreateHITWithHITType Text
chitwhittHITTypeId = lens _chitwhittHITTypeId (\ s a -> s{_chitwhittHITTypeId = a})

-- | An amount of time, in seconds, after which the HIT is no longer available for users to accept. After the lifetime of the HIT elapses, the HIT no longer appears in HIT searches, even if not all of the assignments for the HIT have been accepted. 
chitwhittLifetimeInSeconds :: Lens' CreateHITWithHITType Integer
chitwhittLifetimeInSeconds = lens _chitwhittLifetimeInSeconds (\ s a -> s{_chitwhittLifetimeInSeconds = a})

instance AWSRequest CreateHITWithHITType where
        type Rs CreateHITWithHITType =
             CreateHITWithHITTypeResponse
        request = postJSON mechanicalTurk
        response
          = receiveJSON
              (\ s h x ->
                 CreateHITWithHITTypeResponse' <$>
                   (x .?> "HIT") <*> (pure (fromEnum s)))

instance Hashable CreateHITWithHITType where

instance NFData CreateHITWithHITType where

instance ToHeaders CreateHITWithHITType where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.CreateHITWithHITType"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateHITWithHITType where
        toJSON CreateHITWithHITType'{..}
          = object
              (catMaybes
                 [("HITReviewPolicy" .=) <$>
                    _chitwhittHITReviewPolicy,
                  ("UniqueRequestToken" .=) <$>
                    _chitwhittUniqueRequestToken,
                  ("RequesterAnnotation" .=) <$>
                    _chitwhittRequesterAnnotation,
                  ("MaxAssignments" .=) <$> _chitwhittMaxAssignments,
                  ("HITLayoutId" .=) <$> _chitwhittHITLayoutId,
                  ("HITLayoutParameters" .=) <$>
                    _chitwhittHITLayoutParameters,
                  ("Question" .=) <$> _chitwhittQuestion,
                  ("AssignmentReviewPolicy" .=) <$>
                    _chitwhittAssignmentReviewPolicy,
                  Just ("HITTypeId" .= _chitwhittHITTypeId),
                  Just
                    ("LifetimeInSeconds" .=
                       _chitwhittLifetimeInSeconds)])

instance ToPath CreateHITWithHITType where
        toPath = const "/"

instance ToQuery CreateHITWithHITType where
        toQuery = const mempty

-- | /See:/ 'createHITWithHITTypeResponse' smart constructor.
data CreateHITWithHITTypeResponse = CreateHITWithHITTypeResponse'{_chitwhittrsHIT
                                                                  ::
                                                                  !(Maybe HIT),
                                                                  _chitwhittrsResponseStatus
                                                                  :: !Int}
                                      deriving (Eq, Read, Show, Data, Typeable,
                                                Generic)

-- | Creates a value of 'CreateHITWithHITTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chitwhittrsHIT' - Contains the newly created HIT data. For a description of the HIT data structure as it appears in responses, see the HIT Data Structure documentation. 
--
-- * 'chitwhittrsResponseStatus' - -- | The response status code.
createHITWithHITTypeResponse
    :: Int -- ^ 'chitwhittrsResponseStatus'
    -> CreateHITWithHITTypeResponse
createHITWithHITTypeResponse pResponseStatus_
  = CreateHITWithHITTypeResponse'{_chitwhittrsHIT =
                                    Nothing,
                                  _chitwhittrsResponseStatus = pResponseStatus_}

-- | Contains the newly created HIT data. For a description of the HIT data structure as it appears in responses, see the HIT Data Structure documentation. 
chitwhittrsHIT :: Lens' CreateHITWithHITTypeResponse (Maybe HIT)
chitwhittrsHIT = lens _chitwhittrsHIT (\ s a -> s{_chitwhittrsHIT = a})

-- | -- | The response status code.
chitwhittrsResponseStatus :: Lens' CreateHITWithHITTypeResponse Int
chitwhittrsResponseStatus = lens _chitwhittrsResponseStatus (\ s a -> s{_chitwhittrsResponseStatus = a})

instance NFData CreateHITWithHITTypeResponse where

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
-- Module      : Network.AWS.GuardDuty.CreateThreatIntelSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new ThreatIntelSet. ThreatIntelSets consist of known malicious IP addresses. GuardDuty generates findings based on ThreatIntelSets.
module Network.AWS.GuardDuty.CreateThreatIntelSet
    (
    -- * Creating a Request
      createThreatIntelSet
    , CreateThreatIntelSet
    -- * Request Lenses
    , ctisLocation
    , ctisFormat
    , ctisActivate
    , ctisName
    , ctisDetectorId

    -- * Destructuring the Response
    , createThreatIntelSetResponse
    , CreateThreatIntelSetResponse
    -- * Response Lenses
    , ctisrsThreatIntelSetId
    , ctisrsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | CreateThreatIntelSet request body.
--
-- /See:/ 'createThreatIntelSet' smart constructor.
data CreateThreatIntelSet = CreateThreatIntelSet'{_ctisLocation
                                                  :: !(Maybe Text),
                                                  _ctisFormat ::
                                                  !(Maybe ThreatIntelSetFormat),
                                                  _ctisActivate ::
                                                  !(Maybe Bool),
                                                  _ctisName :: !(Maybe Text),
                                                  _ctisDetectorId :: !Text}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateThreatIntelSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctisLocation' - The URI of the file that contains the ThreatIntelSet. For example (https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key).
--
-- * 'ctisFormat' - The format of the file that contains the ThreatIntelSet.
--
-- * 'ctisActivate' - A boolean value that indicates whether GuardDuty is to start using the uploaded ThreatIntelSet.
--
-- * 'ctisName' - A user-friendly ThreatIntelSet name that is displayed in all finding generated by activity that involves IP addresses included in this ThreatIntelSet.
--
-- * 'ctisDetectorId' - The unique ID of the detector that you want to update.
createThreatIntelSet
    :: Text -- ^ 'ctisDetectorId'
    -> CreateThreatIntelSet
createThreatIntelSet pDetectorId_
  = CreateThreatIntelSet'{_ctisLocation = Nothing,
                          _ctisFormat = Nothing, _ctisActivate = Nothing,
                          _ctisName = Nothing, _ctisDetectorId = pDetectorId_}

-- | The URI of the file that contains the ThreatIntelSet. For example (https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key).
ctisLocation :: Lens' CreateThreatIntelSet (Maybe Text)
ctisLocation = lens _ctisLocation (\ s a -> s{_ctisLocation = a})

-- | The format of the file that contains the ThreatIntelSet.
ctisFormat :: Lens' CreateThreatIntelSet (Maybe ThreatIntelSetFormat)
ctisFormat = lens _ctisFormat (\ s a -> s{_ctisFormat = a})

-- | A boolean value that indicates whether GuardDuty is to start using the uploaded ThreatIntelSet.
ctisActivate :: Lens' CreateThreatIntelSet (Maybe Bool)
ctisActivate = lens _ctisActivate (\ s a -> s{_ctisActivate = a})

-- | A user-friendly ThreatIntelSet name that is displayed in all finding generated by activity that involves IP addresses included in this ThreatIntelSet.
ctisName :: Lens' CreateThreatIntelSet (Maybe Text)
ctisName = lens _ctisName (\ s a -> s{_ctisName = a})

-- | The unique ID of the detector that you want to update.
ctisDetectorId :: Lens' CreateThreatIntelSet Text
ctisDetectorId = lens _ctisDetectorId (\ s a -> s{_ctisDetectorId = a})

instance AWSRequest CreateThreatIntelSet where
        type Rs CreateThreatIntelSet =
             CreateThreatIntelSetResponse
        request = postJSON guardDuty
        response
          = receiveJSON
              (\ s h x ->
                 CreateThreatIntelSetResponse' <$>
                   (x .?> "threatIntelSetId") <*> (pure (fromEnum s)))

instance Hashable CreateThreatIntelSet where

instance NFData CreateThreatIntelSet where

instance ToHeaders CreateThreatIntelSet where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateThreatIntelSet where
        toJSON CreateThreatIntelSet'{..}
          = object
              (catMaybes
                 [("location" .=) <$> _ctisLocation,
                  ("format" .=) <$> _ctisFormat,
                  ("activate" .=) <$> _ctisActivate,
                  ("name" .=) <$> _ctisName])

instance ToPath CreateThreatIntelSet where
        toPath CreateThreatIntelSet'{..}
          = mconcat
              ["/detector/", toBS _ctisDetectorId,
               "/threatintelset"]

instance ToQuery CreateThreatIntelSet where
        toQuery = const mempty

-- | /See:/ 'createThreatIntelSetResponse' smart constructor.
data CreateThreatIntelSetResponse = CreateThreatIntelSetResponse'{_ctisrsThreatIntelSetId
                                                                  ::
                                                                  !(Maybe Text),
                                                                  _ctisrsResponseStatus
                                                                  :: !Int}
                                      deriving (Eq, Read, Show, Data, Typeable,
                                                Generic)

-- | Creates a value of 'CreateThreatIntelSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctisrsThreatIntelSetId' - Undocumented member.
--
-- * 'ctisrsResponseStatus' - -- | The response status code.
createThreatIntelSetResponse
    :: Int -- ^ 'ctisrsResponseStatus'
    -> CreateThreatIntelSetResponse
createThreatIntelSetResponse pResponseStatus_
  = CreateThreatIntelSetResponse'{_ctisrsThreatIntelSetId
                                    = Nothing,
                                  _ctisrsResponseStatus = pResponseStatus_}

-- | Undocumented member.
ctisrsThreatIntelSetId :: Lens' CreateThreatIntelSetResponse (Maybe Text)
ctisrsThreatIntelSetId = lens _ctisrsThreatIntelSetId (\ s a -> s{_ctisrsThreatIntelSetId = a})

-- | -- | The response status code.
ctisrsResponseStatus :: Lens' CreateThreatIntelSetResponse Int
ctisrsResponseStatus = lens _ctisrsResponseStatus (\ s a -> s{_ctisrsResponseStatus = a})

instance NFData CreateThreatIntelSetResponse where

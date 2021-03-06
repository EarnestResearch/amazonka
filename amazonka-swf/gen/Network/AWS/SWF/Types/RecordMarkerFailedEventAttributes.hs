{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.RecordMarkerFailedEventAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.RecordMarkerFailedEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.RecordMarkerFailedCause

-- | Provides the details of the @RecordMarkerFailed@ event.
--
--
--
-- /See:/ 'recordMarkerFailedEventAttributes' smart constructor.
data RecordMarkerFailedEventAttributes = RecordMarkerFailedEventAttributes'{_rmfeaMarkerName
                                                                            ::
                                                                            !Text,
                                                                            _rmfeaCause
                                                                            ::
                                                                            !RecordMarkerFailedCause,
                                                                            _rmfeaDecisionTaskCompletedEventId
                                                                            ::
                                                                            !Integer}
                                           deriving (Eq, Read, Show, Data,
                                                     Typeable, Generic)

-- | Creates a value of 'RecordMarkerFailedEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rmfeaMarkerName' - The marker's name.
--
-- * 'rmfeaCause' - The cause of the failure. This information is generated by the system and can be useful for diagnostic purposes.
--
-- * 'rmfeaDecisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @RecordMarkerFailed@ decision for this cancellation request. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
recordMarkerFailedEventAttributes
    :: Text -- ^ 'rmfeaMarkerName'
    -> RecordMarkerFailedCause -- ^ 'rmfeaCause'
    -> Integer -- ^ 'rmfeaDecisionTaskCompletedEventId'
    -> RecordMarkerFailedEventAttributes
recordMarkerFailedEventAttributes pMarkerName_
  pCause_ pDecisionTaskCompletedEventId_
  = RecordMarkerFailedEventAttributes'{_rmfeaMarkerName
                                         = pMarkerName_,
                                       _rmfeaCause = pCause_,
                                       _rmfeaDecisionTaskCompletedEventId =
                                         pDecisionTaskCompletedEventId_}

-- | The marker's name.
rmfeaMarkerName :: Lens' RecordMarkerFailedEventAttributes Text
rmfeaMarkerName = lens _rmfeaMarkerName (\ s a -> s{_rmfeaMarkerName = a})

-- | The cause of the failure. This information is generated by the system and can be useful for diagnostic purposes.
rmfeaCause :: Lens' RecordMarkerFailedEventAttributes RecordMarkerFailedCause
rmfeaCause = lens _rmfeaCause (\ s a -> s{_rmfeaCause = a})

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @RecordMarkerFailed@ decision for this cancellation request. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
rmfeaDecisionTaskCompletedEventId :: Lens' RecordMarkerFailedEventAttributes Integer
rmfeaDecisionTaskCompletedEventId = lens _rmfeaDecisionTaskCompletedEventId (\ s a -> s{_rmfeaDecisionTaskCompletedEventId = a})

instance FromJSON RecordMarkerFailedEventAttributes
         where
        parseJSON
          = withObject "RecordMarkerFailedEventAttributes"
              (\ x ->
                 RecordMarkerFailedEventAttributes' <$>
                   (x .: "markerName") <*> (x .: "cause") <*>
                     (x .: "decisionTaskCompletedEventId"))

instance Hashable RecordMarkerFailedEventAttributes
         where

instance NFData RecordMarkerFailedEventAttributes
         where

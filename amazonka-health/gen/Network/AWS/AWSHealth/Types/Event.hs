{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.Event
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AWSHealth.Types.Event where

import Network.AWS.AWSHealth.Types.EventStatusCode
import Network.AWS.AWSHealth.Types.EventTypeCategory
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Summary information about an AWS Health event.
--
--
--
-- /See:/ 'event' smart constructor.
data Event = Event'{_eLastUpdatedTime ::
                    !(Maybe POSIX),
                    _eArn :: !(Maybe Text), _eService :: !(Maybe Text),
                    _eStartTime :: !(Maybe POSIX),
                    _eEventTypeCode :: !(Maybe Text),
                    _eEventTypeCategory :: !(Maybe EventTypeCategory),
                    _eAvailabilityZone :: !(Maybe Text),
                    _eEndTime :: !(Maybe POSIX),
                    _eRegion :: !(Maybe Text),
                    _eStatusCode :: !(Maybe EventStatusCode)}
               deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Event' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eLastUpdatedTime' - The most recent date and time that the event was updated.
--
-- * 'eArn' - The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@ 
--
-- * 'eService' - The AWS service that is affected by the event. For example, @EC2@ , @RDS@ .
--
-- * 'eStartTime' - The date and time that the event began.
--
-- * 'eEventTypeCode' - The unique identifier for the event type. The format is @AWS_/SERVICE/ _/DESCRIPTION/ @ ; for example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ .
--
-- * 'eEventTypeCategory' - The category of the event. Possible values are @issue@ , @scheduledChange@ , and @accountNotification@ .
--
-- * 'eAvailabilityZone' - The AWS Availability Zone of the event. For example, us-east-1a.
--
-- * 'eEndTime' - The date and time that the event ended.
--
-- * 'eRegion' - The AWS region name of the event.
--
-- * 'eStatusCode' - The most recent status of the event. Possible values are @open@ , @closed@ , and @upcoming@ .
event
    :: Event
event
  = Event'{_eLastUpdatedTime = Nothing,
           _eArn = Nothing, _eService = Nothing,
           _eStartTime = Nothing, _eEventTypeCode = Nothing,
           _eEventTypeCategory = Nothing,
           _eAvailabilityZone = Nothing, _eEndTime = Nothing,
           _eRegion = Nothing, _eStatusCode = Nothing}

-- | The most recent date and time that the event was updated.
eLastUpdatedTime :: Lens' Event (Maybe UTCTime)
eLastUpdatedTime = lens _eLastUpdatedTime (\ s a -> s{_eLastUpdatedTime = a}) . mapping _Time

-- | The unique identifier for the event. Format: @arn:aws:health:/event-region/ ::event//SERVICE/ //EVENT_TYPE_CODE/ //EVENT_TYPE_PLUS_ID/ @ . Example: @Example: arn:aws:health:us-east-1::event/EC2/EC2_INSTANCE_RETIREMENT_SCHEDULED/EC2_INSTANCE_RETIREMENT_SCHEDULED_ABC123-DEF456@ 
eArn :: Lens' Event (Maybe Text)
eArn = lens _eArn (\ s a -> s{_eArn = a})

-- | The AWS service that is affected by the event. For example, @EC2@ , @RDS@ .
eService :: Lens' Event (Maybe Text)
eService = lens _eService (\ s a -> s{_eService = a})

-- | The date and time that the event began.
eStartTime :: Lens' Event (Maybe UTCTime)
eStartTime = lens _eStartTime (\ s a -> s{_eStartTime = a}) . mapping _Time

-- | The unique identifier for the event type. The format is @AWS_/SERVICE/ _/DESCRIPTION/ @ ; for example, @AWS_EC2_SYSTEM_MAINTENANCE_EVENT@ .
eEventTypeCode :: Lens' Event (Maybe Text)
eEventTypeCode = lens _eEventTypeCode (\ s a -> s{_eEventTypeCode = a})

-- | The category of the event. Possible values are @issue@ , @scheduledChange@ , and @accountNotification@ .
eEventTypeCategory :: Lens' Event (Maybe EventTypeCategory)
eEventTypeCategory = lens _eEventTypeCategory (\ s a -> s{_eEventTypeCategory = a})

-- | The AWS Availability Zone of the event. For example, us-east-1a.
eAvailabilityZone :: Lens' Event (Maybe Text)
eAvailabilityZone = lens _eAvailabilityZone (\ s a -> s{_eAvailabilityZone = a})

-- | The date and time that the event ended.
eEndTime :: Lens' Event (Maybe UTCTime)
eEndTime = lens _eEndTime (\ s a -> s{_eEndTime = a}) . mapping _Time

-- | The AWS region name of the event.
eRegion :: Lens' Event (Maybe Text)
eRegion = lens _eRegion (\ s a -> s{_eRegion = a})

-- | The most recent status of the event. Possible values are @open@ , @closed@ , and @upcoming@ .
eStatusCode :: Lens' Event (Maybe EventStatusCode)
eStatusCode = lens _eStatusCode (\ s a -> s{_eStatusCode = a})

instance FromJSON Event where
        parseJSON
          = withObject "Event"
              (\ x ->
                 Event' <$>
                   (x .:? "lastUpdatedTime") <*> (x .:? "arn") <*>
                     (x .:? "service")
                     <*> (x .:? "startTime")
                     <*> (x .:? "eventTypeCode")
                     <*> (x .:? "eventTypeCategory")
                     <*> (x .:? "availabilityZone")
                     <*> (x .:? "endTime")
                     <*> (x .:? "region")
                     <*> (x .:? "statusCode"))

instance Hashable Event where

instance NFData Event where

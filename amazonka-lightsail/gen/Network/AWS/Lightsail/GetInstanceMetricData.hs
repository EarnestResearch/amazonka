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
-- Module      : Network.AWS.Lightsail.GetInstanceMetricData
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the data points for the specified Amazon Lightsail instance metric, given an instance name.
--
--
module Network.AWS.Lightsail.GetInstanceMetricData
    (
    -- * Creating a Request
      getInstanceMetricData
    , GetInstanceMetricData
    -- * Request Lenses
    , gimdInstanceName
    , gimdMetricName
    , gimdPeriod
    , gimdStartTime
    , gimdEndTime
    , gimdUnit
    , gimdStatistics

    -- * Destructuring the Response
    , getInstanceMetricDataResponse
    , GetInstanceMetricDataResponse
    -- * Response Lenses
    , gimdrsMetricName
    , gimdrsMetricData
    , gimdrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getInstanceMetricData' smart constructor.
data GetInstanceMetricData = GetInstanceMetricData'{_gimdInstanceName
                                                    :: !Text,
                                                    _gimdMetricName ::
                                                    !InstanceMetricName,
                                                    _gimdPeriod :: !Nat,
                                                    _gimdStartTime :: !POSIX,
                                                    _gimdEndTime :: !POSIX,
                                                    _gimdUnit :: !MetricUnit,
                                                    _gimdStatistics ::
                                                    ![MetricStatistic]}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'GetInstanceMetricData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gimdInstanceName' - The name of the instance for which you want to get metrics data.
--
-- * 'gimdMetricName' - The metric for which you want to return information. Valid instance metric names are listed below, along with the most useful @statistics@ to include in your request, and the published @unit@ value.     * __@CPUUtilization@ __ — The percentage of allocated compute units that are currently in use on the instance. This metric identifies the processing power to run the applications on the instance. Tools in your operating system can show a lower percentage than Lightsail when the instance is not allocated a full processor core. @Statistics@ : The most useful statistics are @Maximum@ and @Average@ . @Unit@ : The published unit is @Percent@ .     * __@NetworkIn@ __ — The number of bytes received on all network interfaces by the instance. This metric identifies the volume of incoming network traffic to the instance. The number reported is the number of bytes received during the period. Because this metric is reported in 5-minute intervals, divide the reported number by 300 to find Bytes/second. @Statistics@ : The most useful statistic is @Sum@ . @Unit@ : The published unit is @Bytes@ .     * __@NetworkOut@ __ — The number of bytes sent out on all network interfaces by the instance. This metric identifies the volume of outgoing network traffic from the instance. The number reported is the number of bytes sent during the period. Because this metric is reported in 5-minute intervals, divide the reported number by 300 to find Bytes/second. @Statistics@ : The most useful statistic is @Sum@ . @Unit@ : The published unit is @Bytes@ .     * __@StatusCheckFailed@ __ — Reports whether the instance passed or failed both the instance status check and the system status check. This metric can be either 0 (passed) or 1 (failed). This metric data is available in 1-minute (60 seconds) granularity. @Statistics@ : The most useful statistic is @Sum@ . @Unit@ : The published unit is @Count@ .     * __@StatusCheckFailed_Instance@ __ — Reports whether the instance passed or failed the instance status check. This metric can be either 0 (passed) or 1 (failed). This metric data is available in 1-minute (60 seconds) granularity. @Statistics@ : The most useful statistic is @Sum@ . @Unit@ : The published unit is @Count@ .     * __@StatusCheckFailed_System@ __ — Reports whether the instance passed or failed the system status check. This metric can be either 0 (passed) or 1 (failed). This metric data is available in 1-minute (60 seconds) granularity. @Statistics@ : The most useful statistic is @Sum@ . @Unit@ : The published unit is @Count@ .
--
-- * 'gimdPeriod' - The granularity, in seconds, of the returned data points. The @StatusCheckFailed@ , @StatusCheckFailed_Instance@ , and @StatusCheckFailed_System@ instance metric data is available in 1-minute (60 seconds) granularity. All other instance metric data is available in 5-minute (300 seconds) granularity.
--
-- * 'gimdStartTime' - The start time of the time period.
--
-- * 'gimdEndTime' - The end time of the time period.
--
-- * 'gimdUnit' - The unit for the metric data request. Valid units depend on the metric data being required. For the valid units with each available metric, see the @metricName@ parameter.
--
-- * 'gimdStatistics' - The statistic for the metric. The following statistics are available:     * @Minimum@ — The lowest value observed during the specified period. Use this value to determine low volumes of activity for your application.     * @Maximum@ — The highest value observed during the specified period. Use this value to determine high volumes of activity for your application.     * @Sum@ — All values submitted for the matching metric added together. You can use this statistic to determine the total volume of a metric.     * @Average@ — The value of Sum / SampleCount during the specified period. By comparing this statistic with the Minimum and Maximum values, you can determine the full scope of a metric and how close the average use is to the Minimum and Maximum values. This comparison helps you to know when to increase or decrease your resources.     * @SampleCount@ — The count, or number, of data points used for the statistical calculation.
getInstanceMetricData
    :: Text -- ^ 'gimdInstanceName'
    -> InstanceMetricName -- ^ 'gimdMetricName'
    -> Natural -- ^ 'gimdPeriod'
    -> UTCTime -- ^ 'gimdStartTime'
    -> UTCTime -- ^ 'gimdEndTime'
    -> MetricUnit -- ^ 'gimdUnit'
    -> GetInstanceMetricData
getInstanceMetricData pInstanceName_ pMetricName_
  pPeriod_ pStartTime_ pEndTime_ pUnit_
  = GetInstanceMetricData'{_gimdInstanceName =
                             pInstanceName_,
                           _gimdMetricName = pMetricName_,
                           _gimdPeriod = _Nat # pPeriod_,
                           _gimdStartTime = _Time # pStartTime_,
                           _gimdEndTime = _Time # pEndTime_, _gimdUnit = pUnit_,
                           _gimdStatistics = mempty}

-- | The name of the instance for which you want to get metrics data.
gimdInstanceName :: Lens' GetInstanceMetricData Text
gimdInstanceName = lens _gimdInstanceName (\ s a -> s{_gimdInstanceName = a})

-- | The metric for which you want to return information. Valid instance metric names are listed below, along with the most useful @statistics@ to include in your request, and the published @unit@ value.     * __@CPUUtilization@ __ — The percentage of allocated compute units that are currently in use on the instance. This metric identifies the processing power to run the applications on the instance. Tools in your operating system can show a lower percentage than Lightsail when the instance is not allocated a full processor core. @Statistics@ : The most useful statistics are @Maximum@ and @Average@ . @Unit@ : The published unit is @Percent@ .     * __@NetworkIn@ __ — The number of bytes received on all network interfaces by the instance. This metric identifies the volume of incoming network traffic to the instance. The number reported is the number of bytes received during the period. Because this metric is reported in 5-minute intervals, divide the reported number by 300 to find Bytes/second. @Statistics@ : The most useful statistic is @Sum@ . @Unit@ : The published unit is @Bytes@ .     * __@NetworkOut@ __ — The number of bytes sent out on all network interfaces by the instance. This metric identifies the volume of outgoing network traffic from the instance. The number reported is the number of bytes sent during the period. Because this metric is reported in 5-minute intervals, divide the reported number by 300 to find Bytes/second. @Statistics@ : The most useful statistic is @Sum@ . @Unit@ : The published unit is @Bytes@ .     * __@StatusCheckFailed@ __ — Reports whether the instance passed or failed both the instance status check and the system status check. This metric can be either 0 (passed) or 1 (failed). This metric data is available in 1-minute (60 seconds) granularity. @Statistics@ : The most useful statistic is @Sum@ . @Unit@ : The published unit is @Count@ .     * __@StatusCheckFailed_Instance@ __ — Reports whether the instance passed or failed the instance status check. This metric can be either 0 (passed) or 1 (failed). This metric data is available in 1-minute (60 seconds) granularity. @Statistics@ : The most useful statistic is @Sum@ . @Unit@ : The published unit is @Count@ .     * __@StatusCheckFailed_System@ __ — Reports whether the instance passed or failed the system status check. This metric can be either 0 (passed) or 1 (failed). This metric data is available in 1-minute (60 seconds) granularity. @Statistics@ : The most useful statistic is @Sum@ . @Unit@ : The published unit is @Count@ .
gimdMetricName :: Lens' GetInstanceMetricData InstanceMetricName
gimdMetricName = lens _gimdMetricName (\ s a -> s{_gimdMetricName = a})

-- | The granularity, in seconds, of the returned data points. The @StatusCheckFailed@ , @StatusCheckFailed_Instance@ , and @StatusCheckFailed_System@ instance metric data is available in 1-minute (60 seconds) granularity. All other instance metric data is available in 5-minute (300 seconds) granularity.
gimdPeriod :: Lens' GetInstanceMetricData Natural
gimdPeriod = lens _gimdPeriod (\ s a -> s{_gimdPeriod = a}) . _Nat

-- | The start time of the time period.
gimdStartTime :: Lens' GetInstanceMetricData UTCTime
gimdStartTime = lens _gimdStartTime (\ s a -> s{_gimdStartTime = a}) . _Time

-- | The end time of the time period.
gimdEndTime :: Lens' GetInstanceMetricData UTCTime
gimdEndTime = lens _gimdEndTime (\ s a -> s{_gimdEndTime = a}) . _Time

-- | The unit for the metric data request. Valid units depend on the metric data being required. For the valid units with each available metric, see the @metricName@ parameter.
gimdUnit :: Lens' GetInstanceMetricData MetricUnit
gimdUnit = lens _gimdUnit (\ s a -> s{_gimdUnit = a})

-- | The statistic for the metric. The following statistics are available:     * @Minimum@ — The lowest value observed during the specified period. Use this value to determine low volumes of activity for your application.     * @Maximum@ — The highest value observed during the specified period. Use this value to determine high volumes of activity for your application.     * @Sum@ — All values submitted for the matching metric added together. You can use this statistic to determine the total volume of a metric.     * @Average@ — The value of Sum / SampleCount during the specified period. By comparing this statistic with the Minimum and Maximum values, you can determine the full scope of a metric and how close the average use is to the Minimum and Maximum values. This comparison helps you to know when to increase or decrease your resources.     * @SampleCount@ — The count, or number, of data points used for the statistical calculation.
gimdStatistics :: Lens' GetInstanceMetricData [MetricStatistic]
gimdStatistics = lens _gimdStatistics (\ s a -> s{_gimdStatistics = a}) . _Coerce

instance AWSRequest GetInstanceMetricData where
        type Rs GetInstanceMetricData =
             GetInstanceMetricDataResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetInstanceMetricDataResponse' <$>
                   (x .?> "metricName") <*>
                     (x .?> "metricData" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetInstanceMetricData where

instance NFData GetInstanceMetricData where

instance ToHeaders GetInstanceMetricData where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetInstanceMetricData" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetInstanceMetricData where
        toJSON GetInstanceMetricData'{..}
          = object
              (catMaybes
                 [Just ("instanceName" .= _gimdInstanceName),
                  Just ("metricName" .= _gimdMetricName),
                  Just ("period" .= _gimdPeriod),
                  Just ("startTime" .= _gimdStartTime),
                  Just ("endTime" .= _gimdEndTime),
                  Just ("unit" .= _gimdUnit),
                  Just ("statistics" .= _gimdStatistics)])

instance ToPath GetInstanceMetricData where
        toPath = const "/"

instance ToQuery GetInstanceMetricData where
        toQuery = const mempty

-- | /See:/ 'getInstanceMetricDataResponse' smart constructor.
data GetInstanceMetricDataResponse = GetInstanceMetricDataResponse'{_gimdrsMetricName
                                                                    ::
                                                                    !(Maybe
                                                                        InstanceMetricName),
                                                                    _gimdrsMetricData
                                                                    ::
                                                                    !(Maybe
                                                                        [MetricDatapoint]),
                                                                    _gimdrsResponseStatus
                                                                    :: !Int}
                                       deriving (Eq, Read, Show, Data, Typeable,
                                                 Generic)

-- | Creates a value of 'GetInstanceMetricDataResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gimdrsMetricName' - The metric name to return data for.
--
-- * 'gimdrsMetricData' - An array of key-value pairs containing information about the results of your get instance metric data request.
--
-- * 'gimdrsResponseStatus' - -- | The response status code.
getInstanceMetricDataResponse
    :: Int -- ^ 'gimdrsResponseStatus'
    -> GetInstanceMetricDataResponse
getInstanceMetricDataResponse pResponseStatus_
  = GetInstanceMetricDataResponse'{_gimdrsMetricName =
                                     Nothing,
                                   _gimdrsMetricData = Nothing,
                                   _gimdrsResponseStatus = pResponseStatus_}

-- | The metric name to return data for.
gimdrsMetricName :: Lens' GetInstanceMetricDataResponse (Maybe InstanceMetricName)
gimdrsMetricName = lens _gimdrsMetricName (\ s a -> s{_gimdrsMetricName = a})

-- | An array of key-value pairs containing information about the results of your get instance metric data request.
gimdrsMetricData :: Lens' GetInstanceMetricDataResponse [MetricDatapoint]
gimdrsMetricData = lens _gimdrsMetricData (\ s a -> s{_gimdrsMetricData = a}) . _Default . _Coerce

-- | -- | The response status code.
gimdrsResponseStatus :: Lens' GetInstanceMetricDataResponse Int
gimdrsResponseStatus = lens _gimdrsResponseStatus (\ s a -> s{_gimdrsResponseStatus = a})

instance NFData GetInstanceMetricDataResponse where

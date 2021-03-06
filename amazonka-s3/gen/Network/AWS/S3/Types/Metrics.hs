{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Metrics
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.Metrics where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.MetricsStatus
import Network.AWS.S3.Types.ReplicationTimeValue

-- | A container specifying replication metrics-related settings enabling metrics and Amazon S3 events for S3 Replication Time Control (S3 RTC). Must be specified together with a @ReplicationTime@ block. 
--
--
--
-- /See:/ 'metrics' smart constructor.
data Metrics = Metrics'{_mStatus :: !MetricsStatus,
                        _mEventThreshold :: !ReplicationTimeValue}
                 deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Metrics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mStatus' - Specifies whether the replication metrics are enabled. 
--
-- * 'mEventThreshold' - A container specifying the time threshold for emitting the @s3:Replication:OperationMissedThreshold@ event. 
metrics
    :: MetricsStatus -- ^ 'mStatus'
    -> ReplicationTimeValue -- ^ 'mEventThreshold'
    -> Metrics
metrics pStatus_ pEventThreshold_
  = Metrics'{_mStatus = pStatus_,
             _mEventThreshold = pEventThreshold_}

-- | Specifies whether the replication metrics are enabled. 
mStatus :: Lens' Metrics MetricsStatus
mStatus = lens _mStatus (\ s a -> s{_mStatus = a})

-- | A container specifying the time threshold for emitting the @s3:Replication:OperationMissedThreshold@ event. 
mEventThreshold :: Lens' Metrics ReplicationTimeValue
mEventThreshold = lens _mEventThreshold (\ s a -> s{_mEventThreshold = a})

instance FromXML Metrics where
        parseXML x
          = Metrics' <$>
              (x .@ "Status") <*> (x .@ "EventThreshold")

instance Hashable Metrics where

instance NFData Metrics where

instance ToXML Metrics where
        toXML Metrics'{..}
          = mconcat
              ["Status" @= _mStatus,
               "EventThreshold" @= _mEventThreshold]

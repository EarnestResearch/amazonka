{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetSummary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.DatasetSummary where

import Network.AWS.IoTAnalytics.Types.DatasetStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A summary of information about a data set.
--
--
--
-- /See:/ 'datasetSummary' smart constructor.
data DatasetSummary = DatasetSummary'{_dssCreationTime
                                      :: !(Maybe POSIX),
                                      _dssStatus :: !(Maybe DatasetStatus),
                                      _dssDatasetName :: !(Maybe Text),
                                      _dssLastUpdateTime :: !(Maybe POSIX)}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DatasetSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssCreationTime' - The time the data set was created.
--
-- * 'dssStatus' - The status of the data set.
--
-- * 'dssDatasetName' - The name of the data set.
--
-- * 'dssLastUpdateTime' - The last time the data set was updated.
datasetSummary
    :: DatasetSummary
datasetSummary
  = DatasetSummary'{_dssCreationTime = Nothing,
                    _dssStatus = Nothing, _dssDatasetName = Nothing,
                    _dssLastUpdateTime = Nothing}

-- | The time the data set was created.
dssCreationTime :: Lens' DatasetSummary (Maybe UTCTime)
dssCreationTime = lens _dssCreationTime (\ s a -> s{_dssCreationTime = a}) . mapping _Time

-- | The status of the data set.
dssStatus :: Lens' DatasetSummary (Maybe DatasetStatus)
dssStatus = lens _dssStatus (\ s a -> s{_dssStatus = a})

-- | The name of the data set.
dssDatasetName :: Lens' DatasetSummary (Maybe Text)
dssDatasetName = lens _dssDatasetName (\ s a -> s{_dssDatasetName = a})

-- | The last time the data set was updated.
dssLastUpdateTime :: Lens' DatasetSummary (Maybe UTCTime)
dssLastUpdateTime = lens _dssLastUpdateTime (\ s a -> s{_dssLastUpdateTime = a}) . mapping _Time

instance FromJSON DatasetSummary where
        parseJSON
          = withObject "DatasetSummary"
              (\ x ->
                 DatasetSummary' <$>
                   (x .:? "creationTime") <*> (x .:? "status") <*>
                     (x .:? "datasetName")
                     <*> (x .:? "lastUpdateTime"))

instance Hashable DatasetSummary where

instance NFData DatasetSummary where

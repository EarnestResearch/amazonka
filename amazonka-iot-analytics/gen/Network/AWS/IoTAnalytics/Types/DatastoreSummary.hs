{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatastoreSummary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.DatastoreSummary where

import Network.AWS.IoTAnalytics.Types.DatastoreStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A summary of information about a data store.
--
--
--
-- /See:/ 'datastoreSummary' smart constructor.
data DatastoreSummary = DatastoreSummary'{_dsCreationTime
                                          :: !(Maybe POSIX),
                                          _dsStatus :: !(Maybe DatastoreStatus),
                                          _dsDatastoreName :: !(Maybe Text),
                                          _dsLastUpdateTime :: !(Maybe POSIX)}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DatastoreSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsCreationTime' - When the data store was created.
--
-- * 'dsStatus' - The status of the data store.
--
-- * 'dsDatastoreName' - The name of the data store.
--
-- * 'dsLastUpdateTime' - The last time the data store was updated.
datastoreSummary
    :: DatastoreSummary
datastoreSummary
  = DatastoreSummary'{_dsCreationTime = Nothing,
                      _dsStatus = Nothing, _dsDatastoreName = Nothing,
                      _dsLastUpdateTime = Nothing}

-- | When the data store was created.
dsCreationTime :: Lens' DatastoreSummary (Maybe UTCTime)
dsCreationTime = lens _dsCreationTime (\ s a -> s{_dsCreationTime = a}) . mapping _Time

-- | The status of the data store.
dsStatus :: Lens' DatastoreSummary (Maybe DatastoreStatus)
dsStatus = lens _dsStatus (\ s a -> s{_dsStatus = a})

-- | The name of the data store.
dsDatastoreName :: Lens' DatastoreSummary (Maybe Text)
dsDatastoreName = lens _dsDatastoreName (\ s a -> s{_dsDatastoreName = a})

-- | The last time the data store was updated.
dsLastUpdateTime :: Lens' DatastoreSummary (Maybe UTCTime)
dsLastUpdateTime = lens _dsLastUpdateTime (\ s a -> s{_dsLastUpdateTime = a}) . mapping _Time

instance FromJSON DatastoreSummary where
        parseJSON
          = withObject "DatastoreSummary"
              (\ x ->
                 DatastoreSummary' <$>
                   (x .:? "creationTime") <*> (x .:? "status") <*>
                     (x .:? "datastoreName")
                     <*> (x .:? "lastUpdateTime"))

instance Hashable DatastoreSummary where

instance NFData DatastoreSummary where

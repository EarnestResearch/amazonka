{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.PartitionInput
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.PartitionInput where

import Network.AWS.Glue.Types.StorageDescriptor
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The structure used to create and update a partion.
--
--
--
-- /See:/ 'partitionInput' smart constructor.
data PartitionInput = PartitionInput'{_piValues ::
                                      !(Maybe [Text]),
                                      _piLastAnalyzedTime :: !(Maybe POSIX),
                                      _piStorageDescriptor ::
                                      !(Maybe StorageDescriptor),
                                      _piParameters :: !(Maybe (Map Text Text)),
                                      _piLastAccessTime :: !(Maybe POSIX)}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PartitionInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piValues' - The values of the partition.
--
-- * 'piLastAnalyzedTime' - The last time at which column statistics were computed for this partition.
--
-- * 'piStorageDescriptor' - Provides information about the physical location where the partition is stored.
--
-- * 'piParameters' - Partition parameters, in the form of a list of key-value pairs.
--
-- * 'piLastAccessTime' - The last time at which the partition was accessed.
partitionInput
    :: PartitionInput
partitionInput
  = PartitionInput'{_piValues = Nothing,
                    _piLastAnalyzedTime = Nothing,
                    _piStorageDescriptor = Nothing,
                    _piParameters = Nothing, _piLastAccessTime = Nothing}

-- | The values of the partition.
piValues :: Lens' PartitionInput [Text]
piValues = lens _piValues (\ s a -> s{_piValues = a}) . _Default . _Coerce

-- | The last time at which column statistics were computed for this partition.
piLastAnalyzedTime :: Lens' PartitionInput (Maybe UTCTime)
piLastAnalyzedTime = lens _piLastAnalyzedTime (\ s a -> s{_piLastAnalyzedTime = a}) . mapping _Time

-- | Provides information about the physical location where the partition is stored.
piStorageDescriptor :: Lens' PartitionInput (Maybe StorageDescriptor)
piStorageDescriptor = lens _piStorageDescriptor (\ s a -> s{_piStorageDescriptor = a})

-- | Partition parameters, in the form of a list of key-value pairs.
piParameters :: Lens' PartitionInput (HashMap Text Text)
piParameters = lens _piParameters (\ s a -> s{_piParameters = a}) . _Default . _Map

-- | The last time at which the partition was accessed.
piLastAccessTime :: Lens' PartitionInput (Maybe UTCTime)
piLastAccessTime = lens _piLastAccessTime (\ s a -> s{_piLastAccessTime = a}) . mapping _Time

instance Hashable PartitionInput where

instance NFData PartitionInput where

instance ToJSON PartitionInput where
        toJSON PartitionInput'{..}
          = object
              (catMaybes
                 [("Values" .=) <$> _piValues,
                  ("LastAnalyzedTime" .=) <$> _piLastAnalyzedTime,
                  ("StorageDescriptor" .=) <$> _piStorageDescriptor,
                  ("Parameters" .=) <$> _piParameters,
                  ("LastAccessTime" .=) <$> _piLastAccessTime])

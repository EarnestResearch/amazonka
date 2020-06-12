{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.FileSystemSize
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EFS.Types.FileSystemSize where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Latest known metered size (in bytes) of data stored in the file system, in its @Value@ field, and the time at which that size was determined in its @Timestamp@ field. Note that the value does not represent the size of a consistent snapshot of the file system, but it is eventually consistent when there are no writes to the file system. That is, the value will represent the actual size only if the file system is not modified for a period longer than a couple of hours. Otherwise, the value is not necessarily the exact size the file system was at any instant in time.
--
--
--
-- /See:/ 'fileSystemSize' smart constructor.
data FileSystemSize = FileSystemSize'{_fssTimestamp
                                      :: !(Maybe POSIX),
                                      _fssValue :: !Nat}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FileSystemSize' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fssTimestamp' - Time at which the size of data, returned in the @Value@ field, was determined. The value is the integer number of seconds since 1970-01-01T00:00:00Z.
--
-- * 'fssValue' - Latest known metered size (in bytes) of data stored in the file system.
fileSystemSize
    :: Natural -- ^ 'fssValue'
    -> FileSystemSize
fileSystemSize pValue_
  = FileSystemSize'{_fssTimestamp = Nothing,
                    _fssValue = _Nat # pValue_}

-- | Time at which the size of data, returned in the @Value@ field, was determined. The value is the integer number of seconds since 1970-01-01T00:00:00Z.
fssTimestamp :: Lens' FileSystemSize (Maybe UTCTime)
fssTimestamp = lens _fssTimestamp (\ s a -> s{_fssTimestamp = a}) . mapping _Time

-- | Latest known metered size (in bytes) of data stored in the file system.
fssValue :: Lens' FileSystemSize Natural
fssValue = lens _fssValue (\ s a -> s{_fssValue = a}) . _Nat

instance FromJSON FileSystemSize where
        parseJSON
          = withObject "FileSystemSize"
              (\ x ->
                 FileSystemSize' <$>
                   (x .:? "Timestamp") <*> (x .: "Value"))

instance Hashable FileSystemSize where

instance NFData FileSystemSize where

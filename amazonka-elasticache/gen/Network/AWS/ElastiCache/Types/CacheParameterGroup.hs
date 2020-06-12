{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CacheParameterGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.CacheParameterGroup where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a @CreateCacheParameterGroup@ operation.
--
--
--
-- /See:/ 'cacheParameterGroup' smart constructor.
data CacheParameterGroup = CacheParameterGroup'{_cpgCacheParameterGroupFamily
                                                :: !(Maybe Text),
                                                _cpgCacheParameterGroupName ::
                                                !(Maybe Text),
                                                _cpgDescription ::
                                                !(Maybe Text)}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CacheParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpgCacheParameterGroupFamily' - The name of the cache parameter group family that this cache parameter group is compatible with. Valid values are: @memcached1.4@ | @redis2.6@ | @redis2.8@ | @redis3.2@ 
--
-- * 'cpgCacheParameterGroupName' - The name of the cache parameter group.
--
-- * 'cpgDescription' - The description for this cache parameter group.
cacheParameterGroup
    :: CacheParameterGroup
cacheParameterGroup
  = CacheParameterGroup'{_cpgCacheParameterGroupFamily
                           = Nothing,
                         _cpgCacheParameterGroupName = Nothing,
                         _cpgDescription = Nothing}

-- | The name of the cache parameter group family that this cache parameter group is compatible with. Valid values are: @memcached1.4@ | @redis2.6@ | @redis2.8@ | @redis3.2@ 
cpgCacheParameterGroupFamily :: Lens' CacheParameterGroup (Maybe Text)
cpgCacheParameterGroupFamily = lens _cpgCacheParameterGroupFamily (\ s a -> s{_cpgCacheParameterGroupFamily = a})

-- | The name of the cache parameter group.
cpgCacheParameterGroupName :: Lens' CacheParameterGroup (Maybe Text)
cpgCacheParameterGroupName = lens _cpgCacheParameterGroupName (\ s a -> s{_cpgCacheParameterGroupName = a})

-- | The description for this cache parameter group.
cpgDescription :: Lens' CacheParameterGroup (Maybe Text)
cpgDescription = lens _cpgDescription (\ s a -> s{_cpgDescription = a})

instance FromXML CacheParameterGroup where
        parseXML x
          = CacheParameterGroup' <$>
              (x .@? "CacheParameterGroupFamily") <*>
                (x .@? "CacheParameterGroupName")
                <*> (x .@? "Description")

instance Hashable CacheParameterGroup where

instance NFData CacheParameterGroup where

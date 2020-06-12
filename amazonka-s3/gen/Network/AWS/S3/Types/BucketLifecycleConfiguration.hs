{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.BucketLifecycleConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.BucketLifecycleConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.LifecycleRule

-- | /See:/ 'bucketLifecycleConfiguration' smart constructor.
newtype BucketLifecycleConfiguration = BucketLifecycleConfiguration'{_blcRules
                                                                     ::
                                                                     [LifecycleRule]}
                                         deriving (Eq, Read, Show, Data,
                                                   Typeable, Generic)

-- | Creates a value of 'BucketLifecycleConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blcRules' - Undocumented member.
bucketLifecycleConfiguration
    :: BucketLifecycleConfiguration
bucketLifecycleConfiguration
  = BucketLifecycleConfiguration'{_blcRules = mempty}

-- | Undocumented member.
blcRules :: Lens' BucketLifecycleConfiguration [LifecycleRule]
blcRules = lens _blcRules (\ s a -> s{_blcRules = a}) . _Coerce

instance Hashable BucketLifecycleConfiguration where

instance NFData BucketLifecycleConfiguration where

instance ToXML BucketLifecycleConfiguration where
        toXML BucketLifecycleConfiguration'{..}
          = mconcat [toXMLList "Rule" _blcRules]

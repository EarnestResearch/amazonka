{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ReshardingConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.ReshardingConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of @PreferredAvailabilityZones@ objects that specifies the configuration of a node group in the resharded cluster.
--
--
--
-- /See:/ 'reshardingConfiguration' smart constructor.
newtype ReshardingConfiguration = ReshardingConfiguration'{_rcPreferredAvailabilityZones
                                                           :: Maybe [Text]}
                                    deriving (Eq, Read, Show, Data, Typeable,
                                              Generic)

-- | Creates a value of 'ReshardingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcPreferredAvailabilityZones' - A list of preferred availability zones for the nodes in this cluster.
reshardingConfiguration
    :: ReshardingConfiguration
reshardingConfiguration
  = ReshardingConfiguration'{_rcPreferredAvailabilityZones
                               = Nothing}

-- | A list of preferred availability zones for the nodes in this cluster.
rcPreferredAvailabilityZones :: Lens' ReshardingConfiguration [Text]
rcPreferredAvailabilityZones = lens _rcPreferredAvailabilityZones (\ s a -> s{_rcPreferredAvailabilityZones = a}) . _Default . _Coerce

instance Hashable ReshardingConfiguration where

instance NFData ReshardingConfiguration where

instance ToQuery ReshardingConfiguration where
        toQuery ReshardingConfiguration'{..}
          = mconcat
              ["PreferredAvailabilityZones" =:
                 toQuery
                   (toQueryList "AvailabilityZone" <$>
                      _rcPreferredAvailabilityZones)]

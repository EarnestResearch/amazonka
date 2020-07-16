{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexGroupSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.MultiplexGroupSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Multiplex Group Settings
--
-- /See:/ 'multiplexGroupSettings' smart constructor.
data MultiplexGroupSettings = MultiplexGroupSettings'
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'MultiplexGroupSettings' with the minimum fields required to make a request.
--
multiplexGroupSettings
    :: MultiplexGroupSettings
multiplexGroupSettings = MultiplexGroupSettings'

instance FromJSON MultiplexGroupSettings where
        parseJSON
          = withObject "MultiplexGroupSettings"
              (\ x -> pure MultiplexGroupSettings')

instance Hashable MultiplexGroupSettings where

instance NFData MultiplexGroupSettings where

instance ToJSON MultiplexGroupSettings where
        toJSON = const (Object mempty)

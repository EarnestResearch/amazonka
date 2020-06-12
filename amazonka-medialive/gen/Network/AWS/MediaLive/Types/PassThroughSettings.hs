{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.PassThroughSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.PassThroughSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Placeholder documentation for PassThroughSettings
--
-- /See:/ 'passThroughSettings' smart constructor.
data PassThroughSettings = PassThroughSettings'
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PassThroughSettings' with the minimum fields required to make a request.
--
passThroughSettings
    :: PassThroughSettings
passThroughSettings = PassThroughSettings'

instance FromJSON PassThroughSettings where
        parseJSON
          = withObject "PassThroughSettings"
              (\ x -> pure PassThroughSettings')

instance Hashable PassThroughSettings where

instance NFData PassThroughSettings where

instance ToJSON PassThroughSettings where
        toJSON = const (Object mempty)

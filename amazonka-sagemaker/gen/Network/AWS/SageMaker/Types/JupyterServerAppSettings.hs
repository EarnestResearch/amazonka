{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.JupyterServerAppSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.JupyterServerAppSettings where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.ResourceSpec

-- | Jupyter server's app settings.
--
--
--
-- /See:/ 'jupyterServerAppSettings' smart constructor.
newtype JupyterServerAppSettings = JupyterServerAppSettings'{_jsasDefaultResourceSpec
                                                             ::
                                                             Maybe ResourceSpec}
                                     deriving (Eq, Read, Show, Data, Typeable,
                                               Generic)

-- | Creates a value of 'JupyterServerAppSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jsasDefaultResourceSpec' - The instance type and quantity.
jupyterServerAppSettings
    :: JupyterServerAppSettings
jupyterServerAppSettings
  = JupyterServerAppSettings'{_jsasDefaultResourceSpec
                                = Nothing}

-- | The instance type and quantity.
jsasDefaultResourceSpec :: Lens' JupyterServerAppSettings (Maybe ResourceSpec)
jsasDefaultResourceSpec = lens _jsasDefaultResourceSpec (\ s a -> s{_jsasDefaultResourceSpec = a})

instance FromJSON JupyterServerAppSettings where
        parseJSON
          = withObject "JupyterServerAppSettings"
              (\ x ->
                 JupyterServerAppSettings' <$>
                   (x .:? "DefaultResourceSpec"))

instance Hashable JupyterServerAppSettings where

instance NFData JupyterServerAppSettings where

instance ToJSON JupyterServerAppSettings where
        toJSON JupyterServerAppSettings'{..}
          = object
              (catMaybes
                 [("DefaultResourceSpec" .=) <$>
                    _jsasDefaultResourceSpec])

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LicenseConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LicenseConfiguration where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a license configuration.
--
--
--
-- /See:/ 'licenseConfiguration' smart constructor.
newtype LicenseConfiguration = LicenseConfiguration'{_lcLicenseConfigurationARN
                                                     :: Maybe Text}
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'LicenseConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcLicenseConfigurationARN' - The Amazon Resource Name (ARN) of the license configuration.
licenseConfiguration
    :: LicenseConfiguration
licenseConfiguration
  = LicenseConfiguration'{_lcLicenseConfigurationARN =
                            Nothing}

-- | The Amazon Resource Name (ARN) of the license configuration.
lcLicenseConfigurationARN :: Lens' LicenseConfiguration (Maybe Text)
lcLicenseConfigurationARN = lens _lcLicenseConfigurationARN (\ s a -> s{_lcLicenseConfigurationARN = a})

instance FromXML LicenseConfiguration where
        parseXML x
          = LicenseConfiguration' <$>
              (x .@? "licenseConfigurationArn")

instance Hashable LicenseConfiguration where

instance NFData LicenseConfiguration where

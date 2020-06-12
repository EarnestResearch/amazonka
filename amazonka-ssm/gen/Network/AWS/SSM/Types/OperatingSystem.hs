{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OperatingSystem
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.OperatingSystem where

import Network.AWS.Prelude
  
data OperatingSystem = AmazonLinux
                     | Centos
                     | RedhatEnterpriseLinux
                     | Suse
                     | Ubuntu
                     | Windows
                         deriving (Eq, Ord, Read, Show, Enum, Bounded, Data,
                                   Typeable, Generic)

instance FromText OperatingSystem where
    parser = takeLowerText >>= \case
        "amazon_linux" -> pure AmazonLinux
        "centos" -> pure Centos
        "redhat_enterprise_linux" -> pure RedhatEnterpriseLinux
        "suse" -> pure Suse
        "ubuntu" -> pure Ubuntu
        "windows" -> pure Windows
        e -> fromTextError $ "Failure parsing OperatingSystem from value: '" <> e
           <> "'. Accepted values: amazon_linux, centos, redhat_enterprise_linux, suse, ubuntu, windows"

instance ToText OperatingSystem where
    toText = \case
        AmazonLinux -> "AMAZON_LINUX"
        Centos -> "CENTOS"
        RedhatEnterpriseLinux -> "REDHAT_ENTERPRISE_LINUX"
        Suse -> "SUSE"
        Ubuntu -> "UBUNTU"
        Windows -> "WINDOWS"

instance Hashable     OperatingSystem
instance NFData       OperatingSystem
instance ToByteString OperatingSystem
instance ToQuery      OperatingSystem
instance ToHeader     OperatingSystem

instance ToJSON OperatingSystem where
    toJSON = toJSONText

instance FromJSON OperatingSystem where
    parseJSON = parseJSONText "OperatingSystem"

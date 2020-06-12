{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.DeviceCgroupPermission
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.DeviceCgroupPermission where

import Network.AWS.Prelude
  
data DeviceCgroupPermission = Mknod
                            | Read
                            | Write
                                deriving (Eq, Ord, Read, Show, Enum, Bounded,
                                          Data, Typeable, Generic)

instance FromText DeviceCgroupPermission where
    parser = takeLowerText >>= \case
        "mknod" -> pure Mknod
        "read" -> pure Read
        "write" -> pure Write
        e -> fromTextError $ "Failure parsing DeviceCgroupPermission from value: '" <> e
           <> "'. Accepted values: mknod, read, write"

instance ToText DeviceCgroupPermission where
    toText = \case
        Mknod -> "mknod"
        Read -> "read"
        Write -> "write"

instance Hashable     DeviceCgroupPermission
instance NFData       DeviceCgroupPermission
instance ToByteString DeviceCgroupPermission
instance ToQuery      DeviceCgroupPermission
instance ToHeader     DeviceCgroupPermission

instance ToJSON DeviceCgroupPermission where
    toJSON = toJSONText

instance FromJSON DeviceCgroupPermission where
    parseJSON = parseJSONText "DeviceCgroupPermission"

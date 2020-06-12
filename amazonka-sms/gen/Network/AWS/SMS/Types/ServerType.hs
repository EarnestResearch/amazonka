{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.ServerType where

import Network.AWS.Prelude
  
-- | Type of server.
data ServerType = VirtualMachine
                    deriving (Eq, Ord, Read, Show, Enum, Bounded, Data,
                              Typeable, Generic)

instance FromText ServerType where
    parser = takeLowerText >>= \case
        "virtual_machine" -> pure VirtualMachine
        e -> fromTextError $ "Failure parsing ServerType from value: '" <> e
           <> "'. Accepted values: virtual_machine"

instance ToText ServerType where
    toText = \case
        VirtualMachine -> "VIRTUAL_MACHINE"

instance Hashable     ServerType
instance NFData       ServerType
instance ToByteString ServerType
instance ToQuery      ServerType
instance ToHeader     ServerType

instance FromJSON ServerType where
    parseJSON = parseJSONText "ServerType"

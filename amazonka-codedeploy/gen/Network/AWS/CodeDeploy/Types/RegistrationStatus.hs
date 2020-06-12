{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.RegistrationStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.RegistrationStatus where

import Network.AWS.Prelude
  
data RegistrationStatus = Deregistered
                        | Registered
                            deriving (Eq, Ord, Read, Show, Enum, Bounded, Data,
                                      Typeable, Generic)

instance FromText RegistrationStatus where
    parser = takeLowerText >>= \case
        "deregistered" -> pure Deregistered
        "registered" -> pure Registered
        e -> fromTextError $ "Failure parsing RegistrationStatus from value: '" <> e
           <> "'. Accepted values: deregistered, registered"

instance ToText RegistrationStatus where
    toText = \case
        Deregistered -> "Deregistered"
        Registered -> "Registered"

instance Hashable     RegistrationStatus
instance NFData       RegistrationStatus
instance ToByteString RegistrationStatus
instance ToQuery      RegistrationStatus
instance ToHeader     RegistrationStatus

instance ToJSON RegistrationStatus where
    toJSON = toJSONText

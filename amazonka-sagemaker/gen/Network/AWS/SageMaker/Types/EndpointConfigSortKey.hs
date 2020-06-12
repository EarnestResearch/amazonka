{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.EndpointConfigSortKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.EndpointConfigSortKey where

import Network.AWS.Prelude
  
data EndpointConfigSortKey = CreationTime
                           | Name
                               deriving (Eq, Ord, Read, Show, Enum, Bounded,
                                         Data, Typeable, Generic)

instance FromText EndpointConfigSortKey where
    parser = takeLowerText >>= \case
        "creationtime" -> pure CreationTime
        "name" -> pure Name
        e -> fromTextError $ "Failure parsing EndpointConfigSortKey from value: '" <> e
           <> "'. Accepted values: creationtime, name"

instance ToText EndpointConfigSortKey where
    toText = \case
        CreationTime -> "CreationTime"
        Name -> "Name"

instance Hashable     EndpointConfigSortKey
instance NFData       EndpointConfigSortKey
instance ToByteString EndpointConfigSortKey
instance ToQuery      EndpointConfigSortKey
instance ToHeader     EndpointConfigSortKey

instance ToJSON EndpointConfigSortKey where
    toJSON = toJSONText

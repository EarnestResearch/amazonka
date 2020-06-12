{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.MessageFormatValue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.MessageFormatValue where

import Network.AWS.Prelude
  
data MessageFormatValue = JSON
                        | JSONUnformatted
                            deriving (Eq, Ord, Read, Show, Enum, Bounded, Data,
                                      Typeable, Generic)

instance FromText MessageFormatValue where
    parser = takeLowerText >>= \case
        "json" -> pure JSON
        "json-unformatted" -> pure JSONUnformatted
        e -> fromTextError $ "Failure parsing MessageFormatValue from value: '" <> e
           <> "'. Accepted values: json, json-unformatted"

instance ToText MessageFormatValue where
    toText = \case
        JSON -> "json"
        JSONUnformatted -> "json-unformatted"

instance Hashable     MessageFormatValue
instance NFData       MessageFormatValue
instance ToByteString MessageFormatValue
instance ToQuery      MessageFormatValue
instance ToHeader     MessageFormatValue

instance ToJSON MessageFormatValue where
    toJSON = toJSONText

instance FromJSON MessageFormatValue where
    parseJSON = parseJSONText "MessageFormatValue"

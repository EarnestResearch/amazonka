{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.FileHeaderInfo
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Types.FileHeaderInfo where

import Network.AWS.Prelude
  
data FileHeaderInfo = Ignore
                    | None
                    | Use
                        deriving (Eq, Ord, Read, Show, Enum, Bounded, Data,
                                  Typeable, Generic)

instance FromText FileHeaderInfo where
    parser = takeLowerText >>= \case
        "ignore" -> pure Ignore
        "none" -> pure None
        "use" -> pure Use
        e -> fromTextError $ "Failure parsing FileHeaderInfo from value: '" <> e
           <> "'. Accepted values: ignore, none, use"

instance ToText FileHeaderInfo where
    toText = \case
        Ignore -> "IGNORE"
        None -> "NONE"
        Use -> "USE"

instance Hashable     FileHeaderInfo
instance NFData       FileHeaderInfo
instance ToByteString FileHeaderInfo
instance ToQuery      FileHeaderInfo
instance ToHeader     FileHeaderInfo

instance ToJSON FileHeaderInfo where
    toJSON = toJSONText

instance FromJSON FileHeaderInfo where
    parseJSON = parseJSONText "FileHeaderInfo"

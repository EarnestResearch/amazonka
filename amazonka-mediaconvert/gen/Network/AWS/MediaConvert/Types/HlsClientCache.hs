{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsClientCache
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.HlsClientCache where

import Network.AWS.Prelude
  
-- | When set to ENABLED, sets #EXT-X-ALLOW-CACHE:no tag, which prevents client from saving media segments for later replay.
data HlsClientCache = HCCDisabled
                    | HCCEnabled
                        deriving (Eq, Ord, Read, Show, Enum, Bounded, Data,
                                  Typeable, Generic)

instance FromText HlsClientCache where
    parser = takeLowerText >>= \case
        "disabled" -> pure HCCDisabled
        "enabled" -> pure HCCEnabled
        e -> fromTextError $ "Failure parsing HlsClientCache from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText HlsClientCache where
    toText = \case
        HCCDisabled -> "DISABLED"
        HCCEnabled -> "ENABLED"

instance Hashable     HlsClientCache
instance NFData       HlsClientCache
instance ToByteString HlsClientCache
instance ToQuery      HlsClientCache
instance ToHeader     HlsClientCache

instance ToJSON HlsClientCache where
    toJSON = toJSONText

instance FromJSON HlsClientCache where
    parseJSON = parseJSONText "HlsClientCache"

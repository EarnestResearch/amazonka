{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3SurroundExMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Eac3SurroundExMode where

import Network.AWS.Prelude
  
-- | Placeholder documentation for Eac3SurroundExMode
data Eac3SurroundExMode = ESEMDisabled
                        | ESEMEnabled
                        | ESEMNotIndicated
                            deriving (Eq, Ord, Read, Show, Enum, Bounded, Data,
                                      Typeable, Generic)

instance FromText Eac3SurroundExMode where
    parser = takeLowerText >>= \case
        "disabled" -> pure ESEMDisabled
        "enabled" -> pure ESEMEnabled
        "not_indicated" -> pure ESEMNotIndicated
        e -> fromTextError $ "Failure parsing Eac3SurroundExMode from value: '" <> e
           <> "'. Accepted values: disabled, enabled, not_indicated"

instance ToText Eac3SurroundExMode where
    toText = \case
        ESEMDisabled -> "DISABLED"
        ESEMEnabled -> "ENABLED"
        ESEMNotIndicated -> "NOT_INDICATED"

instance Hashable     Eac3SurroundExMode
instance NFData       Eac3SurroundExMode
instance ToByteString Eac3SurroundExMode
instance ToQuery      Eac3SurroundExMode
instance ToHeader     Eac3SurroundExMode

instance ToJSON Eac3SurroundExMode where
    toJSON = toJSONText

instance FromJSON Eac3SurroundExMode where
    parseJSON = parseJSONText "Eac3SurroundExMode"

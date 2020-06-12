{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MovReference
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.MovReference where

import Network.AWS.Prelude
  
-- | A value of 'external' creates separate media files and the wrapper file (.mov) contains references to these media files. A value of 'self_contained' creates only a wrapper (.mov) file and this file contains all of the media.
data MovReference = External
                  | SelfContained
                      deriving (Eq, Ord, Read, Show, Enum, Bounded, Data,
                                Typeable, Generic)

instance FromText MovReference where
    parser = takeLowerText >>= \case
        "external" -> pure External
        "self_contained" -> pure SelfContained
        e -> fromTextError $ "Failure parsing MovReference from value: '" <> e
           <> "'. Accepted values: external, self_contained"

instance ToText MovReference where
    toText = \case
        External -> "EXTERNAL"
        SelfContained -> "SELF_CONTAINED"

instance Hashable     MovReference
instance NFData       MovReference
instance ToByteString MovReference
instance ToQuery      MovReference
instance ToHeader     MovReference

instance ToJSON MovReference where
    toJSON = toJSONText

instance FromJSON MovReference where
    parseJSON = parseJSONText "MovReference"

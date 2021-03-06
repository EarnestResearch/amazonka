{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.Types.ContainerLevelMetrics
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaStore.Types.ContainerLevelMetrics (
  ContainerLevelMetrics (
    ..
    , Disabled
    , Enabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ContainerLevelMetrics = ContainerLevelMetrics' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern Disabled :: ContainerLevelMetrics
pattern Disabled = ContainerLevelMetrics' "DISABLED"

pattern Enabled :: ContainerLevelMetrics
pattern Enabled = ContainerLevelMetrics' "ENABLED"

{-# COMPLETE
  Disabled,
  Enabled,
  ContainerLevelMetrics' #-}

instance FromText ContainerLevelMetrics where
    parser = (ContainerLevelMetrics' . mk) <$> takeText

instance ToText ContainerLevelMetrics where
    toText (ContainerLevelMetrics' ci) = original ci

-- | Represents an enum of /known/ $ContainerLevelMetrics.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ContainerLevelMetrics where
    toEnum i = case i of
        0 -> Disabled
        1 -> Enabled
        _ -> (error . showText) $ "Unknown index for ContainerLevelMetrics: " <> toText i
    fromEnum x = case x of
        Disabled -> 0
        Enabled -> 1
        ContainerLevelMetrics' name -> (error . showText) $ "Unknown ContainerLevelMetrics: " <> original name

-- | Represents the bounds of /known/ $ContainerLevelMetrics.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ContainerLevelMetrics where
    minBound = Disabled
    maxBound = Enabled

instance Hashable     ContainerLevelMetrics
instance NFData       ContainerLevelMetrics
instance ToByteString ContainerLevelMetrics
instance ToQuery      ContainerLevelMetrics
instance ToHeader     ContainerLevelMetrics

instance ToJSON ContainerLevelMetrics where
    toJSON = toJSONText

instance FromJSON ContainerLevelMetrics where
    parseJSON = parseJSONText "ContainerLevelMetrics"

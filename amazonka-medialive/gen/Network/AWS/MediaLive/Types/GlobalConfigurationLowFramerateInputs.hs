{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.GlobalConfigurationLowFramerateInputs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.GlobalConfigurationLowFramerateInputs (
  GlobalConfigurationLowFramerateInputs (
    ..
    , GCLFIDisabled
    , GCLFIEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Global Configuration Low Framerate Inputs
data GlobalConfigurationLowFramerateInputs = GlobalConfigurationLowFramerateInputs' (CI
                                                                                       Text)
                                               deriving (Eq, Ord, Read, Show,
                                                         Data, Typeable,
                                                         Generic)

pattern GCLFIDisabled :: GlobalConfigurationLowFramerateInputs
pattern GCLFIDisabled = GlobalConfigurationLowFramerateInputs' "DISABLED"

pattern GCLFIEnabled :: GlobalConfigurationLowFramerateInputs
pattern GCLFIEnabled = GlobalConfigurationLowFramerateInputs' "ENABLED"

{-# COMPLETE
  GCLFIDisabled,
  GCLFIEnabled,
  GlobalConfigurationLowFramerateInputs' #-}

instance FromText GlobalConfigurationLowFramerateInputs where
    parser = (GlobalConfigurationLowFramerateInputs' . mk) <$> takeText

instance ToText GlobalConfigurationLowFramerateInputs where
    toText (GlobalConfigurationLowFramerateInputs' ci) = original ci

-- | Represents an enum of /known/ $GlobalConfigurationLowFramerateInputs.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum GlobalConfigurationLowFramerateInputs where
    toEnum i = case i of
        0 -> GCLFIDisabled
        1 -> GCLFIEnabled
        _ -> (error . showText) $ "Unknown index for GlobalConfigurationLowFramerateInputs: " <> toText i
    fromEnum x = case x of
        GCLFIDisabled -> 0
        GCLFIEnabled -> 1
        GlobalConfigurationLowFramerateInputs' name -> (error . showText) $ "Unknown GlobalConfigurationLowFramerateInputs: " <> original name

-- | Represents the bounds of /known/ $GlobalConfigurationLowFramerateInputs.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded GlobalConfigurationLowFramerateInputs where
    minBound = GCLFIDisabled
    maxBound = GCLFIEnabled

instance Hashable     GlobalConfigurationLowFramerateInputs
instance NFData       GlobalConfigurationLowFramerateInputs
instance ToByteString GlobalConfigurationLowFramerateInputs
instance ToQuery      GlobalConfigurationLowFramerateInputs
instance ToHeader     GlobalConfigurationLowFramerateInputs

instance ToJSON GlobalConfigurationLowFramerateInputs where
    toJSON = toJSONText

instance FromJSON GlobalConfigurationLowFramerateInputs where
    parseJSON = parseJSONText "GlobalConfigurationLowFramerateInputs"

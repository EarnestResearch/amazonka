{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.NielsenPcmToId3TaggingState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.NielsenPcmToId3TaggingState (
  NielsenPcmToId3TaggingState (
    ..
    , NPTITSDisabled
    , NPTITSEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | State of Nielsen PCM to ID3 tagging
data NielsenPcmToId3TaggingState = NielsenPcmToId3TaggingState' (CI
                                                                   Text)
                                     deriving (Eq, Ord, Read, Show, Data,
                                               Typeable, Generic)

pattern NPTITSDisabled :: NielsenPcmToId3TaggingState
pattern NPTITSDisabled = NielsenPcmToId3TaggingState' "DISABLED"

pattern NPTITSEnabled :: NielsenPcmToId3TaggingState
pattern NPTITSEnabled = NielsenPcmToId3TaggingState' "ENABLED"

{-# COMPLETE
  NPTITSDisabled,
  NPTITSEnabled,
  NielsenPcmToId3TaggingState' #-}

instance FromText NielsenPcmToId3TaggingState where
    parser = (NielsenPcmToId3TaggingState' . mk) <$> takeText

instance ToText NielsenPcmToId3TaggingState where
    toText (NielsenPcmToId3TaggingState' ci) = original ci

-- | Represents an enum of /known/ $NielsenPcmToId3TaggingState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum NielsenPcmToId3TaggingState where
    toEnum i = case i of
        0 -> NPTITSDisabled
        1 -> NPTITSEnabled
        _ -> (error . showText) $ "Unknown index for NielsenPcmToId3TaggingState: " <> toText i
    fromEnum x = case x of
        NPTITSDisabled -> 0
        NPTITSEnabled -> 1
        NielsenPcmToId3TaggingState' name -> (error . showText) $ "Unknown NielsenPcmToId3TaggingState: " <> original name

-- | Represents the bounds of /known/ $NielsenPcmToId3TaggingState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded NielsenPcmToId3TaggingState where
    minBound = NPTITSDisabled
    maxBound = NPTITSEnabled

instance Hashable     NielsenPcmToId3TaggingState
instance NFData       NielsenPcmToId3TaggingState
instance ToByteString NielsenPcmToId3TaggingState
instance ToQuery      NielsenPcmToId3TaggingState
instance ToHeader     NielsenPcmToId3TaggingState

instance ToJSON NielsenPcmToId3TaggingState where
    toJSON = toJSONText

instance FromJSON NielsenPcmToId3TaggingState where
    parseJSON = parseJSONText "NielsenPcmToId3TaggingState"

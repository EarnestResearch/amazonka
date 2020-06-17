{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35AposWebDeliveryAllowedBehavior
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Scte35AposWebDeliveryAllowedBehavior (
  Scte35AposWebDeliveryAllowedBehavior (
    ..
    , SAWDABFollow
    , SAWDABIgnore
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Placeholder documentation for Scte35AposWebDeliveryAllowedBehavior
data Scte35AposWebDeliveryAllowedBehavior = Scte35AposWebDeliveryAllowedBehavior' (CI
                                                                                     Text)
                                              deriving (Eq, Ord, Read, Show,
                                                        Data, Typeable, Generic)

pattern SAWDABFollow :: Scte35AposWebDeliveryAllowedBehavior
pattern SAWDABFollow = Scte35AposWebDeliveryAllowedBehavior' "FOLLOW"

pattern SAWDABIgnore :: Scte35AposWebDeliveryAllowedBehavior
pattern SAWDABIgnore = Scte35AposWebDeliveryAllowedBehavior' "IGNORE"

{-# COMPLETE
  SAWDABFollow,
  SAWDABIgnore,
  Scte35AposWebDeliveryAllowedBehavior' #-}

instance FromText Scte35AposWebDeliveryAllowedBehavior where
    parser = (Scte35AposWebDeliveryAllowedBehavior' . mk) <$> takeText

instance ToText Scte35AposWebDeliveryAllowedBehavior where
    toText (Scte35AposWebDeliveryAllowedBehavior' ci) = original ci

-- | Represents an enum of /known/ $Scte35AposWebDeliveryAllowedBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Scte35AposWebDeliveryAllowedBehavior where
    toEnum i = case i of
        0 -> SAWDABFollow
        1 -> SAWDABIgnore
        _ -> (error . showText) $ "Unknown index for Scte35AposWebDeliveryAllowedBehavior: " <> toText i
    fromEnum x = case x of
        SAWDABFollow -> 0
        SAWDABIgnore -> 1
        Scte35AposWebDeliveryAllowedBehavior' name -> (error . showText) $ "Unknown Scte35AposWebDeliveryAllowedBehavior: " <> original name

-- | Represents the bounds of /known/ $Scte35AposWebDeliveryAllowedBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Scte35AposWebDeliveryAllowedBehavior where
    minBound = SAWDABFollow
    maxBound = SAWDABIgnore

instance Hashable     Scte35AposWebDeliveryAllowedBehavior
instance NFData       Scte35AposWebDeliveryAllowedBehavior
instance ToByteString Scte35AposWebDeliveryAllowedBehavior
instance ToQuery      Scte35AposWebDeliveryAllowedBehavior
instance ToHeader     Scte35AposWebDeliveryAllowedBehavior

instance ToJSON Scte35AposWebDeliveryAllowedBehavior where
    toJSON = toJSONText

instance FromJSON Scte35AposWebDeliveryAllowedBehavior where
    parseJSON = parseJSONText "Scte35AposWebDeliveryAllowedBehavior"

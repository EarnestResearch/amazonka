{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35WebDeliveryAllowedFlag
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Scte35WebDeliveryAllowedFlag (
  Scte35WebDeliveryAllowedFlag (
    ..
    , WebDeliveryAllowed
    , WebDeliveryNotAllowed
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Corresponds to the web_delivery_allowed_flag parameter. A value of WEB_DELIVERY_NOT_ALLOWED corresponds to 0 (false) in the SCTE-35 specification. If you include one of the "restriction" flags then you must include all four of them.
data Scte35WebDeliveryAllowedFlag = Scte35WebDeliveryAllowedFlag' (CI
                                                                     Text)
                                      deriving (Eq, Ord, Read, Show, Data,
                                                Typeable, Generic)

pattern WebDeliveryAllowed :: Scte35WebDeliveryAllowedFlag
pattern WebDeliveryAllowed = Scte35WebDeliveryAllowedFlag' "WEB_DELIVERY_ALLOWED"

pattern WebDeliveryNotAllowed :: Scte35WebDeliveryAllowedFlag
pattern WebDeliveryNotAllowed = Scte35WebDeliveryAllowedFlag' "WEB_DELIVERY_NOT_ALLOWED"

{-# COMPLETE
  WebDeliveryAllowed,
  WebDeliveryNotAllowed,
  Scte35WebDeliveryAllowedFlag' #-}

instance FromText Scte35WebDeliveryAllowedFlag where
    parser = (Scte35WebDeliveryAllowedFlag' . mk) <$> takeText

instance ToText Scte35WebDeliveryAllowedFlag where
    toText (Scte35WebDeliveryAllowedFlag' ci) = original ci

-- | Represents an enum of /known/ $Scte35WebDeliveryAllowedFlag.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Scte35WebDeliveryAllowedFlag where
    toEnum i = case i of
        0 -> WebDeliveryAllowed
        1 -> WebDeliveryNotAllowed
        _ -> (error . showText) $ "Unknown index for Scte35WebDeliveryAllowedFlag: " <> toText i
    fromEnum x = case x of
        WebDeliveryAllowed -> 0
        WebDeliveryNotAllowed -> 1
        Scte35WebDeliveryAllowedFlag' name -> (error . showText) $ "Unknown Scte35WebDeliveryAllowedFlag: " <> original name

-- | Represents the bounds of /known/ $Scte35WebDeliveryAllowedFlag.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Scte35WebDeliveryAllowedFlag where
    minBound = WebDeliveryAllowed
    maxBound = WebDeliveryNotAllowed

instance Hashable     Scte35WebDeliveryAllowedFlag
instance NFData       Scte35WebDeliveryAllowedFlag
instance ToByteString Scte35WebDeliveryAllowedFlag
instance ToQuery      Scte35WebDeliveryAllowedFlag
instance ToHeader     Scte35WebDeliveryAllowedFlag

instance ToJSON Scte35WebDeliveryAllowedFlag where
    toJSON = toJSONText

instance FromJSON Scte35WebDeliveryAllowedFlag where
    parseJSON = parseJSONText "Scte35WebDeliveryAllowedFlag"

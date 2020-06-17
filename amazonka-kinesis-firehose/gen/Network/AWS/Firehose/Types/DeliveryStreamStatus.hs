{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.DeliveryStreamStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.DeliveryStreamStatus (
  DeliveryStreamStatus (
    ..
    , Active
    , Creating
    , Deleting
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DeliveryStreamStatus = DeliveryStreamStatus' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern Active :: DeliveryStreamStatus
pattern Active = DeliveryStreamStatus' "ACTIVE"

pattern Creating :: DeliveryStreamStatus
pattern Creating = DeliveryStreamStatus' "CREATING"

pattern Deleting :: DeliveryStreamStatus
pattern Deleting = DeliveryStreamStatus' "DELETING"

{-# COMPLETE
  Active,
  Creating,
  Deleting,
  DeliveryStreamStatus' #-}

instance FromText DeliveryStreamStatus where
    parser = (DeliveryStreamStatus' . mk) <$> takeText

instance ToText DeliveryStreamStatus where
    toText (DeliveryStreamStatus' ci) = original ci

-- | Represents an enum of /known/ $DeliveryStreamStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DeliveryStreamStatus where
    toEnum i = case i of
        0 -> Active
        1 -> Creating
        2 -> Deleting
        _ -> (error . showText) $ "Unknown index for DeliveryStreamStatus: " <> toText i
    fromEnum x = case x of
        Active -> 0
        Creating -> 1
        Deleting -> 2
        DeliveryStreamStatus' name -> (error . showText) $ "Unknown DeliveryStreamStatus: " <> original name

-- | Represents the bounds of /known/ $DeliveryStreamStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DeliveryStreamStatus where
    minBound = Active
    maxBound = Deleting

instance Hashable     DeliveryStreamStatus
instance NFData       DeliveryStreamStatus
instance ToByteString DeliveryStreamStatus
instance ToQuery      DeliveryStreamStatus
instance ToHeader     DeliveryStreamStatus

instance FromJSON DeliveryStreamStatus where
    parseJSON = parseJSONText "DeliveryStreamStatus"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingConnectivityIndexingMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.ThingConnectivityIndexingMode (
  ThingConnectivityIndexingMode (
    ..
    , TCIMOff
    , TCIMStatus
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ThingConnectivityIndexingMode = ThingConnectivityIndexingMode' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern TCIMOff :: ThingConnectivityIndexingMode
pattern TCIMOff = ThingConnectivityIndexingMode' "OFF"

pattern TCIMStatus :: ThingConnectivityIndexingMode
pattern TCIMStatus = ThingConnectivityIndexingMode' "STATUS"

{-# COMPLETE
  TCIMOff,
  TCIMStatus,
  ThingConnectivityIndexingMode' #-}

instance FromText ThingConnectivityIndexingMode where
    parser = (ThingConnectivityIndexingMode' . mk) <$> takeText

instance ToText ThingConnectivityIndexingMode where
    toText (ThingConnectivityIndexingMode' ci) = original ci

-- | Represents an enum of /known/ $ThingConnectivityIndexingMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ThingConnectivityIndexingMode where
    toEnum i = case i of
        0 -> TCIMOff
        1 -> TCIMStatus
        _ -> (error . showText) $ "Unknown index for ThingConnectivityIndexingMode: " <> toText i
    fromEnum x = case x of
        TCIMOff -> 0
        TCIMStatus -> 1
        ThingConnectivityIndexingMode' name -> (error . showText) $ "Unknown ThingConnectivityIndexingMode: " <> original name

-- | Represents the bounds of /known/ $ThingConnectivityIndexingMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ThingConnectivityIndexingMode where
    minBound = TCIMOff
    maxBound = TCIMStatus

instance Hashable     ThingConnectivityIndexingMode
instance NFData       ThingConnectivityIndexingMode
instance ToByteString ThingConnectivityIndexingMode
instance ToQuery      ThingConnectivityIndexingMode
instance ToHeader     ThingConnectivityIndexingMode

instance ToJSON ThingConnectivityIndexingMode where
    toJSON = toJSONText

instance FromJSON ThingConnectivityIndexingMode where
    parseJSON = parseJSONText "ThingConnectivityIndexingMode"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.ConfigurationSetAttribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.ConfigurationSetAttribute (
  ConfigurationSetAttribute (
    ..
    , EventDestinations
    , ReputationOptions
    , TrackingOptions
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ConfigurationSetAttribute = ConfigurationSetAttribute' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern EventDestinations :: ConfigurationSetAttribute
pattern EventDestinations = ConfigurationSetAttribute' "eventDestinations"

pattern ReputationOptions :: ConfigurationSetAttribute
pattern ReputationOptions = ConfigurationSetAttribute' "reputationOptions"

pattern TrackingOptions :: ConfigurationSetAttribute
pattern TrackingOptions = ConfigurationSetAttribute' "trackingOptions"

{-# COMPLETE
  EventDestinations,
  ReputationOptions,
  TrackingOptions,
  ConfigurationSetAttribute' #-}

instance FromText ConfigurationSetAttribute where
    parser = (ConfigurationSetAttribute' . mk) <$> takeText

instance ToText ConfigurationSetAttribute where
    toText (ConfigurationSetAttribute' ci) = original ci

-- | Represents an enum of /known/ $ConfigurationSetAttribute.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ConfigurationSetAttribute where
    toEnum i = case i of
        0 -> EventDestinations
        1 -> ReputationOptions
        2 -> TrackingOptions
        _ -> (error . showText) $ "Unknown index for ConfigurationSetAttribute: " <> toText i
    fromEnum x = case x of
        EventDestinations -> 0
        ReputationOptions -> 1
        TrackingOptions -> 2
        ConfigurationSetAttribute' name -> (error . showText) $ "Unknown ConfigurationSetAttribute: " <> original name

-- | Represents the bounds of /known/ $ConfigurationSetAttribute.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ConfigurationSetAttribute where
    minBound = EventDestinations
    maxBound = TrackingOptions

instance Hashable     ConfigurationSetAttribute
instance NFData       ConfigurationSetAttribute
instance ToByteString ConfigurationSetAttribute
instance ToQuery      ConfigurationSetAttribute
instance ToHeader     ConfigurationSetAttribute

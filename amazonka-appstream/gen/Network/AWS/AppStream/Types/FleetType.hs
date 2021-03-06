{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.FleetType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.FleetType (
  FleetType (
    ..
    , AlwaysOn
    , OnDemand
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data FleetType = FleetType' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern AlwaysOn :: FleetType
pattern AlwaysOn = FleetType' "ALWAYS_ON"

pattern OnDemand :: FleetType
pattern OnDemand = FleetType' "ON_DEMAND"

{-# COMPLETE
  AlwaysOn,
  OnDemand,
  FleetType' #-}

instance FromText FleetType where
    parser = (FleetType' . mk) <$> takeText

instance ToText FleetType where
    toText (FleetType' ci) = original ci

-- | Represents an enum of /known/ $FleetType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FleetType where
    toEnum i = case i of
        0 -> AlwaysOn
        1 -> OnDemand
        _ -> (error . showText) $ "Unknown index for FleetType: " <> toText i
    fromEnum x = case x of
        AlwaysOn -> 0
        OnDemand -> 1
        FleetType' name -> (error . showText) $ "Unknown FleetType: " <> original name

-- | Represents the bounds of /known/ $FleetType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FleetType where
    minBound = AlwaysOn
    maxBound = OnDemand

instance Hashable     FleetType
instance NFData       FleetType
instance ToByteString FleetType
instance ToQuery      FleetType
instance ToHeader     FleetType

instance ToJSON FleetType where
    toJSON = toJSONText

instance FromJSON FleetType where
    parseJSON = parseJSONText "FleetType"

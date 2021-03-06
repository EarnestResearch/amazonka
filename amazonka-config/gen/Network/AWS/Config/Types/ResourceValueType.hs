{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ResourceValueType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.ResourceValueType (
  ResourceValueType (
    ..
    , ResourceId
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ResourceValueType = ResourceValueType' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern ResourceId :: ResourceValueType
pattern ResourceId = ResourceValueType' "RESOURCE_ID"

{-# COMPLETE
  ResourceId,
  ResourceValueType' #-}

instance FromText ResourceValueType where
    parser = (ResourceValueType' . mk) <$> takeText

instance ToText ResourceValueType where
    toText (ResourceValueType' ci) = original ci

-- | Represents an enum of /known/ $ResourceValueType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ResourceValueType where
    toEnum i = case i of
        0 -> ResourceId
        _ -> (error . showText) $ "Unknown index for ResourceValueType: " <> toText i
    fromEnum x = case x of
        ResourceId -> 0
        ResourceValueType' name -> (error . showText) $ "Unknown ResourceValueType: " <> original name

-- | Represents the bounds of /known/ $ResourceValueType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ResourceValueType where
    minBound = ResourceId
    maxBound = ResourceId

instance Hashable     ResourceValueType
instance NFData       ResourceValueType
instance ToByteString ResourceValueType
instance ToQuery      ResourceValueType
instance ToHeader     ResourceValueType

instance ToJSON ResourceValueType where
    toJSON = toJSONText

instance FromJSON ResourceValueType where
    parseJSON = parseJSONText "ResourceValueType"

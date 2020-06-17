{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ClusterField
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.ClusterField (
  ClusterField (
    ..
    , Statistics
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ClusterField = ClusterField' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern Statistics :: ClusterField
pattern Statistics = ClusterField' "STATISTICS"

{-# COMPLETE
  Statistics,
  ClusterField' #-}

instance FromText ClusterField where
    parser = (ClusterField' . mk) <$> takeText

instance ToText ClusterField where
    toText (ClusterField' ci) = original ci

-- | Represents an enum of /known/ $ClusterField.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ClusterField where
    toEnum i = case i of
        0 -> Statistics
        _ -> (error . showText) $ "Unknown index for ClusterField: " <> toText i
    fromEnum x = case x of
        Statistics -> 0
        ClusterField' name -> (error . showText) $ "Unknown ClusterField: " <> original name

-- | Represents the bounds of /known/ $ClusterField.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ClusterField where
    minBound = Statistics
    maxBound = Statistics

instance Hashable     ClusterField
instance NFData       ClusterField
instance ToByteString ClusterField
instance ToQuery      ClusterField
instance ToHeader     ClusterField

instance ToJSON ClusterField where
    toJSON = toJSONText

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.LoadMetricType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScalingPlans.Types.LoadMetricType (
  LoadMetricType (
    ..
    , ALBTargetGroupRequestCount
    , ASGTotalCPUUtilization
    , ASGTotalNetworkIn
    , ASGTotalNetworkOut
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LoadMetricType = LoadMetricType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern ALBTargetGroupRequestCount :: LoadMetricType
pattern ALBTargetGroupRequestCount = LoadMetricType' "ALBTargetGroupRequestCount"

pattern ASGTotalCPUUtilization :: LoadMetricType
pattern ASGTotalCPUUtilization = LoadMetricType' "ASGTotalCPUUtilization"

pattern ASGTotalNetworkIn :: LoadMetricType
pattern ASGTotalNetworkIn = LoadMetricType' "ASGTotalNetworkIn"

pattern ASGTotalNetworkOut :: LoadMetricType
pattern ASGTotalNetworkOut = LoadMetricType' "ASGTotalNetworkOut"

{-# COMPLETE
  ALBTargetGroupRequestCount,
  ASGTotalCPUUtilization,
  ASGTotalNetworkIn,
  ASGTotalNetworkOut,
  LoadMetricType' #-}

instance FromText LoadMetricType where
    parser = (LoadMetricType' . mk) <$> takeText

instance ToText LoadMetricType where
    toText (LoadMetricType' ci) = original ci

-- | Represents an enum of /known/ $LoadMetricType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LoadMetricType where
    toEnum i = case i of
        0 -> ALBTargetGroupRequestCount
        1 -> ASGTotalCPUUtilization
        2 -> ASGTotalNetworkIn
        3 -> ASGTotalNetworkOut
        _ -> (error . showText) $ "Unknown index for LoadMetricType: " <> toText i
    fromEnum x = case x of
        ALBTargetGroupRequestCount -> 0
        ASGTotalCPUUtilization -> 1
        ASGTotalNetworkIn -> 2
        ASGTotalNetworkOut -> 3
        LoadMetricType' name -> (error . showText) $ "Unknown LoadMetricType: " <> original name

-- | Represents the bounds of /known/ $LoadMetricType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LoadMetricType where
    minBound = ALBTargetGroupRequestCount
    maxBound = ASGTotalNetworkOut

instance Hashable     LoadMetricType
instance NFData       LoadMetricType
instance ToByteString LoadMetricType
instance ToQuery      LoadMetricType
instance ToHeader     LoadMetricType

instance ToJSON LoadMetricType where
    toJSON = toJSONText

instance FromJSON LoadMetricType where
    parseJSON = parseJSONText "LoadMetricType"

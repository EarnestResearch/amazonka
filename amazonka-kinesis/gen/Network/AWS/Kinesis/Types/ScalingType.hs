{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.ScalingType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.ScalingType where

import Network.AWS.Prelude
  
data ScalingType = UniformScaling
                     deriving (Eq, Ord, Read, Show, Enum, Bounded, Data,
                               Typeable, Generic)

instance FromText ScalingType where
    parser = takeLowerText >>= \case
        "uniform_scaling" -> pure UniformScaling
        e -> fromTextError $ "Failure parsing ScalingType from value: '" <> e
           <> "'. Accepted values: uniform_scaling"

instance ToText ScalingType where
    toText = \case
        UniformScaling -> "UNIFORM_SCALING"

instance Hashable     ScalingType
instance NFData       ScalingType
instance ToByteString ScalingType
instance ToQuery      ScalingType
instance ToHeader     ScalingType

instance ToJSON ScalingType where
    toJSON = toJSONText

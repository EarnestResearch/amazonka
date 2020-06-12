{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ComputeType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.ComputeType where

import Network.AWS.Prelude
  
data ComputeType = BuildGENERAL12XLARGE
                 | BuildGENERAL1Large
                 | BuildGENERAL1Medium
                 | BuildGENERAL1Small
                     deriving (Eq, Ord, Read, Show, Enum, Bounded, Data,
                               Typeable, Generic)

instance FromText ComputeType where
    parser = takeLowerText >>= \case
        "build_general1_2xlarge" -> pure BuildGENERAL12XLARGE
        "build_general1_large" -> pure BuildGENERAL1Large
        "build_general1_medium" -> pure BuildGENERAL1Medium
        "build_general1_small" -> pure BuildGENERAL1Small
        e -> fromTextError $ "Failure parsing ComputeType from value: '" <> e
           <> "'. Accepted values: build_general1_2xlarge, build_general1_large, build_general1_medium, build_general1_small"

instance ToText ComputeType where
    toText = \case
        BuildGENERAL12XLARGE -> "BUILD_GENERAL1_2XLARGE"
        BuildGENERAL1Large -> "BUILD_GENERAL1_LARGE"
        BuildGENERAL1Medium -> "BUILD_GENERAL1_MEDIUM"
        BuildGENERAL1Small -> "BUILD_GENERAL1_SMALL"

instance Hashable     ComputeType
instance NFData       ComputeType
instance ToByteString ComputeType
instance ToQuery      ComputeType
instance ToHeader     ComputeType

instance ToJSON ComputeType where
    toJSON = toJSONText

instance FromJSON ComputeType where
    parseJSON = parseJSONText "ComputeType"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.ReportType where

import Network.AWS.Prelude
  
data ReportType = Test
                    deriving (Eq, Ord, Read, Show, Enum, Bounded, Data,
                              Typeable, Generic)

instance FromText ReportType where
    parser = takeLowerText >>= \case
        "test" -> pure Test
        e -> fromTextError $ "Failure parsing ReportType from value: '" <> e
           <> "'. Accepted values: test"

instance ToText ReportType where
    toText = \case
        Test -> "TEST"

instance Hashable     ReportType
instance NFData       ReportType
instance ToByteString ReportType
instance ToQuery      ReportType
instance ToHeader     ReportType

instance ToJSON ReportType where
    toJSON = toJSONText

instance FromJSON ReportType where
    parseJSON = parseJSONText "ReportType"

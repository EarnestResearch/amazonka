{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Types.ReportVersioning
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostAndUsageReport.Types.ReportVersioning (
  ReportVersioning (
    ..
    , CreateNewReport
    , OverwriteReport
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ReportVersioning = ReportVersioning' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern CreateNewReport :: ReportVersioning
pattern CreateNewReport = ReportVersioning' "CREATE_NEW_REPORT"

pattern OverwriteReport :: ReportVersioning
pattern OverwriteReport = ReportVersioning' "OVERWRITE_REPORT"

{-# COMPLETE
  CreateNewReport,
  OverwriteReport,
  ReportVersioning' #-}

instance FromText ReportVersioning where
    parser = (ReportVersioning' . mk) <$> takeText

instance ToText ReportVersioning where
    toText (ReportVersioning' ci) = original ci

-- | Represents an enum of /known/ $ReportVersioning.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReportVersioning where
    toEnum i = case i of
        0 -> CreateNewReport
        1 -> OverwriteReport
        _ -> (error . showText) $ "Unknown index for ReportVersioning: " <> toText i
    fromEnum x = case x of
        CreateNewReport -> 0
        OverwriteReport -> 1
        ReportVersioning' name -> (error . showText) $ "Unknown ReportVersioning: " <> original name

-- | Represents the bounds of /known/ $ReportVersioning.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReportVersioning where
    minBound = CreateNewReport
    maxBound = OverwriteReport

instance Hashable     ReportVersioning
instance NFData       ReportVersioning
instance ToByteString ReportVersioning
instance ToQuery      ReportVersioning
instance ToHeader     ReportVersioning

instance ToJSON ReportVersioning where
    toJSON = toJSONText

instance FromJSON ReportVersioning where
    parseJSON = parseJSONText "ReportVersioning"

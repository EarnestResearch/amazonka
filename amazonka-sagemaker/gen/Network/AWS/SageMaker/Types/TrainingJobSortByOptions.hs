{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrainingJobSortByOptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.TrainingJobSortByOptions (
  TrainingJobSortByOptions (
    ..
    , TJSBOCreationTime
    , TJSBOFinalObjectiveMetricValue
    , TJSBOName
    , TJSBOStatus
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TrainingJobSortByOptions = TrainingJobSortByOptions' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern TJSBOCreationTime :: TrainingJobSortByOptions
pattern TJSBOCreationTime = TrainingJobSortByOptions' "CreationTime"

pattern TJSBOFinalObjectiveMetricValue :: TrainingJobSortByOptions
pattern TJSBOFinalObjectiveMetricValue = TrainingJobSortByOptions' "FinalObjectiveMetricValue"

pattern TJSBOName :: TrainingJobSortByOptions
pattern TJSBOName = TrainingJobSortByOptions' "Name"

pattern TJSBOStatus :: TrainingJobSortByOptions
pattern TJSBOStatus = TrainingJobSortByOptions' "Status"

{-# COMPLETE
  TJSBOCreationTime,
  TJSBOFinalObjectiveMetricValue,
  TJSBOName,
  TJSBOStatus,
  TrainingJobSortByOptions' #-}

instance FromText TrainingJobSortByOptions where
    parser = (TrainingJobSortByOptions' . mk) <$> takeText

instance ToText TrainingJobSortByOptions where
    toText (TrainingJobSortByOptions' ci) = original ci

-- | Represents an enum of /known/ $TrainingJobSortByOptions.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TrainingJobSortByOptions where
    toEnum i = case i of
        0 -> TJSBOCreationTime
        1 -> TJSBOFinalObjectiveMetricValue
        2 -> TJSBOName
        3 -> TJSBOStatus
        _ -> (error . showText) $ "Unknown index for TrainingJobSortByOptions: " <> toText i
    fromEnum x = case x of
        TJSBOCreationTime -> 0
        TJSBOFinalObjectiveMetricValue -> 1
        TJSBOName -> 2
        TJSBOStatus -> 3
        TrainingJobSortByOptions' name -> (error . showText) $ "Unknown TrainingJobSortByOptions: " <> original name

-- | Represents the bounds of /known/ $TrainingJobSortByOptions.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TrainingJobSortByOptions where
    minBound = TJSBOCreationTime
    maxBound = TJSBOStatus

instance Hashable     TrainingJobSortByOptions
instance NFData       TrainingJobSortByOptions
instance ToByteString TrainingJobSortByOptions
instance ToQuery      TrainingJobSortByOptions
instance ToHeader     TrainingJobSortByOptions

instance ToJSON TrainingJobSortByOptions where
    toJSON = toJSONText

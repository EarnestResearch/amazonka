{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.QualityFilter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.QualityFilter (
  QualityFilter (
    ..
    , Auto
    , High
    , Low
    , Medium
    , None
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data QualityFilter = QualityFilter' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Auto :: QualityFilter
pattern Auto = QualityFilter' "AUTO"

pattern High :: QualityFilter
pattern High = QualityFilter' "HIGH"

pattern Low :: QualityFilter
pattern Low = QualityFilter' "LOW"

pattern Medium :: QualityFilter
pattern Medium = QualityFilter' "MEDIUM"

pattern None :: QualityFilter
pattern None = QualityFilter' "NONE"

{-# COMPLETE
  Auto,
  High,
  Low,
  Medium,
  None,
  QualityFilter' #-}

instance FromText QualityFilter where
    parser = (QualityFilter' . mk) <$> takeText

instance ToText QualityFilter where
    toText (QualityFilter' ci) = original ci

-- | Represents an enum of /known/ $QualityFilter.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum QualityFilter where
    toEnum i = case i of
        0 -> Auto
        1 -> High
        2 -> Low
        3 -> Medium
        4 -> None
        _ -> (error . showText) $ "Unknown index for QualityFilter: " <> toText i
    fromEnum x = case x of
        Auto -> 0
        High -> 1
        Low -> 2
        Medium -> 3
        None -> 4
        QualityFilter' name -> (error . showText) $ "Unknown QualityFilter: " <> original name

-- | Represents the bounds of /known/ $QualityFilter.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded QualityFilter where
    minBound = Auto
    maxBound = None

instance Hashable     QualityFilter
instance NFData       QualityFilter
instance ToByteString QualityFilter
instance ToQuery      QualityFilter
instance ToHeader     QualityFilter

instance ToJSON QualityFilter where
    toJSON = toJSONText

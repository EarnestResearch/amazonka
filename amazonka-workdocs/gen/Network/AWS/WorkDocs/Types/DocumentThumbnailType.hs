{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.DocumentThumbnailType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.DocumentThumbnailType (
  DocumentThumbnailType (
    ..
    , Large
    , Small
    , SmallHq
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DocumentThumbnailType = DocumentThumbnailType' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern Large :: DocumentThumbnailType
pattern Large = DocumentThumbnailType' "LARGE"

pattern Small :: DocumentThumbnailType
pattern Small = DocumentThumbnailType' "SMALL"

pattern SmallHq :: DocumentThumbnailType
pattern SmallHq = DocumentThumbnailType' "SMALL_HQ"

{-# COMPLETE
  Large,
  Small,
  SmallHq,
  DocumentThumbnailType' #-}

instance FromText DocumentThumbnailType where
    parser = (DocumentThumbnailType' . mk) <$> takeText

instance ToText DocumentThumbnailType where
    toText (DocumentThumbnailType' ci) = original ci

-- | Represents an enum of /known/ $DocumentThumbnailType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DocumentThumbnailType where
    toEnum i = case i of
        0 -> Large
        1 -> Small
        2 -> SmallHq
        _ -> (error . showText) $ "Unknown index for DocumentThumbnailType: " <> toText i
    fromEnum x = case x of
        Large -> 0
        Small -> 1
        SmallHq -> 2
        DocumentThumbnailType' name -> (error . showText) $ "Unknown DocumentThumbnailType: " <> original name

-- | Represents the bounds of /known/ $DocumentThumbnailType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DocumentThumbnailType where
    minBound = Large
    maxBound = SmallHq

instance Hashable     DocumentThumbnailType
instance NFData       DocumentThumbnailType
instance ToByteString DocumentThumbnailType
instance ToQuery      DocumentThumbnailType
instance ToHeader     DocumentThumbnailType

instance FromJSON DocumentThumbnailType where
    parseJSON = parseJSONText "DocumentThumbnailType"

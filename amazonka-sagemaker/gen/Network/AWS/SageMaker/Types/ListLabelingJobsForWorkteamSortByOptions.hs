{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ListLabelingJobsForWorkteamSortByOptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ListLabelingJobsForWorkteamSortByOptions (
  ListLabelingJobsForWorkteamSortByOptions (
    ..
    , LLJFWSBOCreationTime
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ListLabelingJobsForWorkteamSortByOptions = ListLabelingJobsForWorkteamSortByOptions' (CI
                                                                                             Text)
                                                  deriving (Eq, Ord, Read, Show,
                                                            Data, Typeable,
                                                            Generic)

pattern LLJFWSBOCreationTime :: ListLabelingJobsForWorkteamSortByOptions
pattern LLJFWSBOCreationTime = ListLabelingJobsForWorkteamSortByOptions' "CreationTime"

{-# COMPLETE
  LLJFWSBOCreationTime,
  ListLabelingJobsForWorkteamSortByOptions' #-}

instance FromText ListLabelingJobsForWorkteamSortByOptions where
    parser = (ListLabelingJobsForWorkteamSortByOptions' . mk) <$> takeText

instance ToText ListLabelingJobsForWorkteamSortByOptions where
    toText (ListLabelingJobsForWorkteamSortByOptions' ci) = original ci

-- | Represents an enum of /known/ $ListLabelingJobsForWorkteamSortByOptions.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ListLabelingJobsForWorkteamSortByOptions where
    toEnum i = case i of
        0 -> LLJFWSBOCreationTime
        _ -> (error . showText) $ "Unknown index for ListLabelingJobsForWorkteamSortByOptions: " <> toText i
    fromEnum x = case x of
        LLJFWSBOCreationTime -> 0
        ListLabelingJobsForWorkteamSortByOptions' name -> (error . showText) $ "Unknown ListLabelingJobsForWorkteamSortByOptions: " <> original name

-- | Represents the bounds of /known/ $ListLabelingJobsForWorkteamSortByOptions.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ListLabelingJobsForWorkteamSortByOptions where
    minBound = LLJFWSBOCreationTime
    maxBound = LLJFWSBOCreationTime

instance Hashable     ListLabelingJobsForWorkteamSortByOptions
instance NFData       ListLabelingJobsForWorkteamSortByOptions
instance ToByteString ListLabelingJobsForWorkteamSortByOptions
instance ToQuery      ListLabelingJobsForWorkteamSortByOptions
instance ToHeader     ListLabelingJobsForWorkteamSortByOptions

instance ToJSON ListLabelingJobsForWorkteamSortByOptions where
    toJSON = toJSONText

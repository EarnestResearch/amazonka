{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.ShareStatusType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.ShareStatusType (
  ShareStatusType (
    ..
    , Failure
    , Success
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ShareStatusType = ShareStatusType' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Failure :: ShareStatusType
pattern Failure = ShareStatusType' "FAILURE"

pattern Success :: ShareStatusType
pattern Success = ShareStatusType' "SUCCESS"

{-# COMPLETE
  Failure,
  Success,
  ShareStatusType' #-}

instance FromText ShareStatusType where
    parser = (ShareStatusType' . mk) <$> takeText

instance ToText ShareStatusType where
    toText (ShareStatusType' ci) = original ci

-- | Represents an enum of /known/ $ShareStatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ShareStatusType where
    toEnum i = case i of
        0 -> Failure
        1 -> Success
        _ -> (error . showText) $ "Unknown index for ShareStatusType: " <> toText i
    fromEnum x = case x of
        Failure -> 0
        Success -> 1
        ShareStatusType' name -> (error . showText) $ "Unknown ShareStatusType: " <> original name

-- | Represents the bounds of /known/ $ShareStatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ShareStatusType where
    minBound = Failure
    maxBound = Success

instance Hashable     ShareStatusType
instance NFData       ShareStatusType
instance ToByteString ShareStatusType
instance ToQuery      ShareStatusType
instance ToHeader     ShareStatusType

instance FromJSON ShareStatusType where
    parseJSON = parseJSONText "ShareStatusType"

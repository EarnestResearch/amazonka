{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.BucketVersioningStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.BucketVersioningStatus (
  BucketVersioningStatus (
    ..
    , BVSEnabled
    , BVSSuspended
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.S3.Internal

data BucketVersioningStatus = BucketVersioningStatus' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern BVSEnabled :: BucketVersioningStatus
pattern BVSEnabled = BucketVersioningStatus' "Enabled"

pattern BVSSuspended :: BucketVersioningStatus
pattern BVSSuspended = BucketVersioningStatus' "Suspended"

{-# COMPLETE
  BVSEnabled,
  BVSSuspended,
  BucketVersioningStatus' #-}

instance FromText BucketVersioningStatus where
    parser = (BucketVersioningStatus' . mk) <$> takeText

instance ToText BucketVersioningStatus where
    toText (BucketVersioningStatus' ci) = original ci

-- | Represents an enum of /known/ $BucketVersioningStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BucketVersioningStatus where
    toEnum i = case i of
        0 -> BVSEnabled
        1 -> BVSSuspended
        _ -> (error . showText) $ "Unknown index for BucketVersioningStatus: " <> toText i
    fromEnum x = case x of
        BVSEnabled -> 0
        BVSSuspended -> 1
        BucketVersioningStatus' name -> (error . showText) $ "Unknown BucketVersioningStatus: " <> original name

-- | Represents the bounds of /known/ $BucketVersioningStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BucketVersioningStatus where
    minBound = BVSEnabled
    maxBound = BVSSuspended

instance Hashable     BucketVersioningStatus
instance NFData       BucketVersioningStatus
instance ToByteString BucketVersioningStatus
instance ToQuery      BucketVersioningStatus
instance ToHeader     BucketVersioningStatus

instance FromXML BucketVersioningStatus where
    parseXML = parseXMLText "BucketVersioningStatus"

instance ToXML BucketVersioningStatus where
    toXML = toXMLText

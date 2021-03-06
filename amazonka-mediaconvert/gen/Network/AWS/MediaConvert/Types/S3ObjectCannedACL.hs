{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.S3ObjectCannedACL
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.S3ObjectCannedACL (
  S3ObjectCannedACL (
    ..
    , AuthenticatedRead
    , BucketOwnerFullControl
    , BucketOwnerRead
    , PublicRead
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Choose an Amazon S3 canned ACL for MediaConvert to apply to this output.
data S3ObjectCannedACL = S3ObjectCannedACL' (CI Text)
                           deriving (Eq, Ord, Read, Show, Data, Typeable,
                                     Generic)

pattern AuthenticatedRead :: S3ObjectCannedACL
pattern AuthenticatedRead = S3ObjectCannedACL' "AUTHENTICATED_READ"

pattern BucketOwnerFullControl :: S3ObjectCannedACL
pattern BucketOwnerFullControl = S3ObjectCannedACL' "BUCKET_OWNER_FULL_CONTROL"

pattern BucketOwnerRead :: S3ObjectCannedACL
pattern BucketOwnerRead = S3ObjectCannedACL' "BUCKET_OWNER_READ"

pattern PublicRead :: S3ObjectCannedACL
pattern PublicRead = S3ObjectCannedACL' "PUBLIC_READ"

{-# COMPLETE
  AuthenticatedRead,
  BucketOwnerFullControl,
  BucketOwnerRead,
  PublicRead,
  S3ObjectCannedACL' #-}

instance FromText S3ObjectCannedACL where
    parser = (S3ObjectCannedACL' . mk) <$> takeText

instance ToText S3ObjectCannedACL where
    toText (S3ObjectCannedACL' ci) = original ci

-- | Represents an enum of /known/ $S3ObjectCannedACL.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum S3ObjectCannedACL where
    toEnum i = case i of
        0 -> AuthenticatedRead
        1 -> BucketOwnerFullControl
        2 -> BucketOwnerRead
        3 -> PublicRead
        _ -> (error . showText) $ "Unknown index for S3ObjectCannedACL: " <> toText i
    fromEnum x = case x of
        AuthenticatedRead -> 0
        BucketOwnerFullControl -> 1
        BucketOwnerRead -> 2
        PublicRead -> 3
        S3ObjectCannedACL' name -> (error . showText) $ "Unknown S3ObjectCannedACL: " <> original name

-- | Represents the bounds of /known/ $S3ObjectCannedACL.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded S3ObjectCannedACL where
    minBound = AuthenticatedRead
    maxBound = PublicRead

instance Hashable     S3ObjectCannedACL
instance NFData       S3ObjectCannedACL
instance ToByteString S3ObjectCannedACL
instance ToQuery      S3ObjectCannedACL
instance ToHeader     S3ObjectCannedACL

instance ToJSON S3ObjectCannedACL where
    toJSON = toJSONText

instance FromJSON S3ObjectCannedACL where
    parseJSON = parseJSONText "S3ObjectCannedACL"

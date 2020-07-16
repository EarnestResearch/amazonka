{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsMediaStoreStorageClass
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.HlsMediaStoreStorageClass (
  HlsMediaStoreStorageClass (
    ..
    , Temporal
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Hls Media Store Storage Class
data HlsMediaStoreStorageClass = HlsMediaStoreStorageClass' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern Temporal :: HlsMediaStoreStorageClass
pattern Temporal = HlsMediaStoreStorageClass' "TEMPORAL"

{-# COMPLETE
  Temporal,
  HlsMediaStoreStorageClass' #-}

instance FromText HlsMediaStoreStorageClass where
    parser = (HlsMediaStoreStorageClass' . mk) <$> takeText

instance ToText HlsMediaStoreStorageClass where
    toText (HlsMediaStoreStorageClass' ci) = original ci

-- | Represents an enum of /known/ $HlsMediaStoreStorageClass.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HlsMediaStoreStorageClass where
    toEnum i = case i of
        0 -> Temporal
        _ -> (error . showText) $ "Unknown index for HlsMediaStoreStorageClass: " <> toText i
    fromEnum x = case x of
        Temporal -> 0
        HlsMediaStoreStorageClass' name -> (error . showText) $ "Unknown HlsMediaStoreStorageClass: " <> original name

-- | Represents the bounds of /known/ $HlsMediaStoreStorageClass.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HlsMediaStoreStorageClass where
    minBound = Temporal
    maxBound = Temporal

instance Hashable     HlsMediaStoreStorageClass
instance NFData       HlsMediaStoreStorageClass
instance ToByteString HlsMediaStoreStorageClass
instance ToQuery      HlsMediaStoreStorageClass
instance ToHeader     HlsMediaStoreStorageClass

instance ToJSON HlsMediaStoreStorageClass where
    toJSON = toJSONText

instance FromJSON HlsMediaStoreStorageClass where
    parseJSON = parseJSONText "HlsMediaStoreStorageClass"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsWebdavHTTPTransferMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.HlsWebdavHTTPTransferMode (
  HlsWebdavHTTPTransferMode (
    ..
    , WebdavChunked
    , WebdavNonChunked
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Placeholder documentation for HlsWebdavHttpTransferMode
data HlsWebdavHTTPTransferMode = HlsWebdavHTTPTransferMode' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern WebdavChunked :: HlsWebdavHTTPTransferMode
pattern WebdavChunked = HlsWebdavHTTPTransferMode' "CHUNKED"

pattern WebdavNonChunked :: HlsWebdavHTTPTransferMode
pattern WebdavNonChunked = HlsWebdavHTTPTransferMode' "NON_CHUNKED"

{-# COMPLETE
  WebdavChunked,
  WebdavNonChunked,
  HlsWebdavHTTPTransferMode' #-}

instance FromText HlsWebdavHTTPTransferMode where
    parser = (HlsWebdavHTTPTransferMode' . mk) <$> takeText

instance ToText HlsWebdavHTTPTransferMode where
    toText (HlsWebdavHTTPTransferMode' ci) = original ci

-- | Represents an enum of /known/ $HlsWebdavHTTPTransferMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HlsWebdavHTTPTransferMode where
    toEnum i = case i of
        0 -> WebdavChunked
        1 -> WebdavNonChunked
        _ -> (error . showText) $ "Unknown index for HlsWebdavHTTPTransferMode: " <> toText i
    fromEnum x = case x of
        WebdavChunked -> 0
        WebdavNonChunked -> 1
        HlsWebdavHTTPTransferMode' name -> (error . showText) $ "Unknown HlsWebdavHTTPTransferMode: " <> original name

-- | Represents the bounds of /known/ $HlsWebdavHTTPTransferMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HlsWebdavHTTPTransferMode where
    minBound = WebdavChunked
    maxBound = WebdavNonChunked

instance Hashable     HlsWebdavHTTPTransferMode
instance NFData       HlsWebdavHTTPTransferMode
instance ToByteString HlsWebdavHTTPTransferMode
instance ToQuery      HlsWebdavHTTPTransferMode
instance ToHeader     HlsWebdavHTTPTransferMode

instance ToJSON HlsWebdavHTTPTransferMode where
    toJSON = toJSONText

instance FromJSON HlsWebdavHTTPTransferMode where
    parseJSON = parseJSONText "HlsWebdavHTTPTransferMode"

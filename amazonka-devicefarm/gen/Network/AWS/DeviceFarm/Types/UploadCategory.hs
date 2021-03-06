{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.UploadCategory
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.UploadCategory (
  UploadCategory (
    ..
    , UCCurated
    , UCPrivate
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data UploadCategory = UploadCategory' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern UCCurated :: UploadCategory
pattern UCCurated = UploadCategory' "CURATED"

pattern UCPrivate :: UploadCategory
pattern UCPrivate = UploadCategory' "PRIVATE"

{-# COMPLETE
  UCCurated,
  UCPrivate,
  UploadCategory' #-}

instance FromText UploadCategory where
    parser = (UploadCategory' . mk) <$> takeText

instance ToText UploadCategory where
    toText (UploadCategory' ci) = original ci

-- | Represents an enum of /known/ $UploadCategory.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum UploadCategory where
    toEnum i = case i of
        0 -> UCCurated
        1 -> UCPrivate
        _ -> (error . showText) $ "Unknown index for UploadCategory: " <> toText i
    fromEnum x = case x of
        UCCurated -> 0
        UCPrivate -> 1
        UploadCategory' name -> (error . showText) $ "Unknown UploadCategory: " <> original name

-- | Represents the bounds of /known/ $UploadCategory.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded UploadCategory where
    minBound = UCCurated
    maxBound = UCPrivate

instance Hashable     UploadCategory
instance NFData       UploadCategory
instance ToByteString UploadCategory
instance ToQuery      UploadCategory
instance ToHeader     UploadCategory

instance FromJSON UploadCategory where
    parseJSON = parseJSONText "UploadCategory"

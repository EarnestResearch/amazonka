{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.CopyOption
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.CopyOption (
  CopyOption (
    ..
    , CopyTags
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CopyOption = CopyOption' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern CopyTags :: CopyOption
pattern CopyTags = CopyOption' "CopyTags"

{-# COMPLETE
  CopyTags,
  CopyOption' #-}

instance FromText CopyOption where
    parser = (CopyOption' . mk) <$> takeText

instance ToText CopyOption where
    toText (CopyOption' ci) = original ci

-- | Represents an enum of /known/ $CopyOption.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CopyOption where
    toEnum i = case i of
        0 -> CopyTags
        _ -> (error . showText) $ "Unknown index for CopyOption: " <> toText i
    fromEnum x = case x of
        CopyTags -> 0
        CopyOption' name -> (error . showText) $ "Unknown CopyOption: " <> original name

-- | Represents the bounds of /known/ $CopyOption.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CopyOption where
    minBound = CopyTags
    maxBound = CopyTags

instance Hashable     CopyOption
instance NFData       CopyOption
instance ToByteString CopyOption
instance ToQuery      CopyOption
instance ToHeader     CopyOption

instance ToJSON CopyOption where
    toJSON = toJSONText

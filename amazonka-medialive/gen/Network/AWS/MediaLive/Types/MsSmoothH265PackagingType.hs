{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MsSmoothH265PackagingType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.MsSmoothH265PackagingType (
  MsSmoothH265PackagingType (
    ..
    , MSHPTHEV1
    , MSHPTHVC1
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Ms Smooth H265 Packaging Type
data MsSmoothH265PackagingType = MsSmoothH265PackagingType' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern MSHPTHEV1 :: MsSmoothH265PackagingType
pattern MSHPTHEV1 = MsSmoothH265PackagingType' "HEV1"

pattern MSHPTHVC1 :: MsSmoothH265PackagingType
pattern MSHPTHVC1 = MsSmoothH265PackagingType' "HVC1"

{-# COMPLETE
  MSHPTHEV1,
  MSHPTHVC1,
  MsSmoothH265PackagingType' #-}

instance FromText MsSmoothH265PackagingType where
    parser = (MsSmoothH265PackagingType' . mk) <$> takeText

instance ToText MsSmoothH265PackagingType where
    toText (MsSmoothH265PackagingType' ci) = original ci

-- | Represents an enum of /known/ $MsSmoothH265PackagingType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MsSmoothH265PackagingType where
    toEnum i = case i of
        0 -> MSHPTHEV1
        1 -> MSHPTHVC1
        _ -> (error . showText) $ "Unknown index for MsSmoothH265PackagingType: " <> toText i
    fromEnum x = case x of
        MSHPTHEV1 -> 0
        MSHPTHVC1 -> 1
        MsSmoothH265PackagingType' name -> (error . showText) $ "Unknown MsSmoothH265PackagingType: " <> original name

-- | Represents the bounds of /known/ $MsSmoothH265PackagingType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MsSmoothH265PackagingType where
    minBound = MSHPTHEV1
    maxBound = MSHPTHVC1

instance Hashable     MsSmoothH265PackagingType
instance NFData       MsSmoothH265PackagingType
instance ToByteString MsSmoothH265PackagingType
instance ToQuery      MsSmoothH265PackagingType
instance ToHeader     MsSmoothH265PackagingType

instance ToJSON MsSmoothH265PackagingType where
    toJSON = toJSONText

instance FromJSON MsSmoothH265PackagingType where
    parseJSON = parseJSONText "MsSmoothH265PackagingType"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.AZMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.AZMode (
  AZMode (
    ..
    , CrossAz
    , SingleAz
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AZMode = AZMode' (CI Text)
                deriving (Eq, Ord, Read, Show, Data, Typeable,
                          Generic)

pattern CrossAz :: AZMode
pattern CrossAz = AZMode' "cross-az"

pattern SingleAz :: AZMode
pattern SingleAz = AZMode' "single-az"

{-# COMPLETE
  CrossAz,
  SingleAz,
  AZMode' #-}

instance FromText AZMode where
    parser = (AZMode' . mk) <$> takeText

instance ToText AZMode where
    toText (AZMode' ci) = original ci

-- | Represents an enum of /known/ $AZMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AZMode where
    toEnum i = case i of
        0 -> CrossAz
        1 -> SingleAz
        _ -> (error . showText) $ "Unknown index for AZMode: " <> toText i
    fromEnum x = case x of
        CrossAz -> 0
        SingleAz -> 1
        AZMode' name -> (error . showText) $ "Unknown AZMode: " <> original name

-- | Represents the bounds of /known/ $AZMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AZMode where
    minBound = CrossAz
    maxBound = SingleAz

instance Hashable     AZMode
instance NFData       AZMode
instance ToByteString AZMode
instance ToQuery      AZMode
instance ToHeader     AZMode

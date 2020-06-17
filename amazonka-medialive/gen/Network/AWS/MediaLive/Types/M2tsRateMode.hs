{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsRateMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.M2tsRateMode (
  M2tsRateMode (
    ..
    , MRMCbr
    , MRMVbr
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Placeholder documentation for M2tsRateMode
data M2tsRateMode = M2tsRateMode' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern MRMCbr :: M2tsRateMode
pattern MRMCbr = M2tsRateMode' "CBR"

pattern MRMVbr :: M2tsRateMode
pattern MRMVbr = M2tsRateMode' "VBR"

{-# COMPLETE
  MRMCbr,
  MRMVbr,
  M2tsRateMode' #-}

instance FromText M2tsRateMode where
    parser = (M2tsRateMode' . mk) <$> takeText

instance ToText M2tsRateMode where
    toText (M2tsRateMode' ci) = original ci

-- | Represents an enum of /known/ $M2tsRateMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum M2tsRateMode where
    toEnum i = case i of
        0 -> MRMCbr
        1 -> MRMVbr
        _ -> (error . showText) $ "Unknown index for M2tsRateMode: " <> toText i
    fromEnum x = case x of
        MRMCbr -> 0
        MRMVbr -> 1
        M2tsRateMode' name -> (error . showText) $ "Unknown M2tsRateMode: " <> original name

-- | Represents the bounds of /known/ $M2tsRateMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded M2tsRateMode where
    minBound = MRMCbr
    maxBound = MRMVbr

instance Hashable     M2tsRateMode
instance NFData       M2tsRateMode
instance ToByteString M2tsRateMode
instance ToQuery      M2tsRateMode
instance ToHeader     M2tsRateMode

instance ToJSON M2tsRateMode where
    toJSON = toJSONText

instance FromJSON M2tsRateMode where
    parseJSON = parseJSONText "M2tsRateMode"

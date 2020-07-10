{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.RebootOption
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.RebootOption (
  RebootOption (
    ..
    , NoReboot
    , RebootIfNeeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RebootOption = RebootOption' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern NoReboot :: RebootOption
pattern NoReboot = RebootOption' "NoReboot"

pattern RebootIfNeeded :: RebootOption
pattern RebootIfNeeded = RebootOption' "RebootIfNeeded"

{-# COMPLETE
  NoReboot,
  RebootIfNeeded,
  RebootOption' #-}

instance FromText RebootOption where
    parser = (RebootOption' . mk) <$> takeText

instance ToText RebootOption where
    toText (RebootOption' ci) = original ci

-- | Represents an enum of /known/ $RebootOption.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RebootOption where
    toEnum i = case i of
        0 -> NoReboot
        1 -> RebootIfNeeded
        _ -> (error . showText) $ "Unknown index for RebootOption: " <> toText i
    fromEnum x = case x of
        NoReboot -> 0
        RebootIfNeeded -> 1
        RebootOption' name -> (error . showText) $ "Unknown RebootOption: " <> original name

-- | Represents the bounds of /known/ $RebootOption.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RebootOption where
    minBound = NoReboot
    maxBound = RebootIfNeeded

instance Hashable     RebootOption
instance NFData       RebootOption
instance ToByteString RebootOption
instance ToQuery      RebootOption
instance ToHeader     RebootOption

instance FromJSON RebootOption where
    parseJSON = parseJSONText "RebootOption"

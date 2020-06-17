{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BurnInBackgroundColor
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.BurnInBackgroundColor (
  BurnInBackgroundColor (
    ..
    , BIBCBlack
    , BIBCNone
    , BIBCWhite
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Placeholder documentation for BurnInBackgroundColor
data BurnInBackgroundColor = BurnInBackgroundColor' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern BIBCBlack :: BurnInBackgroundColor
pattern BIBCBlack = BurnInBackgroundColor' "BLACK"

pattern BIBCNone :: BurnInBackgroundColor
pattern BIBCNone = BurnInBackgroundColor' "NONE"

pattern BIBCWhite :: BurnInBackgroundColor
pattern BIBCWhite = BurnInBackgroundColor' "WHITE"

{-# COMPLETE
  BIBCBlack,
  BIBCNone,
  BIBCWhite,
  BurnInBackgroundColor' #-}

instance FromText BurnInBackgroundColor where
    parser = (BurnInBackgroundColor' . mk) <$> takeText

instance ToText BurnInBackgroundColor where
    toText (BurnInBackgroundColor' ci) = original ci

-- | Represents an enum of /known/ $BurnInBackgroundColor.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BurnInBackgroundColor where
    toEnum i = case i of
        0 -> BIBCBlack
        1 -> BIBCNone
        2 -> BIBCWhite
        _ -> (error . showText) $ "Unknown index for BurnInBackgroundColor: " <> toText i
    fromEnum x = case x of
        BIBCBlack -> 0
        BIBCNone -> 1
        BIBCWhite -> 2
        BurnInBackgroundColor' name -> (error . showText) $ "Unknown BurnInBackgroundColor: " <> original name

-- | Represents the bounds of /known/ $BurnInBackgroundColor.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BurnInBackgroundColor where
    minBound = BIBCBlack
    maxBound = BIBCWhite

instance Hashable     BurnInBackgroundColor
instance NFData       BurnInBackgroundColor
instance ToByteString BurnInBackgroundColor
instance ToQuery      BurnInBackgroundColor
instance ToHeader     BurnInBackgroundColor

instance ToJSON BurnInBackgroundColor where
    toJSON = toJSONText

instance FromJSON BurnInBackgroundColor where
    parseJSON = parseJSONText "BurnInBackgroundColor"

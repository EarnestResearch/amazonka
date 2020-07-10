{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.MarketType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.MarketType (
  MarketType (
    ..
    , Spot
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data MarketType = MarketType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern Spot :: MarketType
pattern Spot = MarketType' "spot"

{-# COMPLETE
  Spot,
  MarketType' #-}

instance FromText MarketType where
    parser = (MarketType' . mk) <$> takeText

instance ToText MarketType where
    toText (MarketType' ci) = original ci

-- | Represents an enum of /known/ $MarketType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MarketType where
    toEnum i = case i of
        0 -> Spot
        _ -> (error . showText) $ "Unknown index for MarketType: " <> toText i
    fromEnum x = case x of
        Spot -> 0
        MarketType' name -> (error . showText) $ "Unknown MarketType: " <> original name

-- | Represents the bounds of /known/ $MarketType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MarketType where
    minBound = Spot
    maxBound = Spot

instance Hashable     MarketType
instance NFData       MarketType
instance ToByteString MarketType
instance ToQuery      MarketType
instance ToHeader     MarketType

instance FromXML MarketType where
    parseXML = parseXMLText "MarketType"

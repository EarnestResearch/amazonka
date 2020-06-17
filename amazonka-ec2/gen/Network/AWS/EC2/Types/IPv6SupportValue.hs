{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IPv6SupportValue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.IPv6SupportValue (
  IPv6SupportValue (
    ..
    , ISVDisable
    , ISVEnable
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data IPv6SupportValue = IPv6SupportValue' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern ISVDisable :: IPv6SupportValue
pattern ISVDisable = IPv6SupportValue' "disable"

pattern ISVEnable :: IPv6SupportValue
pattern ISVEnable = IPv6SupportValue' "enable"

{-# COMPLETE
  ISVDisable,
  ISVEnable,
  IPv6SupportValue' #-}

instance FromText IPv6SupportValue where
    parser = (IPv6SupportValue' . mk) <$> takeText

instance ToText IPv6SupportValue where
    toText (IPv6SupportValue' ci) = original ci

-- | Represents an enum of /known/ $IPv6SupportValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum IPv6SupportValue where
    toEnum i = case i of
        0 -> ISVDisable
        1 -> ISVEnable
        _ -> (error . showText) $ "Unknown index for IPv6SupportValue: " <> toText i
    fromEnum x = case x of
        ISVDisable -> 0
        ISVEnable -> 1
        IPv6SupportValue' name -> (error . showText) $ "Unknown IPv6SupportValue: " <> original name

-- | Represents the bounds of /known/ $IPv6SupportValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded IPv6SupportValue where
    minBound = ISVDisable
    maxBound = ISVEnable

instance Hashable     IPv6SupportValue
instance NFData       IPv6SupportValue
instance ToByteString IPv6SupportValue
instance ToQuery      IPv6SupportValue
instance ToHeader     IPv6SupportValue

instance FromXML IPv6SupportValue where
    parseXML = parseXMLText "IPv6SupportValue"

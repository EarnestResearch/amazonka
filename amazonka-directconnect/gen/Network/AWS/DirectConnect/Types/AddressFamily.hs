{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.AddressFamily
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.AddressFamily (
  AddressFamily (
    ..
    , IPV4
    , IPV6
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AddressFamily = AddressFamily' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern IPV4 :: AddressFamily
pattern IPV4 = AddressFamily' "ipv4"

pattern IPV6 :: AddressFamily
pattern IPV6 = AddressFamily' "ipv6"

{-# COMPLETE
  IPV4,
  IPV6,
  AddressFamily' #-}

instance FromText AddressFamily where
    parser = (AddressFamily' . mk) <$> takeText

instance ToText AddressFamily where
    toText (AddressFamily' ci) = original ci

-- | Represents an enum of /known/ $AddressFamily.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AddressFamily where
    toEnum i = case i of
        0 -> IPV4
        1 -> IPV6
        _ -> (error . showText) $ "Unknown index for AddressFamily: " <> toText i
    fromEnum x = case x of
        IPV4 -> 0
        IPV6 -> 1
        AddressFamily' name -> (error . showText) $ "Unknown AddressFamily: " <> original name

-- | Represents the bounds of /known/ $AddressFamily.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AddressFamily where
    minBound = IPV4
    maxBound = IPV6

instance Hashable     AddressFamily
instance NFData       AddressFamily
instance ToByteString AddressFamily
instance ToQuery      AddressFamily
instance ToHeader     AddressFamily

instance ToJSON AddressFamily where
    toJSON = toJSONText

instance FromJSON AddressFamily where
    parseJSON = parseJSONText "AddressFamily"

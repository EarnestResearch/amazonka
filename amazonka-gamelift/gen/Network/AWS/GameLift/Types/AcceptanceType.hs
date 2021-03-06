{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.AcceptanceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.AcceptanceType (
  AcceptanceType (
    ..
    , Accept
    , Reject
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AcceptanceType = AcceptanceType' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern Accept :: AcceptanceType
pattern Accept = AcceptanceType' "ACCEPT"

pattern Reject :: AcceptanceType
pattern Reject = AcceptanceType' "REJECT"

{-# COMPLETE
  Accept,
  Reject,
  AcceptanceType' #-}

instance FromText AcceptanceType where
    parser = (AcceptanceType' . mk) <$> takeText

instance ToText AcceptanceType where
    toText (AcceptanceType' ci) = original ci

-- | Represents an enum of /known/ $AcceptanceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AcceptanceType where
    toEnum i = case i of
        0 -> Accept
        1 -> Reject
        _ -> (error . showText) $ "Unknown index for AcceptanceType: " <> toText i
    fromEnum x = case x of
        Accept -> 0
        Reject -> 1
        AcceptanceType' name -> (error . showText) $ "Unknown AcceptanceType: " <> original name

-- | Represents the bounds of /known/ $AcceptanceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AcceptanceType where
    minBound = Accept
    maxBound = Reject

instance Hashable     AcceptanceType
instance NFData       AcceptanceType
instance ToByteString AcceptanceType
instance ToQuery      AcceptanceType
instance ToHeader     AcceptanceType

instance ToJSON AcceptanceType where
    toJSON = toJSONText

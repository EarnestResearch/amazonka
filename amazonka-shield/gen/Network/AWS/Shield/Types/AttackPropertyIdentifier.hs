{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.AttackPropertyIdentifier
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types.AttackPropertyIdentifier (
  AttackPropertyIdentifier (
    ..
    , DestinationURL
    , Referrer
    , SourceASN
    , SourceCountry
    , SourceIPAddress
    , SourceUserAgent
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AttackPropertyIdentifier = AttackPropertyIdentifier' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern DestinationURL :: AttackPropertyIdentifier
pattern DestinationURL = AttackPropertyIdentifier' "DESTINATION_URL"

pattern Referrer :: AttackPropertyIdentifier
pattern Referrer = AttackPropertyIdentifier' "REFERRER"

pattern SourceASN :: AttackPropertyIdentifier
pattern SourceASN = AttackPropertyIdentifier' "SOURCE_ASN"

pattern SourceCountry :: AttackPropertyIdentifier
pattern SourceCountry = AttackPropertyIdentifier' "SOURCE_COUNTRY"

pattern SourceIPAddress :: AttackPropertyIdentifier
pattern SourceIPAddress = AttackPropertyIdentifier' "SOURCE_IP_ADDRESS"

pattern SourceUserAgent :: AttackPropertyIdentifier
pattern SourceUserAgent = AttackPropertyIdentifier' "SOURCE_USER_AGENT"

{-# COMPLETE
  DestinationURL,
  Referrer,
  SourceASN,
  SourceCountry,
  SourceIPAddress,
  SourceUserAgent,
  AttackPropertyIdentifier' #-}

instance FromText AttackPropertyIdentifier where
    parser = (AttackPropertyIdentifier' . mk) <$> takeText

instance ToText AttackPropertyIdentifier where
    toText (AttackPropertyIdentifier' ci) = original ci

-- | Represents an enum of /known/ $AttackPropertyIdentifier.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AttackPropertyIdentifier where
    toEnum i = case i of
        0 -> DestinationURL
        1 -> Referrer
        2 -> SourceASN
        3 -> SourceCountry
        4 -> SourceIPAddress
        5 -> SourceUserAgent
        _ -> (error . showText) $ "Unknown index for AttackPropertyIdentifier: " <> toText i
    fromEnum x = case x of
        DestinationURL -> 0
        Referrer -> 1
        SourceASN -> 2
        SourceCountry -> 3
        SourceIPAddress -> 4
        SourceUserAgent -> 5
        AttackPropertyIdentifier' name -> (error . showText) $ "Unknown AttackPropertyIdentifier: " <> original name

-- | Represents the bounds of /known/ $AttackPropertyIdentifier.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AttackPropertyIdentifier where
    minBound = DestinationURL
    maxBound = SourceUserAgent

instance Hashable     AttackPropertyIdentifier
instance NFData       AttackPropertyIdentifier
instance ToByteString AttackPropertyIdentifier
instance ToQuery      AttackPropertyIdentifier
instance ToHeader     AttackPropertyIdentifier

instance FromJSON AttackPropertyIdentifier where
    parseJSON = parseJSONText "AttackPropertyIdentifier"

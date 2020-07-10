{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ListingStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ListingStatus (
  ListingStatus (
    ..
    , LSActive
    , LSCancelled
    , LSClosed
    , LSPending
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ListingStatus = ListingStatus' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern LSActive :: ListingStatus
pattern LSActive = ListingStatus' "active"

pattern LSCancelled :: ListingStatus
pattern LSCancelled = ListingStatus' "cancelled"

pattern LSClosed :: ListingStatus
pattern LSClosed = ListingStatus' "closed"

pattern LSPending :: ListingStatus
pattern LSPending = ListingStatus' "pending"

{-# COMPLETE
  LSActive,
  LSCancelled,
  LSClosed,
  LSPending,
  ListingStatus' #-}

instance FromText ListingStatus where
    parser = (ListingStatus' . mk) <$> takeText

instance ToText ListingStatus where
    toText (ListingStatus' ci) = original ci

-- | Represents an enum of /known/ $ListingStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ListingStatus where
    toEnum i = case i of
        0 -> LSActive
        1 -> LSCancelled
        2 -> LSClosed
        3 -> LSPending
        _ -> (error . showText) $ "Unknown index for ListingStatus: " <> toText i
    fromEnum x = case x of
        LSActive -> 0
        LSCancelled -> 1
        LSClosed -> 2
        LSPending -> 3
        ListingStatus' name -> (error . showText) $ "Unknown ListingStatus: " <> original name

-- | Represents the bounds of /known/ $ListingStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ListingStatus where
    minBound = LSActive
    maxBound = LSPending

instance Hashable     ListingStatus
instance NFData       ListingStatus
instance ToByteString ListingStatus
instance ToQuery      ListingStatus
instance ToHeader     ListingStatus

instance FromXML ListingStatus where
    parseXML = parseXMLText "ListingStatus"

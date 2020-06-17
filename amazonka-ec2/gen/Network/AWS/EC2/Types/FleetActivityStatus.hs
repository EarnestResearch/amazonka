{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetActivityStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.FleetActivityStatus (
  FleetActivityStatus (
    ..
    , Error'
    , Fulfilled
    , PendingFulfillment
    , PendingTermination
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data FleetActivityStatus = FleetActivityStatus' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern Error' :: FleetActivityStatus
pattern Error' = FleetActivityStatus' "error"

pattern Fulfilled :: FleetActivityStatus
pattern Fulfilled = FleetActivityStatus' "fulfilled"

pattern PendingFulfillment :: FleetActivityStatus
pattern PendingFulfillment = FleetActivityStatus' "pending_fulfillment"

pattern PendingTermination :: FleetActivityStatus
pattern PendingTermination = FleetActivityStatus' "pending_termination"

{-# COMPLETE
  Error',
  Fulfilled,
  PendingFulfillment,
  PendingTermination,
  FleetActivityStatus' #-}

instance FromText FleetActivityStatus where
    parser = (FleetActivityStatus' . mk) <$> takeText

instance ToText FleetActivityStatus where
    toText (FleetActivityStatus' ci) = original ci

-- | Represents an enum of /known/ $FleetActivityStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FleetActivityStatus where
    toEnum i = case i of
        0 -> Error'
        1 -> Fulfilled
        2 -> PendingFulfillment
        3 -> PendingTermination
        _ -> (error . showText) $ "Unknown index for FleetActivityStatus: " <> toText i
    fromEnum x = case x of
        Error' -> 0
        Fulfilled -> 1
        PendingFulfillment -> 2
        PendingTermination -> 3
        FleetActivityStatus' name -> (error . showText) $ "Unknown FleetActivityStatus: " <> original name

-- | Represents the bounds of /known/ $FleetActivityStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FleetActivityStatus where
    minBound = Error'
    maxBound = PendingTermination

instance Hashable     FleetActivityStatus
instance NFData       FleetActivityStatus
instance ToByteString FleetActivityStatus
instance ToQuery      FleetActivityStatus
instance ToHeader     FleetActivityStatus

instance FromXML FleetActivityStatus where
    parseXML = parseXMLText "FleetActivityStatus"

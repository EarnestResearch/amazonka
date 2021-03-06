{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.InsufficientDataHealthStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.InsufficientDataHealthStatus (
  InsufficientDataHealthStatus (
    ..
    , Healthy
    , LastKnownStatus
    , Unhealthy
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.Route53.Internal

data InsufficientDataHealthStatus = InsufficientDataHealthStatus' (CI
                                                                     Text)
                                      deriving (Eq, Ord, Read, Show, Data,
                                                Typeable, Generic)

pattern Healthy :: InsufficientDataHealthStatus
pattern Healthy = InsufficientDataHealthStatus' "Healthy"

pattern LastKnownStatus :: InsufficientDataHealthStatus
pattern LastKnownStatus = InsufficientDataHealthStatus' "LastKnownStatus"

pattern Unhealthy :: InsufficientDataHealthStatus
pattern Unhealthy = InsufficientDataHealthStatus' "Unhealthy"

{-# COMPLETE
  Healthy,
  LastKnownStatus,
  Unhealthy,
  InsufficientDataHealthStatus' #-}

instance FromText InsufficientDataHealthStatus where
    parser = (InsufficientDataHealthStatus' . mk) <$> takeText

instance ToText InsufficientDataHealthStatus where
    toText (InsufficientDataHealthStatus' ci) = original ci

-- | Represents an enum of /known/ $InsufficientDataHealthStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InsufficientDataHealthStatus where
    toEnum i = case i of
        0 -> Healthy
        1 -> LastKnownStatus
        2 -> Unhealthy
        _ -> (error . showText) $ "Unknown index for InsufficientDataHealthStatus: " <> toText i
    fromEnum x = case x of
        Healthy -> 0
        LastKnownStatus -> 1
        Unhealthy -> 2
        InsufficientDataHealthStatus' name -> (error . showText) $ "Unknown InsufficientDataHealthStatus: " <> original name

-- | Represents the bounds of /known/ $InsufficientDataHealthStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InsufficientDataHealthStatus where
    minBound = Healthy
    maxBound = Unhealthy

instance Hashable     InsufficientDataHealthStatus
instance NFData       InsufficientDataHealthStatus
instance ToByteString InsufficientDataHealthStatus
instance ToQuery      InsufficientDataHealthStatus
instance ToHeader     InsufficientDataHealthStatus

instance FromXML InsufficientDataHealthStatus where
    parseXML = parseXMLText "InsufficientDataHealthStatus"

instance ToXML InsufficientDataHealthStatus where
    toXML = toXMLText

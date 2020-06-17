{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ByoipCidrState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ByoipCidrState (
  ByoipCidrState (
    ..
    , Advertised
    , Deprovisioned
    , FailedDeprovision
    , FailedProvision
    , PendingDeprovision
    , PendingProvision
    , Provisioned
    , ProvisionedNotPubliclyAdvertisable
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ByoipCidrState = ByoipCidrState' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern Advertised :: ByoipCidrState
pattern Advertised = ByoipCidrState' "advertised"

pattern Deprovisioned :: ByoipCidrState
pattern Deprovisioned = ByoipCidrState' "deprovisioned"

pattern FailedDeprovision :: ByoipCidrState
pattern FailedDeprovision = ByoipCidrState' "failed-deprovision"

pattern FailedProvision :: ByoipCidrState
pattern FailedProvision = ByoipCidrState' "failed-provision"

pattern PendingDeprovision :: ByoipCidrState
pattern PendingDeprovision = ByoipCidrState' "pending-deprovision"

pattern PendingProvision :: ByoipCidrState
pattern PendingProvision = ByoipCidrState' "pending-provision"

pattern Provisioned :: ByoipCidrState
pattern Provisioned = ByoipCidrState' "provisioned"

pattern ProvisionedNotPubliclyAdvertisable :: ByoipCidrState
pattern ProvisionedNotPubliclyAdvertisable = ByoipCidrState' "provisioned-not-publicly-advertisable"

{-# COMPLETE
  Advertised,
  Deprovisioned,
  FailedDeprovision,
  FailedProvision,
  PendingDeprovision,
  PendingProvision,
  Provisioned,
  ProvisionedNotPubliclyAdvertisable,
  ByoipCidrState' #-}

instance FromText ByoipCidrState where
    parser = (ByoipCidrState' . mk) <$> takeText

instance ToText ByoipCidrState where
    toText (ByoipCidrState' ci) = original ci

-- | Represents an enum of /known/ $ByoipCidrState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ByoipCidrState where
    toEnum i = case i of
        0 -> Advertised
        1 -> Deprovisioned
        2 -> FailedDeprovision
        3 -> FailedProvision
        4 -> PendingDeprovision
        5 -> PendingProvision
        6 -> Provisioned
        7 -> ProvisionedNotPubliclyAdvertisable
        _ -> (error . showText) $ "Unknown index for ByoipCidrState: " <> toText i
    fromEnum x = case x of
        Advertised -> 0
        Deprovisioned -> 1
        FailedDeprovision -> 2
        FailedProvision -> 3
        PendingDeprovision -> 4
        PendingProvision -> 5
        Provisioned -> 6
        ProvisionedNotPubliclyAdvertisable -> 7
        ByoipCidrState' name -> (error . showText) $ "Unknown ByoipCidrState: " <> original name

-- | Represents the bounds of /known/ $ByoipCidrState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ByoipCidrState where
    minBound = Advertised
    maxBound = ProvisionedNotPubliclyAdvertisable

instance Hashable     ByoipCidrState
instance NFData       ByoipCidrState
instance ToByteString ByoipCidrState
instance ToQuery      ByoipCidrState
instance ToHeader     ByoipCidrState

instance FromXML ByoipCidrState where
    parseXML = parseXMLText "ByoipCidrState"

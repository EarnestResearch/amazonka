{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceHealthReason
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.InstanceHealthReason (
  InstanceHealthReason (
    ..
    , Instance_DeregistrationInProgress
    , Instance_FailedHealthChecks
    , Instance_IPUnusable
    , Instance_InvalidState
    , Instance_NotInUse
    , Instance_NotRegistered
    , Instance_ResponseCodeMismatch
    , Instance_Timeout
    , Lb_InitialHealthChecking
    , Lb_InternalError
    , Lb_RegistrationInProgress
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data InstanceHealthReason = InstanceHealthReason' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern Instance_DeregistrationInProgress :: InstanceHealthReason
pattern Instance_DeregistrationInProgress = InstanceHealthReason' "Instance.DeregistrationInProgress"

pattern Instance_FailedHealthChecks :: InstanceHealthReason
pattern Instance_FailedHealthChecks = InstanceHealthReason' "Instance.FailedHealthChecks"

pattern Instance_IPUnusable :: InstanceHealthReason
pattern Instance_IPUnusable = InstanceHealthReason' "Instance.IpUnusable"

pattern Instance_InvalidState :: InstanceHealthReason
pattern Instance_InvalidState = InstanceHealthReason' "Instance.InvalidState"

pattern Instance_NotInUse :: InstanceHealthReason
pattern Instance_NotInUse = InstanceHealthReason' "Instance.NotInUse"

pattern Instance_NotRegistered :: InstanceHealthReason
pattern Instance_NotRegistered = InstanceHealthReason' "Instance.NotRegistered"

pattern Instance_ResponseCodeMismatch :: InstanceHealthReason
pattern Instance_ResponseCodeMismatch = InstanceHealthReason' "Instance.ResponseCodeMismatch"

pattern Instance_Timeout :: InstanceHealthReason
pattern Instance_Timeout = InstanceHealthReason' "Instance.Timeout"

pattern Lb_InitialHealthChecking :: InstanceHealthReason
pattern Lb_InitialHealthChecking = InstanceHealthReason' "Lb.InitialHealthChecking"

pattern Lb_InternalError :: InstanceHealthReason
pattern Lb_InternalError = InstanceHealthReason' "Lb.InternalError"

pattern Lb_RegistrationInProgress :: InstanceHealthReason
pattern Lb_RegistrationInProgress = InstanceHealthReason' "Lb.RegistrationInProgress"

{-# COMPLETE
  Instance_DeregistrationInProgress,
  Instance_FailedHealthChecks,
  Instance_IPUnusable,
  Instance_InvalidState,
  Instance_NotInUse,
  Instance_NotRegistered,
  Instance_ResponseCodeMismatch,
  Instance_Timeout,
  Lb_InitialHealthChecking,
  Lb_InternalError,
  Lb_RegistrationInProgress,
  InstanceHealthReason' #-}

instance FromText InstanceHealthReason where
    parser = (InstanceHealthReason' . mk) <$> takeText

instance ToText InstanceHealthReason where
    toText (InstanceHealthReason' ci) = original ci

-- | Represents an enum of /known/ $InstanceHealthReason.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InstanceHealthReason where
    toEnum i = case i of
        0 -> Instance_DeregistrationInProgress
        1 -> Instance_FailedHealthChecks
        2 -> Instance_IPUnusable
        3 -> Instance_InvalidState
        4 -> Instance_NotInUse
        5 -> Instance_NotRegistered
        6 -> Instance_ResponseCodeMismatch
        7 -> Instance_Timeout
        8 -> Lb_InitialHealthChecking
        9 -> Lb_InternalError
        10 -> Lb_RegistrationInProgress
        _ -> (error . showText) $ "Unknown index for InstanceHealthReason: " <> toText i
    fromEnum x = case x of
        Instance_DeregistrationInProgress -> 0
        Instance_FailedHealthChecks -> 1
        Instance_IPUnusable -> 2
        Instance_InvalidState -> 3
        Instance_NotInUse -> 4
        Instance_NotRegistered -> 5
        Instance_ResponseCodeMismatch -> 6
        Instance_Timeout -> 7
        Lb_InitialHealthChecking -> 8
        Lb_InternalError -> 9
        Lb_RegistrationInProgress -> 10
        InstanceHealthReason' name -> (error . showText) $ "Unknown InstanceHealthReason: " <> original name

-- | Represents the bounds of /known/ $InstanceHealthReason.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InstanceHealthReason where
    minBound = Instance_DeregistrationInProgress
    maxBound = Lb_RegistrationInProgress

instance Hashable     InstanceHealthReason
instance NFData       InstanceHealthReason
instance ToByteString InstanceHealthReason
instance ToQuery      InstanceHealthReason
instance ToHeader     InstanceHealthReason

instance FromJSON InstanceHealthReason where
    parseJSON = parseJSONText "InstanceHealthReason"

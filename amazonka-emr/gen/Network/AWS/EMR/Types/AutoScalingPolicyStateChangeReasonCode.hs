{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReasonCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReasonCode (
  AutoScalingPolicyStateChangeReasonCode (
    ..
    , CleanupFailure
    , ProvisionFailure
    , UserRequest
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AutoScalingPolicyStateChangeReasonCode = AutoScalingPolicyStateChangeReasonCode' (CI
                                                                                         Text)
                                                deriving (Eq, Ord, Read, Show,
                                                          Data, Typeable,
                                                          Generic)

pattern CleanupFailure :: AutoScalingPolicyStateChangeReasonCode
pattern CleanupFailure = AutoScalingPolicyStateChangeReasonCode' "CLEANUP_FAILURE"

pattern ProvisionFailure :: AutoScalingPolicyStateChangeReasonCode
pattern ProvisionFailure = AutoScalingPolicyStateChangeReasonCode' "PROVISION_FAILURE"

pattern UserRequest :: AutoScalingPolicyStateChangeReasonCode
pattern UserRequest = AutoScalingPolicyStateChangeReasonCode' "USER_REQUEST"

{-# COMPLETE
  CleanupFailure,
  ProvisionFailure,
  UserRequest,
  AutoScalingPolicyStateChangeReasonCode' #-}

instance FromText AutoScalingPolicyStateChangeReasonCode where
    parser = (AutoScalingPolicyStateChangeReasonCode' . mk) <$> takeText

instance ToText AutoScalingPolicyStateChangeReasonCode where
    toText (AutoScalingPolicyStateChangeReasonCode' ci) = original ci

-- | Represents an enum of /known/ $AutoScalingPolicyStateChangeReasonCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AutoScalingPolicyStateChangeReasonCode where
    toEnum i = case i of
        0 -> CleanupFailure
        1 -> ProvisionFailure
        2 -> UserRequest
        _ -> (error . showText) $ "Unknown index for AutoScalingPolicyStateChangeReasonCode: " <> toText i
    fromEnum x = case x of
        CleanupFailure -> 0
        ProvisionFailure -> 1
        UserRequest -> 2
        AutoScalingPolicyStateChangeReasonCode' name -> (error . showText) $ "Unknown AutoScalingPolicyStateChangeReasonCode: " <> original name

-- | Represents the bounds of /known/ $AutoScalingPolicyStateChangeReasonCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AutoScalingPolicyStateChangeReasonCode where
    minBound = CleanupFailure
    maxBound = UserRequest

instance Hashable     AutoScalingPolicyStateChangeReasonCode
instance NFData       AutoScalingPolicyStateChangeReasonCode
instance ToByteString AutoScalingPolicyStateChangeReasonCode
instance ToQuery      AutoScalingPolicyStateChangeReasonCode
instance ToHeader     AutoScalingPolicyStateChangeReasonCode

instance FromJSON AutoScalingPolicyStateChangeReasonCode where
    parseJSON = parseJSONText "AutoScalingPolicyStateChangeReasonCode"

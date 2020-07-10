{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ResourceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.ResourceType (
  ResourceType (
    ..
    , RTAccountSettings
    , RTCaCertificate
    , RTClientId
    , RTCognitoIdentityPool
    , RTDeviceCertificate
    , RTIAMRole
    , RTIotPolicy
    , RTRoleAlias
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ResourceType = ResourceType' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern RTAccountSettings :: ResourceType
pattern RTAccountSettings = ResourceType' "ACCOUNT_SETTINGS"

pattern RTCaCertificate :: ResourceType
pattern RTCaCertificate = ResourceType' "CA_CERTIFICATE"

pattern RTClientId :: ResourceType
pattern RTClientId = ResourceType' "CLIENT_ID"

pattern RTCognitoIdentityPool :: ResourceType
pattern RTCognitoIdentityPool = ResourceType' "COGNITO_IDENTITY_POOL"

pattern RTDeviceCertificate :: ResourceType
pattern RTDeviceCertificate = ResourceType' "DEVICE_CERTIFICATE"

pattern RTIAMRole :: ResourceType
pattern RTIAMRole = ResourceType' "IAM_ROLE"

pattern RTIotPolicy :: ResourceType
pattern RTIotPolicy = ResourceType' "IOT_POLICY"

pattern RTRoleAlias :: ResourceType
pattern RTRoleAlias = ResourceType' "ROLE_ALIAS"

{-# COMPLETE
  RTAccountSettings,
  RTCaCertificate,
  RTClientId,
  RTCognitoIdentityPool,
  RTDeviceCertificate,
  RTIAMRole,
  RTIotPolicy,
  RTRoleAlias,
  ResourceType' #-}

instance FromText ResourceType where
    parser = (ResourceType' . mk) <$> takeText

instance ToText ResourceType where
    toText (ResourceType' ci) = original ci

-- | Represents an enum of /known/ $ResourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ResourceType where
    toEnum i = case i of
        0 -> RTAccountSettings
        1 -> RTCaCertificate
        2 -> RTClientId
        3 -> RTCognitoIdentityPool
        4 -> RTDeviceCertificate
        5 -> RTIAMRole
        6 -> RTIotPolicy
        7 -> RTRoleAlias
        _ -> (error . showText) $ "Unknown index for ResourceType: " <> toText i
    fromEnum x = case x of
        RTAccountSettings -> 0
        RTCaCertificate -> 1
        RTClientId -> 2
        RTCognitoIdentityPool -> 3
        RTDeviceCertificate -> 4
        RTIAMRole -> 5
        RTIotPolicy -> 6
        RTRoleAlias -> 7
        ResourceType' name -> (error . showText) $ "Unknown ResourceType: " <> original name

-- | Represents the bounds of /known/ $ResourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ResourceType where
    minBound = RTAccountSettings
    maxBound = RTRoleAlias

instance Hashable     ResourceType
instance NFData       ResourceType
instance ToByteString ResourceType
instance ToQuery      ResourceType
instance ToHeader     ResourceType

instance FromJSON ResourceType where
    parseJSON = parseJSONText "ResourceType"

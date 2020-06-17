{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRevocationReason
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRevocationReason (
  LoadBalancerTLSCertificateRevocationReason (
    ..
    , AACompromise
    , AffiliationChanged
    , CaCompromise
    , CertificateHold
    , CessationOfOperation
    , KeyCompromise
    , PrivilegeWithdrawn
    , RemoveFromCrl
    , Superceded
    , Unspecified
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LoadBalancerTLSCertificateRevocationReason = LoadBalancerTLSCertificateRevocationReason' (CI
                                                                                                 Text)
                                                    deriving (Eq, Ord, Read,
                                                              Show, Data,
                                                              Typeable, Generic)

pattern AACompromise :: LoadBalancerTLSCertificateRevocationReason
pattern AACompromise = LoadBalancerTLSCertificateRevocationReason' "A_A_COMPROMISE"

pattern AffiliationChanged :: LoadBalancerTLSCertificateRevocationReason
pattern AffiliationChanged = LoadBalancerTLSCertificateRevocationReason' "AFFILIATION_CHANGED"

pattern CaCompromise :: LoadBalancerTLSCertificateRevocationReason
pattern CaCompromise = LoadBalancerTLSCertificateRevocationReason' "CA_COMPROMISE"

pattern CertificateHold :: LoadBalancerTLSCertificateRevocationReason
pattern CertificateHold = LoadBalancerTLSCertificateRevocationReason' "CERTIFICATE_HOLD"

pattern CessationOfOperation :: LoadBalancerTLSCertificateRevocationReason
pattern CessationOfOperation = LoadBalancerTLSCertificateRevocationReason' "CESSATION_OF_OPERATION"

pattern KeyCompromise :: LoadBalancerTLSCertificateRevocationReason
pattern KeyCompromise = LoadBalancerTLSCertificateRevocationReason' "KEY_COMPROMISE"

pattern PrivilegeWithdrawn :: LoadBalancerTLSCertificateRevocationReason
pattern PrivilegeWithdrawn = LoadBalancerTLSCertificateRevocationReason' "PRIVILEGE_WITHDRAWN"

pattern RemoveFromCrl :: LoadBalancerTLSCertificateRevocationReason
pattern RemoveFromCrl = LoadBalancerTLSCertificateRevocationReason' "REMOVE_FROM_CRL"

pattern Superceded :: LoadBalancerTLSCertificateRevocationReason
pattern Superceded = LoadBalancerTLSCertificateRevocationReason' "SUPERCEDED"

pattern Unspecified :: LoadBalancerTLSCertificateRevocationReason
pattern Unspecified = LoadBalancerTLSCertificateRevocationReason' "UNSPECIFIED"

{-# COMPLETE
  AACompromise,
  AffiliationChanged,
  CaCompromise,
  CertificateHold,
  CessationOfOperation,
  KeyCompromise,
  PrivilegeWithdrawn,
  RemoveFromCrl,
  Superceded,
  Unspecified,
  LoadBalancerTLSCertificateRevocationReason' #-}

instance FromText LoadBalancerTLSCertificateRevocationReason where
    parser = (LoadBalancerTLSCertificateRevocationReason' . mk) <$> takeText

instance ToText LoadBalancerTLSCertificateRevocationReason where
    toText (LoadBalancerTLSCertificateRevocationReason' ci) = original ci

-- | Represents an enum of /known/ $LoadBalancerTLSCertificateRevocationReason.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LoadBalancerTLSCertificateRevocationReason where
    toEnum i = case i of
        0 -> AACompromise
        1 -> AffiliationChanged
        2 -> CaCompromise
        3 -> CertificateHold
        4 -> CessationOfOperation
        5 -> KeyCompromise
        6 -> PrivilegeWithdrawn
        7 -> RemoveFromCrl
        8 -> Superceded
        9 -> Unspecified
        _ -> (error . showText) $ "Unknown index for LoadBalancerTLSCertificateRevocationReason: " <> toText i
    fromEnum x = case x of
        AACompromise -> 0
        AffiliationChanged -> 1
        CaCompromise -> 2
        CertificateHold -> 3
        CessationOfOperation -> 4
        KeyCompromise -> 5
        PrivilegeWithdrawn -> 6
        RemoveFromCrl -> 7
        Superceded -> 8
        Unspecified -> 9
        LoadBalancerTLSCertificateRevocationReason' name -> (error . showText) $ "Unknown LoadBalancerTLSCertificateRevocationReason: " <> original name

-- | Represents the bounds of /known/ $LoadBalancerTLSCertificateRevocationReason.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LoadBalancerTLSCertificateRevocationReason where
    minBound = AACompromise
    maxBound = Unspecified

instance Hashable     LoadBalancerTLSCertificateRevocationReason
instance NFData       LoadBalancerTLSCertificateRevocationReason
instance ToByteString LoadBalancerTLSCertificateRevocationReason
instance ToQuery      LoadBalancerTLSCertificateRevocationReason
instance ToHeader     LoadBalancerTLSCertificateRevocationReason

instance FromJSON LoadBalancerTLSCertificateRevocationReason where
    parseJSON = parseJSONText "LoadBalancerTLSCertificateRevocationReason"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceStateChangeReasonCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.InstanceStateChangeReasonCode (
  InstanceStateChangeReasonCode (
    ..
    , ISCRCBootstrapFailure
    , ISCRCClusterTerminated
    , ISCRCInstanceFailure
    , ISCRCInternalError
    , ISCRCValidationError
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data InstanceStateChangeReasonCode = InstanceStateChangeReasonCode' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern ISCRCBootstrapFailure :: InstanceStateChangeReasonCode
pattern ISCRCBootstrapFailure = InstanceStateChangeReasonCode' "BOOTSTRAP_FAILURE"

pattern ISCRCClusterTerminated :: InstanceStateChangeReasonCode
pattern ISCRCClusterTerminated = InstanceStateChangeReasonCode' "CLUSTER_TERMINATED"

pattern ISCRCInstanceFailure :: InstanceStateChangeReasonCode
pattern ISCRCInstanceFailure = InstanceStateChangeReasonCode' "INSTANCE_FAILURE"

pattern ISCRCInternalError :: InstanceStateChangeReasonCode
pattern ISCRCInternalError = InstanceStateChangeReasonCode' "INTERNAL_ERROR"

pattern ISCRCValidationError :: InstanceStateChangeReasonCode
pattern ISCRCValidationError = InstanceStateChangeReasonCode' "VALIDATION_ERROR"

{-# COMPLETE
  ISCRCBootstrapFailure,
  ISCRCClusterTerminated,
  ISCRCInstanceFailure,
  ISCRCInternalError,
  ISCRCValidationError,
  InstanceStateChangeReasonCode' #-}

instance FromText InstanceStateChangeReasonCode where
    parser = (InstanceStateChangeReasonCode' . mk) <$> takeText

instance ToText InstanceStateChangeReasonCode where
    toText (InstanceStateChangeReasonCode' ci) = original ci

-- | Represents an enum of /known/ $InstanceStateChangeReasonCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InstanceStateChangeReasonCode where
    toEnum i = case i of
        0 -> ISCRCBootstrapFailure
        1 -> ISCRCClusterTerminated
        2 -> ISCRCInstanceFailure
        3 -> ISCRCInternalError
        4 -> ISCRCValidationError
        _ -> (error . showText) $ "Unknown index for InstanceStateChangeReasonCode: " <> toText i
    fromEnum x = case x of
        ISCRCBootstrapFailure -> 0
        ISCRCClusterTerminated -> 1
        ISCRCInstanceFailure -> 2
        ISCRCInternalError -> 3
        ISCRCValidationError -> 4
        InstanceStateChangeReasonCode' name -> (error . showText) $ "Unknown InstanceStateChangeReasonCode: " <> original name

-- | Represents the bounds of /known/ $InstanceStateChangeReasonCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InstanceStateChangeReasonCode where
    minBound = ISCRCBootstrapFailure
    maxBound = ISCRCValidationError

instance Hashable     InstanceStateChangeReasonCode
instance NFData       InstanceStateChangeReasonCode
instance ToByteString InstanceStateChangeReasonCode
instance ToQuery      InstanceStateChangeReasonCode
instance ToHeader     InstanceStateChangeReasonCode

instance FromJSON InstanceStateChangeReasonCode where
    parseJSON = parseJSONText "InstanceStateChangeReasonCode"

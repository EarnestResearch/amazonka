{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CancelBatchErrorCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.CancelBatchErrorCode (
  CancelBatchErrorCode (
    ..
    , CBECFleetRequestIdDoesNotExist
    , CBECFleetRequestIdMalformed
    , CBECFleetRequestNotInCancellableState
    , CBECUnexpectedError
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data CancelBatchErrorCode = CancelBatchErrorCode' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern CBECFleetRequestIdDoesNotExist :: CancelBatchErrorCode
pattern CBECFleetRequestIdDoesNotExist = CancelBatchErrorCode' "fleetRequestIdDoesNotExist"

pattern CBECFleetRequestIdMalformed :: CancelBatchErrorCode
pattern CBECFleetRequestIdMalformed = CancelBatchErrorCode' "fleetRequestIdMalformed"

pattern CBECFleetRequestNotInCancellableState :: CancelBatchErrorCode
pattern CBECFleetRequestNotInCancellableState = CancelBatchErrorCode' "fleetRequestNotInCancellableState"

pattern CBECUnexpectedError :: CancelBatchErrorCode
pattern CBECUnexpectedError = CancelBatchErrorCode' "unexpectedError"

{-# COMPLETE
  CBECFleetRequestIdDoesNotExist,
  CBECFleetRequestIdMalformed,
  CBECFleetRequestNotInCancellableState,
  CBECUnexpectedError,
  CancelBatchErrorCode' #-}

instance FromText CancelBatchErrorCode where
    parser = (CancelBatchErrorCode' . mk) <$> takeText

instance ToText CancelBatchErrorCode where
    toText (CancelBatchErrorCode' ci) = original ci

-- | Represents an enum of /known/ $CancelBatchErrorCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CancelBatchErrorCode where
    toEnum i = case i of
        0 -> CBECFleetRequestIdDoesNotExist
        1 -> CBECFleetRequestIdMalformed
        2 -> CBECFleetRequestNotInCancellableState
        3 -> CBECUnexpectedError
        _ -> (error . showText) $ "Unknown index for CancelBatchErrorCode: " <> toText i
    fromEnum x = case x of
        CBECFleetRequestIdDoesNotExist -> 0
        CBECFleetRequestIdMalformed -> 1
        CBECFleetRequestNotInCancellableState -> 2
        CBECUnexpectedError -> 3
        CancelBatchErrorCode' name -> (error . showText) $ "Unknown CancelBatchErrorCode: " <> original name

-- | Represents the bounds of /known/ $CancelBatchErrorCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CancelBatchErrorCode where
    minBound = CBECFleetRequestIdDoesNotExist
    maxBound = CBECUnexpectedError

instance Hashable     CancelBatchErrorCode
instance NFData       CancelBatchErrorCode
instance ToByteString CancelBatchErrorCode
instance ToQuery      CancelBatchErrorCode
instance ToHeader     CancelBatchErrorCode

instance FromXML CancelBatchErrorCode where
    parseXML = parseXMLText "CancelBatchErrorCode"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchReadExceptionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchReadExceptionType (
  BatchReadExceptionType (
    ..
    , AccessDeniedException
    , CannotListParentOfRootException
    , DirectoryNotEnabledException
    , FacetValidationException
    , InternalServiceException
    , InvalidARNException
    , InvalidNextTokenException
    , LimitExceededException
    , NotIndexException
    , NotNodeException
    , NotPolicyException
    , ResourceNotFoundException
    , ValidationException
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data BatchReadExceptionType = BatchReadExceptionType' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern AccessDeniedException :: BatchReadExceptionType
pattern AccessDeniedException = BatchReadExceptionType' "AccessDeniedException"

pattern CannotListParentOfRootException :: BatchReadExceptionType
pattern CannotListParentOfRootException = BatchReadExceptionType' "CannotListParentOfRootException"

pattern DirectoryNotEnabledException :: BatchReadExceptionType
pattern DirectoryNotEnabledException = BatchReadExceptionType' "DirectoryNotEnabledException"

pattern FacetValidationException :: BatchReadExceptionType
pattern FacetValidationException = BatchReadExceptionType' "FacetValidationException"

pattern InternalServiceException :: BatchReadExceptionType
pattern InternalServiceException = BatchReadExceptionType' "InternalServiceException"

pattern InvalidARNException :: BatchReadExceptionType
pattern InvalidARNException = BatchReadExceptionType' "InvalidArnException"

pattern InvalidNextTokenException :: BatchReadExceptionType
pattern InvalidNextTokenException = BatchReadExceptionType' "InvalidNextTokenException"

pattern LimitExceededException :: BatchReadExceptionType
pattern LimitExceededException = BatchReadExceptionType' "LimitExceededException"

pattern NotIndexException :: BatchReadExceptionType
pattern NotIndexException = BatchReadExceptionType' "NotIndexException"

pattern NotNodeException :: BatchReadExceptionType
pattern NotNodeException = BatchReadExceptionType' "NotNodeException"

pattern NotPolicyException :: BatchReadExceptionType
pattern NotPolicyException = BatchReadExceptionType' "NotPolicyException"

pattern ResourceNotFoundException :: BatchReadExceptionType
pattern ResourceNotFoundException = BatchReadExceptionType' "ResourceNotFoundException"

pattern ValidationException :: BatchReadExceptionType
pattern ValidationException = BatchReadExceptionType' "ValidationException"

{-# COMPLETE
  AccessDeniedException,
  CannotListParentOfRootException,
  DirectoryNotEnabledException,
  FacetValidationException,
  InternalServiceException,
  InvalidARNException,
  InvalidNextTokenException,
  LimitExceededException,
  NotIndexException,
  NotNodeException,
  NotPolicyException,
  ResourceNotFoundException,
  ValidationException,
  BatchReadExceptionType' #-}

instance FromText BatchReadExceptionType where
    parser = (BatchReadExceptionType' . mk) <$> takeText

instance ToText BatchReadExceptionType where
    toText (BatchReadExceptionType' ci) = original ci

-- | Represents an enum of /known/ $BatchReadExceptionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BatchReadExceptionType where
    toEnum i = case i of
        0 -> AccessDeniedException
        1 -> CannotListParentOfRootException
        2 -> DirectoryNotEnabledException
        3 -> FacetValidationException
        4 -> InternalServiceException
        5 -> InvalidARNException
        6 -> InvalidNextTokenException
        7 -> LimitExceededException
        8 -> NotIndexException
        9 -> NotNodeException
        10 -> NotPolicyException
        11 -> ResourceNotFoundException
        12 -> ValidationException
        _ -> (error . showText) $ "Unknown index for BatchReadExceptionType: " <> toText i
    fromEnum x = case x of
        AccessDeniedException -> 0
        CannotListParentOfRootException -> 1
        DirectoryNotEnabledException -> 2
        FacetValidationException -> 3
        InternalServiceException -> 4
        InvalidARNException -> 5
        InvalidNextTokenException -> 6
        LimitExceededException -> 7
        NotIndexException -> 8
        NotNodeException -> 9
        NotPolicyException -> 10
        ResourceNotFoundException -> 11
        ValidationException -> 12
        BatchReadExceptionType' name -> (error . showText) $ "Unknown BatchReadExceptionType: " <> original name

-- | Represents the bounds of /known/ $BatchReadExceptionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BatchReadExceptionType where
    minBound = AccessDeniedException
    maxBound = ValidationException

instance Hashable     BatchReadExceptionType
instance NFData       BatchReadExceptionType
instance ToByteString BatchReadExceptionType
instance ToQuery      BatchReadExceptionType
instance ToHeader     BatchReadExceptionType

instance FromJSON BatchReadExceptionType where
    parseJSON = parseJSONText "BatchReadExceptionType"

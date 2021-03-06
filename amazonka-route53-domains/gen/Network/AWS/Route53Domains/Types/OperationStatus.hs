{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.OperationStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53Domains.Types.OperationStatus (
  OperationStatus (
    ..
    , Error'
    , Failed
    , InProgress
    , Submitted
    , Successful
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OperationStatus = OperationStatus' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern Error' :: OperationStatus
pattern Error' = OperationStatus' "ERROR"

pattern Failed :: OperationStatus
pattern Failed = OperationStatus' "FAILED"

pattern InProgress :: OperationStatus
pattern InProgress = OperationStatus' "IN_PROGRESS"

pattern Submitted :: OperationStatus
pattern Submitted = OperationStatus' "SUBMITTED"

pattern Successful :: OperationStatus
pattern Successful = OperationStatus' "SUCCESSFUL"

{-# COMPLETE
  Error',
  Failed,
  InProgress,
  Submitted,
  Successful,
  OperationStatus' #-}

instance FromText OperationStatus where
    parser = (OperationStatus' . mk) <$> takeText

instance ToText OperationStatus where
    toText (OperationStatus' ci) = original ci

-- | Represents an enum of /known/ $OperationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OperationStatus where
    toEnum i = case i of
        0 -> Error'
        1 -> Failed
        2 -> InProgress
        3 -> Submitted
        4 -> Successful
        _ -> (error . showText) $ "Unknown index for OperationStatus: " <> toText i
    fromEnum x = case x of
        Error' -> 0
        Failed -> 1
        InProgress -> 2
        Submitted -> 3
        Successful -> 4
        OperationStatus' name -> (error . showText) $ "Unknown OperationStatus: " <> original name

-- | Represents the bounds of /known/ $OperationStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OperationStatus where
    minBound = Error'
    maxBound = Successful

instance Hashable     OperationStatus
instance NFData       OperationStatus
instance ToByteString OperationStatus
instance ToQuery      OperationStatus
instance ToHeader     OperationStatus

instance FromJSON OperationStatus where
    parseJSON = parseJSONText "OperationStatus"

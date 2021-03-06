{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ProvisionedProductPlanStatus (
  ProvisionedProductPlanStatus (
    ..
    , CreateFailed
    , CreateInProgress
    , CreateSuccess
    , ExecuteFailed
    , ExecuteInProgress
    , ExecuteSuccess
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ProvisionedProductPlanStatus = ProvisionedProductPlanStatus' (CI
                                                                     Text)
                                      deriving (Eq, Ord, Read, Show, Data,
                                                Typeable, Generic)

pattern CreateFailed :: ProvisionedProductPlanStatus
pattern CreateFailed = ProvisionedProductPlanStatus' "CREATE_FAILED"

pattern CreateInProgress :: ProvisionedProductPlanStatus
pattern CreateInProgress = ProvisionedProductPlanStatus' "CREATE_IN_PROGRESS"

pattern CreateSuccess :: ProvisionedProductPlanStatus
pattern CreateSuccess = ProvisionedProductPlanStatus' "CREATE_SUCCESS"

pattern ExecuteFailed :: ProvisionedProductPlanStatus
pattern ExecuteFailed = ProvisionedProductPlanStatus' "EXECUTE_FAILED"

pattern ExecuteInProgress :: ProvisionedProductPlanStatus
pattern ExecuteInProgress = ProvisionedProductPlanStatus' "EXECUTE_IN_PROGRESS"

pattern ExecuteSuccess :: ProvisionedProductPlanStatus
pattern ExecuteSuccess = ProvisionedProductPlanStatus' "EXECUTE_SUCCESS"

{-# COMPLETE
  CreateFailed,
  CreateInProgress,
  CreateSuccess,
  ExecuteFailed,
  ExecuteInProgress,
  ExecuteSuccess,
  ProvisionedProductPlanStatus' #-}

instance FromText ProvisionedProductPlanStatus where
    parser = (ProvisionedProductPlanStatus' . mk) <$> takeText

instance ToText ProvisionedProductPlanStatus where
    toText (ProvisionedProductPlanStatus' ci) = original ci

-- | Represents an enum of /known/ $ProvisionedProductPlanStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProvisionedProductPlanStatus where
    toEnum i = case i of
        0 -> CreateFailed
        1 -> CreateInProgress
        2 -> CreateSuccess
        3 -> ExecuteFailed
        4 -> ExecuteInProgress
        5 -> ExecuteSuccess
        _ -> (error . showText) $ "Unknown index for ProvisionedProductPlanStatus: " <> toText i
    fromEnum x = case x of
        CreateFailed -> 0
        CreateInProgress -> 1
        CreateSuccess -> 2
        ExecuteFailed -> 3
        ExecuteInProgress -> 4
        ExecuteSuccess -> 5
        ProvisionedProductPlanStatus' name -> (error . showText) $ "Unknown ProvisionedProductPlanStatus: " <> original name

-- | Represents the bounds of /known/ $ProvisionedProductPlanStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProvisionedProductPlanStatus where
    minBound = CreateFailed
    maxBound = ExecuteSuccess

instance Hashable     ProvisionedProductPlanStatus
instance NFData       ProvisionedProductPlanStatus
instance ToByteString ProvisionedProductPlanStatus
instance ToQuery      ProvisionedProductPlanStatus
instance ToHeader     ProvisionedProductPlanStatus

instance FromJSON ProvisionedProductPlanStatus where
    parseJSON = parseJSONText "ProvisionedProductPlanStatus"

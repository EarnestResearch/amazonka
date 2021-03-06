{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.NotifyWorkersFailureCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MechanicalTurk.Types.NotifyWorkersFailureCode (
  NotifyWorkersFailureCode (
    ..
    , HardFailure
    , SoftFailure
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data NotifyWorkersFailureCode = NotifyWorkersFailureCode' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern HardFailure :: NotifyWorkersFailureCode
pattern HardFailure = NotifyWorkersFailureCode' "HardFailure"

pattern SoftFailure :: NotifyWorkersFailureCode
pattern SoftFailure = NotifyWorkersFailureCode' "SoftFailure"

{-# COMPLETE
  HardFailure,
  SoftFailure,
  NotifyWorkersFailureCode' #-}

instance FromText NotifyWorkersFailureCode where
    parser = (NotifyWorkersFailureCode' . mk) <$> takeText

instance ToText NotifyWorkersFailureCode where
    toText (NotifyWorkersFailureCode' ci) = original ci

-- | Represents an enum of /known/ $NotifyWorkersFailureCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum NotifyWorkersFailureCode where
    toEnum i = case i of
        0 -> HardFailure
        1 -> SoftFailure
        _ -> (error . showText) $ "Unknown index for NotifyWorkersFailureCode: " <> toText i
    fromEnum x = case x of
        HardFailure -> 0
        SoftFailure -> 1
        NotifyWorkersFailureCode' name -> (error . showText) $ "Unknown NotifyWorkersFailureCode: " <> original name

-- | Represents the bounds of /known/ $NotifyWorkersFailureCode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded NotifyWorkersFailureCode where
    minBound = HardFailure
    maxBound = SoftFailure

instance Hashable     NotifyWorkersFailureCode
instance NFData       NotifyWorkersFailureCode
instance ToByteString NotifyWorkersFailureCode
instance ToQuery      NotifyWorkersFailureCode
instance ToHeader     NotifyWorkersFailureCode

instance FromJSON NotifyWorkersFailureCode where
    parseJSON = parseJSONText "NotifyWorkersFailureCode"

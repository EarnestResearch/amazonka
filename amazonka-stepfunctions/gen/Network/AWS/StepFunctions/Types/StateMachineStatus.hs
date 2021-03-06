{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.StateMachineStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.StateMachineStatus (
  StateMachineStatus (
    ..
    , Active
    , Deleting
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StateMachineStatus = StateMachineStatus' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Active :: StateMachineStatus
pattern Active = StateMachineStatus' "ACTIVE"

pattern Deleting :: StateMachineStatus
pattern Deleting = StateMachineStatus' "DELETING"

{-# COMPLETE
  Active,
  Deleting,
  StateMachineStatus' #-}

instance FromText StateMachineStatus where
    parser = (StateMachineStatus' . mk) <$> takeText

instance ToText StateMachineStatus where
    toText (StateMachineStatus' ci) = original ci

-- | Represents an enum of /known/ $StateMachineStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StateMachineStatus where
    toEnum i = case i of
        0 -> Active
        1 -> Deleting
        _ -> (error . showText) $ "Unknown index for StateMachineStatus: " <> toText i
    fromEnum x = case x of
        Active -> 0
        Deleting -> 1
        StateMachineStatus' name -> (error . showText) $ "Unknown StateMachineStatus: " <> original name

-- | Represents the bounds of /known/ $StateMachineStatus.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StateMachineStatus where
    minBound = Active
    maxBound = Deleting

instance Hashable     StateMachineStatus
instance NFData       StateMachineStatus
instance ToByteString StateMachineStatus
instance ToQuery      StateMachineStatus
instance ToHeader     StateMachineStatus

instance FromJSON StateMachineStatus where
    parseJSON = parseJSONText "StateMachineStatus"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.StepCancellationOption
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.StepCancellationOption (
  StepCancellationOption (
    ..
    , SendInterrupt
    , TerminateProcess
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StepCancellationOption = StepCancellationOption' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern SendInterrupt :: StepCancellationOption
pattern SendInterrupt = StepCancellationOption' "SEND_INTERRUPT"

pattern TerminateProcess :: StepCancellationOption
pattern TerminateProcess = StepCancellationOption' "TERMINATE_PROCESS"

{-# COMPLETE
  SendInterrupt,
  TerminateProcess,
  StepCancellationOption' #-}

instance FromText StepCancellationOption where
    parser = (StepCancellationOption' . mk) <$> takeText

instance ToText StepCancellationOption where
    toText (StepCancellationOption' ci) = original ci

-- | Represents an enum of /known/ $StepCancellationOption.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StepCancellationOption where
    toEnum i = case i of
        0 -> SendInterrupt
        1 -> TerminateProcess
        _ -> (error . showText) $ "Unknown index for StepCancellationOption: " <> toText i
    fromEnum x = case x of
        SendInterrupt -> 0
        TerminateProcess -> 1
        StepCancellationOption' name -> (error . showText) $ "Unknown StepCancellationOption: " <> original name

-- | Represents the bounds of /known/ $StepCancellationOption.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StepCancellationOption where
    minBound = SendInterrupt
    maxBound = TerminateProcess

instance Hashable     StepCancellationOption
instance NFData       StepCancellationOption
instance ToByteString StepCancellationOption
instance ToQuery      StepCancellationOption
instance ToHeader     StepCancellationOption

instance ToJSON StepCancellationOption where
    toJSON = toJSONText

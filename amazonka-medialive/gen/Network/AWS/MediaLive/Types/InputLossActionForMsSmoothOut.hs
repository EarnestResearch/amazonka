{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputLossActionForMsSmoothOut
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputLossActionForMsSmoothOut (
  InputLossActionForMsSmoothOut (
    ..
    , EmitOutput
    , PauseOutput
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Input Loss Action For Ms Smooth Out
data InputLossActionForMsSmoothOut = InputLossActionForMsSmoothOut' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern EmitOutput :: InputLossActionForMsSmoothOut
pattern EmitOutput = InputLossActionForMsSmoothOut' "EMIT_OUTPUT"

pattern PauseOutput :: InputLossActionForMsSmoothOut
pattern PauseOutput = InputLossActionForMsSmoothOut' "PAUSE_OUTPUT"

{-# COMPLETE
  EmitOutput,
  PauseOutput,
  InputLossActionForMsSmoothOut' #-}

instance FromText InputLossActionForMsSmoothOut where
    parser = (InputLossActionForMsSmoothOut' . mk) <$> takeText

instance ToText InputLossActionForMsSmoothOut where
    toText (InputLossActionForMsSmoothOut' ci) = original ci

-- | Represents an enum of /known/ $InputLossActionForMsSmoothOut.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InputLossActionForMsSmoothOut where
    toEnum i = case i of
        0 -> EmitOutput
        1 -> PauseOutput
        _ -> (error . showText) $ "Unknown index for InputLossActionForMsSmoothOut: " <> toText i
    fromEnum x = case x of
        EmitOutput -> 0
        PauseOutput -> 1
        InputLossActionForMsSmoothOut' name -> (error . showText) $ "Unknown InputLossActionForMsSmoothOut: " <> original name

-- | Represents the bounds of /known/ $InputLossActionForMsSmoothOut.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InputLossActionForMsSmoothOut where
    minBound = EmitOutput
    maxBound = PauseOutput

instance Hashable     InputLossActionForMsSmoothOut
instance NFData       InputLossActionForMsSmoothOut
instance ToByteString InputLossActionForMsSmoothOut
instance ToQuery      InputLossActionForMsSmoothOut
instance ToHeader     InputLossActionForMsSmoothOut

instance ToJSON InputLossActionForMsSmoothOut where
    toJSON = toJSONText

instance FromJSON InputLossActionForMsSmoothOut where
    parseJSON = parseJSONText "InputLossActionForMsSmoothOut"

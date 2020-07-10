{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputLossActionForRtmpOut
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputLossActionForRtmpOut (
  InputLossActionForRtmpOut (
    ..
    , ILAFROEmitOutput
    , ILAFROPauseOutput
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Input Loss Action For Rtmp Out
data InputLossActionForRtmpOut = InputLossActionForRtmpOut' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern ILAFROEmitOutput :: InputLossActionForRtmpOut
pattern ILAFROEmitOutput = InputLossActionForRtmpOut' "EMIT_OUTPUT"

pattern ILAFROPauseOutput :: InputLossActionForRtmpOut
pattern ILAFROPauseOutput = InputLossActionForRtmpOut' "PAUSE_OUTPUT"

{-# COMPLETE
  ILAFROEmitOutput,
  ILAFROPauseOutput,
  InputLossActionForRtmpOut' #-}

instance FromText InputLossActionForRtmpOut where
    parser = (InputLossActionForRtmpOut' . mk) <$> takeText

instance ToText InputLossActionForRtmpOut where
    toText (InputLossActionForRtmpOut' ci) = original ci

-- | Represents an enum of /known/ $InputLossActionForRtmpOut.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InputLossActionForRtmpOut where
    toEnum i = case i of
        0 -> ILAFROEmitOutput
        1 -> ILAFROPauseOutput
        _ -> (error . showText) $ "Unknown index for InputLossActionForRtmpOut: " <> toText i
    fromEnum x = case x of
        ILAFROEmitOutput -> 0
        ILAFROPauseOutput -> 1
        InputLossActionForRtmpOut' name -> (error . showText) $ "Unknown InputLossActionForRtmpOut: " <> original name

-- | Represents the bounds of /known/ $InputLossActionForRtmpOut.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InputLossActionForRtmpOut where
    minBound = ILAFROEmitOutput
    maxBound = ILAFROPauseOutput

instance Hashable     InputLossActionForRtmpOut
instance NFData       InputLossActionForRtmpOut
instance ToByteString InputLossActionForRtmpOut
instance ToQuery      InputLossActionForRtmpOut
instance ToHeader     InputLossActionForRtmpOut

instance ToJSON InputLossActionForRtmpOut where
    toJSON = toJSONText

instance FromJSON InputLossActionForRtmpOut where
    parseJSON = parseJSONText "InputLossActionForRtmpOut"

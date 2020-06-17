{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputLossActionForUdpOut
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputLossActionForUdpOut (
  InputLossActionForUdpOut (
    ..
    , DropProgram
    , DropTs
    , EmitProgram
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Placeholder documentation for InputLossActionForUdpOut
data InputLossActionForUdpOut = InputLossActionForUdpOut' (CI
                                                             Text)
                                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                                            Generic)

pattern DropProgram :: InputLossActionForUdpOut
pattern DropProgram = InputLossActionForUdpOut' "DROP_PROGRAM"

pattern DropTs :: InputLossActionForUdpOut
pattern DropTs = InputLossActionForUdpOut' "DROP_TS"

pattern EmitProgram :: InputLossActionForUdpOut
pattern EmitProgram = InputLossActionForUdpOut' "EMIT_PROGRAM"

{-# COMPLETE
  DropProgram,
  DropTs,
  EmitProgram,
  InputLossActionForUdpOut' #-}

instance FromText InputLossActionForUdpOut where
    parser = (InputLossActionForUdpOut' . mk) <$> takeText

instance ToText InputLossActionForUdpOut where
    toText (InputLossActionForUdpOut' ci) = original ci

-- | Represents an enum of /known/ $InputLossActionForUdpOut.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InputLossActionForUdpOut where
    toEnum i = case i of
        0 -> DropProgram
        1 -> DropTs
        2 -> EmitProgram
        _ -> (error . showText) $ "Unknown index for InputLossActionForUdpOut: " <> toText i
    fromEnum x = case x of
        DropProgram -> 0
        DropTs -> 1
        EmitProgram -> 2
        InputLossActionForUdpOut' name -> (error . showText) $ "Unknown InputLossActionForUdpOut: " <> original name

-- | Represents the bounds of /known/ $InputLossActionForUdpOut.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InputLossActionForUdpOut where
    minBound = DropProgram
    maxBound = EmitProgram

instance Hashable     InputLossActionForUdpOut
instance NFData       InputLossActionForUdpOut
instance ToByteString InputLossActionForUdpOut
instance ToQuery      InputLossActionForUdpOut
instance ToHeader     InputLossActionForUdpOut

instance ToJSON InputLossActionForUdpOut where
    toJSON = toJSONText

instance FromJSON InputLossActionForUdpOut where
    parseJSON = parseJSONText "InputLossActionForUdpOut"

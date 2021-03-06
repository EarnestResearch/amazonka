{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Av1FramerateControl
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Av1FramerateControl (
  Av1FramerateControl (
    ..
    , InitializeFromSource
    , Specified
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | If you are using the console, use the Framerate setting to specify the frame rate for this output. If you want to keep the same frame rate as the input video, choose Follow source. If you want to do frame rate conversion, choose a frame rate from the dropdown list or choose Custom. The framerates shown in the dropdown list are decimal approximations of fractions. If you choose Custom, specify your frame rate as a fraction. If you are creating your transcoding job specification as a JSON file without the console, use FramerateControl to specify which value the service uses for the frame rate for this output. Choose INITIALIZE_FROM_SOURCE if you want the service to use the frame rate from the input. Choose SPECIFIED if you want the service to use the frame rate you specify in the settings FramerateNumerator and FramerateDenominator.
data Av1FramerateControl = Av1FramerateControl' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern InitializeFromSource :: Av1FramerateControl
pattern InitializeFromSource = Av1FramerateControl' "INITIALIZE_FROM_SOURCE"

pattern Specified :: Av1FramerateControl
pattern Specified = Av1FramerateControl' "SPECIFIED"

{-# COMPLETE
  InitializeFromSource,
  Specified,
  Av1FramerateControl' #-}

instance FromText Av1FramerateControl where
    parser = (Av1FramerateControl' . mk) <$> takeText

instance ToText Av1FramerateControl where
    toText (Av1FramerateControl' ci) = original ci

-- | Represents an enum of /known/ $Av1FramerateControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Av1FramerateControl where
    toEnum i = case i of
        0 -> InitializeFromSource
        1 -> Specified
        _ -> (error . showText) $ "Unknown index for Av1FramerateControl: " <> toText i
    fromEnum x = case x of
        InitializeFromSource -> 0
        Specified -> 1
        Av1FramerateControl' name -> (error . showText) $ "Unknown Av1FramerateControl: " <> original name

-- | Represents the bounds of /known/ $Av1FramerateControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Av1FramerateControl where
    minBound = InitializeFromSource
    maxBound = Specified

instance Hashable     Av1FramerateControl
instance NFData       Av1FramerateControl
instance ToByteString Av1FramerateControl
instance ToQuery      Av1FramerateControl
instance ToHeader     Av1FramerateControl

instance ToJSON Av1FramerateControl where
    toJSON = toJSONText

instance FromJSON Av1FramerateControl where
    parseJSON = parseJSONText "Av1FramerateControl"

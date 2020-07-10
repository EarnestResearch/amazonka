{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputClass
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputClass (
  InputClass (
    ..
    , ICSinglePipeline
    , ICStandard
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | A standard input has two sources and a single pipeline input only has one.
data InputClass = InputClass' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern ICSinglePipeline :: InputClass
pattern ICSinglePipeline = InputClass' "SINGLE_PIPELINE"

pattern ICStandard :: InputClass
pattern ICStandard = InputClass' "STANDARD"

{-# COMPLETE
  ICSinglePipeline,
  ICStandard,
  InputClass' #-}

instance FromText InputClass where
    parser = (InputClass' . mk) <$> takeText

instance ToText InputClass where
    toText (InputClass' ci) = original ci

-- | Represents an enum of /known/ $InputClass.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InputClass where
    toEnum i = case i of
        0 -> ICSinglePipeline
        1 -> ICStandard
        _ -> (error . showText) $ "Unknown index for InputClass: " <> toText i
    fromEnum x = case x of
        ICSinglePipeline -> 0
        ICStandard -> 1
        InputClass' name -> (error . showText) $ "Unknown InputClass: " <> original name

-- | Represents the bounds of /known/ $InputClass.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InputClass where
    minBound = ICSinglePipeline
    maxBound = ICStandard

instance Hashable     InputClass
instance NFData       InputClass
instance ToByteString InputClass
instance ToQuery      InputClass
instance ToHeader     InputClass

instance FromJSON InputClass where
    parseJSON = parseJSONText "InputClass"

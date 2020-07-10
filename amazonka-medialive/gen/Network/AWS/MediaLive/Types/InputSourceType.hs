{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputSourceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputSourceType (
  InputSourceType (
    ..
    , ISTDynamic
    , ISTStatic
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | There are two types of input sources, static and dynamic. If an input source is dynamic you can
--
-- change the source url of the input dynamically using an input switch action. However, the only input type
-- to support a dynamic url at this time is MP4_FILE. By default all input sources are static.
data InputSourceType = InputSourceType' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern ISTDynamic :: InputSourceType
pattern ISTDynamic = InputSourceType' "DYNAMIC"

pattern ISTStatic :: InputSourceType
pattern ISTStatic = InputSourceType' "STATIC"

{-# COMPLETE
  ISTDynamic,
  ISTStatic,
  InputSourceType' #-}

instance FromText InputSourceType where
    parser = (InputSourceType' . mk) <$> takeText

instance ToText InputSourceType where
    toText (InputSourceType' ci) = original ci

-- | Represents an enum of /known/ $InputSourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum InputSourceType where
    toEnum i = case i of
        0 -> ISTDynamic
        1 -> ISTStatic
        _ -> (error . showText) $ "Unknown index for InputSourceType: " <> toText i
    fromEnum x = case x of
        ISTDynamic -> 0
        ISTStatic -> 1
        InputSourceType' name -> (error . showText) $ "Unknown InputSourceType: " <> original name

-- | Represents the bounds of /known/ $InputSourceType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded InputSourceType where
    minBound = ISTDynamic
    maxBound = ISTStatic

instance Hashable     InputSourceType
instance NFData       InputSourceType
instance ToByteString InputSourceType
instance ToQuery      InputSourceType
instance ToHeader     InputSourceType

instance FromJSON InputSourceType where
    parseJSON = parseJSONText "InputSourceType"

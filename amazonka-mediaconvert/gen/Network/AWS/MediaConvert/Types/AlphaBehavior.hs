{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AlphaBehavior
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AlphaBehavior (
  AlphaBehavior (
    ..
    , Discard
    , RemapToLuma
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Ignore this setting unless this input is a QuickTime animation with an alpha channel. Use this setting to create separate Key and Fill outputs. In each output, specify which part of the input MediaConvert uses. Leave this setting at the default value DISCARD to delete the alpha channel and preserve the video. Set it to REMAP_TO_LUMA to delete the video and map the alpha channel to the luma channel of your outputs.
data AlphaBehavior = AlphaBehavior' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Discard :: AlphaBehavior
pattern Discard = AlphaBehavior' "DISCARD"

pattern RemapToLuma :: AlphaBehavior
pattern RemapToLuma = AlphaBehavior' "REMAP_TO_LUMA"

{-# COMPLETE
  Discard,
  RemapToLuma,
  AlphaBehavior' #-}

instance FromText AlphaBehavior where
    parser = (AlphaBehavior' . mk) <$> takeText

instance ToText AlphaBehavior where
    toText (AlphaBehavior' ci) = original ci

-- | Represents an enum of /known/ $AlphaBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AlphaBehavior where
    toEnum i = case i of
        0 -> Discard
        1 -> RemapToLuma
        _ -> (error . showText) $ "Unknown index for AlphaBehavior: " <> toText i
    fromEnum x = case x of
        Discard -> 0
        RemapToLuma -> 1
        AlphaBehavior' name -> (error . showText) $ "Unknown AlphaBehavior: " <> original name

-- | Represents the bounds of /known/ $AlphaBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AlphaBehavior where
    minBound = Discard
    maxBound = RemapToLuma

instance Hashable     AlphaBehavior
instance NFData       AlphaBehavior
instance ToByteString AlphaBehavior
instance ToQuery      AlphaBehavior
instance ToHeader     AlphaBehavior

instance ToJSON AlphaBehavior where
    toJSON = toJSONText

instance FromJSON AlphaBehavior where
    parseJSON = parseJSONText "AlphaBehavior"

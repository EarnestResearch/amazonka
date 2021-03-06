{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsBufferModel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.M2tsBufferModel (
  M2tsBufferModel (
    ..
    , MBMMultiplex
    , MBMNone
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Controls what buffer model to use for accurate interleaving. If set to MULTIPLEX, use multiplex  buffer model. If set to NONE, this can lead to lower latency, but low-memory devices may not be able to play back the stream without interruptions.
data M2tsBufferModel = M2tsBufferModel' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern MBMMultiplex :: M2tsBufferModel
pattern MBMMultiplex = M2tsBufferModel' "MULTIPLEX"

pattern MBMNone :: M2tsBufferModel
pattern MBMNone = M2tsBufferModel' "NONE"

{-# COMPLETE
  MBMMultiplex,
  MBMNone,
  M2tsBufferModel' #-}

instance FromText M2tsBufferModel where
    parser = (M2tsBufferModel' . mk) <$> takeText

instance ToText M2tsBufferModel where
    toText (M2tsBufferModel' ci) = original ci

-- | Represents an enum of /known/ $M2tsBufferModel.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum M2tsBufferModel where
    toEnum i = case i of
        0 -> MBMMultiplex
        1 -> MBMNone
        _ -> (error . showText) $ "Unknown index for M2tsBufferModel: " <> toText i
    fromEnum x = case x of
        MBMMultiplex -> 0
        MBMNone -> 1
        M2tsBufferModel' name -> (error . showText) $ "Unknown M2tsBufferModel: " <> original name

-- | Represents the bounds of /known/ $M2tsBufferModel.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded M2tsBufferModel where
    minBound = MBMMultiplex
    maxBound = MBMNone

instance Hashable     M2tsBufferModel
instance NFData       M2tsBufferModel
instance ToByteString M2tsBufferModel
instance ToQuery      M2tsBufferModel
instance ToHeader     M2tsBufferModel

instance ToJSON M2tsBufferModel where
    toJSON = toJSONText

instance FromJSON M2tsBufferModel where
    parseJSON = parseJSONText "M2tsBufferModel"

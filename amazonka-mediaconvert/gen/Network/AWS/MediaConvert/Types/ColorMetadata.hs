{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ColorMetadata
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.ColorMetadata (
  ColorMetadata (
    ..
    , Ignore
    , Insert
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Choose Insert (INSERT) for this setting to include color metadata in this output. Choose Ignore (IGNORE) to exclude color metadata from this output. If you don't specify a value, the service sets this to Insert by default.
data ColorMetadata = ColorMetadata' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Ignore :: ColorMetadata
pattern Ignore = ColorMetadata' "IGNORE"

pattern Insert :: ColorMetadata
pattern Insert = ColorMetadata' "INSERT"

{-# COMPLETE
  Ignore,
  Insert,
  ColorMetadata' #-}

instance FromText ColorMetadata where
    parser = (ColorMetadata' . mk) <$> takeText

instance ToText ColorMetadata where
    toText (ColorMetadata' ci) = original ci

-- | Represents an enum of /known/ $ColorMetadata.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ColorMetadata where
    toEnum i = case i of
        0 -> Ignore
        1 -> Insert
        _ -> (error . showText) $ "Unknown index for ColorMetadata: " <> toText i
    fromEnum x = case x of
        Ignore -> 0
        Insert -> 1
        ColorMetadata' name -> (error . showText) $ "Unknown ColorMetadata: " <> original name

-- | Represents the bounds of /known/ $ColorMetadata.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ColorMetadata where
    minBound = Ignore
    maxBound = Insert

instance Hashable     ColorMetadata
instance NFData       ColorMetadata
instance ToByteString ColorMetadata
instance ToQuery      ColorMetadata
instance ToHeader     ColorMetadata

instance ToJSON ColorMetadata where
    toJSON = toJSONText

instance FromJSON ColorMetadata where
    parseJSON = parseJSONText "ColorMetadata"

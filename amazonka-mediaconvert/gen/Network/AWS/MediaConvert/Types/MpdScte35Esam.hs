{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MpdScte35Esam
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.MpdScte35Esam (
  MpdScte35Esam (
    ..
    , MSEInsert
    , MSENone
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Use this setting only when you specify SCTE-35 markers from ESAM. Choose INSERT to put SCTE-35 markers in this output at the insertion points that you specify in an ESAM XML document. Provide the document in the setting SCC XML (sccXml).
data MpdScte35Esam = MpdScte35Esam' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern MSEInsert :: MpdScte35Esam
pattern MSEInsert = MpdScte35Esam' "INSERT"

pattern MSENone :: MpdScte35Esam
pattern MSENone = MpdScte35Esam' "NONE"

{-# COMPLETE
  MSEInsert,
  MSENone,
  MpdScte35Esam' #-}

instance FromText MpdScte35Esam where
    parser = (MpdScte35Esam' . mk) <$> takeText

instance ToText MpdScte35Esam where
    toText (MpdScte35Esam' ci) = original ci

-- | Represents an enum of /known/ $MpdScte35Esam.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MpdScte35Esam where
    toEnum i = case i of
        0 -> MSEInsert
        1 -> MSENone
        _ -> (error . showText) $ "Unknown index for MpdScte35Esam: " <> toText i
    fromEnum x = case x of
        MSEInsert -> 0
        MSENone -> 1
        MpdScte35Esam' name -> (error . showText) $ "Unknown MpdScte35Esam: " <> original name

-- | Represents the bounds of /known/ $MpdScte35Esam.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MpdScte35Esam where
    minBound = MSEInsert
    maxBound = MSENone

instance Hashable     MpdScte35Esam
instance NFData       MpdScte35Esam
instance ToByteString MpdScte35Esam
instance ToQuery      MpdScte35Esam
instance ToHeader     MpdScte35Esam

instance ToJSON MpdScte35Esam where
    toJSON = toJSONText

instance FromJSON MpdScte35Esam where
    parseJSON = parseJSONText "MpdScte35Esam"

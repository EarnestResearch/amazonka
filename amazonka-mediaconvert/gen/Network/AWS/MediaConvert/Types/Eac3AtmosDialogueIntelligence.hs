{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosDialogueIntelligence
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Eac3AtmosDialogueIntelligence (
  Eac3AtmosDialogueIntelligence (
    ..
    , EADIDisabled
    , EADIEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Enable Dolby Dialogue Intelligence to adjust loudness based on dialogue analysis.
data Eac3AtmosDialogueIntelligence = Eac3AtmosDialogueIntelligence' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern EADIDisabled :: Eac3AtmosDialogueIntelligence
pattern EADIDisabled = Eac3AtmosDialogueIntelligence' "DISABLED"

pattern EADIEnabled :: Eac3AtmosDialogueIntelligence
pattern EADIEnabled = Eac3AtmosDialogueIntelligence' "ENABLED"

{-# COMPLETE
  EADIDisabled,
  EADIEnabled,
  Eac3AtmosDialogueIntelligence' #-}

instance FromText Eac3AtmosDialogueIntelligence where
    parser = (Eac3AtmosDialogueIntelligence' . mk) <$> takeText

instance ToText Eac3AtmosDialogueIntelligence where
    toText (Eac3AtmosDialogueIntelligence' ci) = original ci

-- | Represents an enum of /known/ $Eac3AtmosDialogueIntelligence.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Eac3AtmosDialogueIntelligence where
    toEnum i = case i of
        0 -> EADIDisabled
        1 -> EADIEnabled
        _ -> (error . showText) $ "Unknown index for Eac3AtmosDialogueIntelligence: " <> toText i
    fromEnum x = case x of
        EADIDisabled -> 0
        EADIEnabled -> 1
        Eac3AtmosDialogueIntelligence' name -> (error . showText) $ "Unknown Eac3AtmosDialogueIntelligence: " <> original name

-- | Represents the bounds of /known/ $Eac3AtmosDialogueIntelligence.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Eac3AtmosDialogueIntelligence where
    minBound = EADIDisabled
    maxBound = EADIEnabled

instance Hashable     Eac3AtmosDialogueIntelligence
instance NFData       Eac3AtmosDialogueIntelligence
instance ToByteString Eac3AtmosDialogueIntelligence
instance ToQuery      Eac3AtmosDialogueIntelligence
instance ToHeader     Eac3AtmosDialogueIntelligence

instance ToJSON Eac3AtmosDialogueIntelligence where
    toJSON = toJSONText

instance FromJSON Eac3AtmosDialogueIntelligence where
    parseJSON = parseJSONText "Eac3AtmosDialogueIntelligence"

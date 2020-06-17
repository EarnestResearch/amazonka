{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.CEState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.CEState (
  CEState (
    ..
    , Disabled
    , Enabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CEState = CEState' (CI Text)
                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                           Generic)

pattern Disabled :: CEState
pattern Disabled = CEState' "DISABLED"

pattern Enabled :: CEState
pattern Enabled = CEState' "ENABLED"

{-# COMPLETE
  Disabled,
  Enabled,
  CEState' #-}

instance FromText CEState where
    parser = (CEState' . mk) <$> takeText

instance ToText CEState where
    toText (CEState' ci) = original ci

-- | Represents an enum of /known/ $CEState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CEState where
    toEnum i = case i of
        0 -> Disabled
        1 -> Enabled
        _ -> (error . showText) $ "Unknown index for CEState: " <> toText i
    fromEnum x = case x of
        Disabled -> 0
        Enabled -> 1
        CEState' name -> (error . showText) $ "Unknown CEState: " <> original name

-- | Represents the bounds of /known/ $CEState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CEState where
    minBound = Disabled
    maxBound = Enabled

instance Hashable     CEState
instance NFData       CEState
instance ToByteString CEState
instance ToQuery      CEState
instance ToHeader     CEState

instance ToJSON CEState where
    toJSON = toJSONText

instance FromJSON CEState where
    parseJSON = parseJSONText "CEState"

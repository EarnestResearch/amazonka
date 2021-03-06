{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.AutoRenew
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types.AutoRenew (
  AutoRenew (
    ..
    , Disabled
    , Enabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AutoRenew = AutoRenew' (CI Text)
                   deriving (Eq, Ord, Read, Show, Data, Typeable,
                             Generic)

pattern Disabled :: AutoRenew
pattern Disabled = AutoRenew' "DISABLED"

pattern Enabled :: AutoRenew
pattern Enabled = AutoRenew' "ENABLED"

{-# COMPLETE
  Disabled,
  Enabled,
  AutoRenew' #-}

instance FromText AutoRenew where
    parser = (AutoRenew' . mk) <$> takeText

instance ToText AutoRenew where
    toText (AutoRenew' ci) = original ci

-- | Represents an enum of /known/ $AutoRenew.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AutoRenew where
    toEnum i = case i of
        0 -> Disabled
        1 -> Enabled
        _ -> (error . showText) $ "Unknown index for AutoRenew: " <> toText i
    fromEnum x = case x of
        Disabled -> 0
        Enabled -> 1
        AutoRenew' name -> (error . showText) $ "Unknown AutoRenew: " <> original name

-- | Represents the bounds of /known/ $AutoRenew.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AutoRenew where
    minBound = Disabled
    maxBound = Enabled

instance Hashable     AutoRenew
instance NFData       AutoRenew
instance ToByteString AutoRenew
instance ToQuery      AutoRenew
instance ToHeader     AutoRenew

instance ToJSON AutoRenew where
    toJSON = toJSONText

instance FromJSON AutoRenew where
    parseJSON = parseJSONText "AutoRenew"

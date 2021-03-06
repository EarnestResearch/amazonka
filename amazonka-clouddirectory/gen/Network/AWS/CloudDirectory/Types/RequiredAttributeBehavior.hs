{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.RequiredAttributeBehavior
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.RequiredAttributeBehavior (
  RequiredAttributeBehavior (
    ..
    , NotRequired
    , RequiredAlways
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RequiredAttributeBehavior = RequiredAttributeBehavior' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern NotRequired :: RequiredAttributeBehavior
pattern NotRequired = RequiredAttributeBehavior' "NOT_REQUIRED"

pattern RequiredAlways :: RequiredAttributeBehavior
pattern RequiredAlways = RequiredAttributeBehavior' "REQUIRED_ALWAYS"

{-# COMPLETE
  NotRequired,
  RequiredAlways,
  RequiredAttributeBehavior' #-}

instance FromText RequiredAttributeBehavior where
    parser = (RequiredAttributeBehavior' . mk) <$> takeText

instance ToText RequiredAttributeBehavior where
    toText (RequiredAttributeBehavior' ci) = original ci

-- | Represents an enum of /known/ $RequiredAttributeBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RequiredAttributeBehavior where
    toEnum i = case i of
        0 -> NotRequired
        1 -> RequiredAlways
        _ -> (error . showText) $ "Unknown index for RequiredAttributeBehavior: " <> toText i
    fromEnum x = case x of
        NotRequired -> 0
        RequiredAlways -> 1
        RequiredAttributeBehavior' name -> (error . showText) $ "Unknown RequiredAttributeBehavior: " <> original name

-- | Represents the bounds of /known/ $RequiredAttributeBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RequiredAttributeBehavior where
    minBound = NotRequired
    maxBound = RequiredAlways

instance Hashable     RequiredAttributeBehavior
instance NFData       RequiredAttributeBehavior
instance ToByteString RequiredAttributeBehavior
instance ToQuery      RequiredAttributeBehavior
instance ToHeader     RequiredAttributeBehavior

instance ToJSON RequiredAttributeBehavior where
    toJSON = toJSONText

instance FromJSON RequiredAttributeBehavior where
    parseJSON = parseJSONText "RequiredAttributeBehavior"

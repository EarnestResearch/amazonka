{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Operator
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.Operator (
  Operator (
    ..
    , OAll
    , OAny
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Operator = Operator' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern OAll :: Operator
pattern OAll = Operator' "ALL"

pattern OAny :: Operator
pattern OAny = Operator' "ANY"

{-# COMPLETE
  OAll,
  OAny,
  Operator' #-}

instance FromText Operator where
    parser = (Operator' . mk) <$> takeText

instance ToText Operator where
    toText (Operator' ci) = original ci

-- | Represents an enum of /known/ $Operator.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Operator where
    toEnum i = case i of
        0 -> OAll
        1 -> OAny
        _ -> (error . showText) $ "Unknown index for Operator: " <> toText i
    fromEnum x = case x of
        OAll -> 0
        OAny -> 1
        Operator' name -> (error . showText) $ "Unknown Operator: " <> original name

-- | Represents the bounds of /known/ $Operator.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Operator where
    minBound = OAll
    maxBound = OAny

instance Hashable     Operator
instance NFData       Operator
instance ToByteString Operator
instance ToQuery      Operator
instance ToHeader     Operator

instance ToJSON Operator where
    toJSON = toJSONText

instance FromJSON Operator where
    parseJSON = parseJSONText "Operator"

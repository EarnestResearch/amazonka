{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.BooleanEnumType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.BooleanEnumType (
  BooleanEnumType (
    ..
    , False'
    , True'
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data BooleanEnumType = BooleanEnumType' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern False' :: BooleanEnumType
pattern False' = BooleanEnumType' "FALSE"

pattern True' :: BooleanEnumType
pattern True' = BooleanEnumType' "TRUE"

{-# COMPLETE
  False',
  True',
  BooleanEnumType' #-}

instance FromText BooleanEnumType where
    parser = (BooleanEnumType' . mk) <$> takeText

instance ToText BooleanEnumType where
    toText (BooleanEnumType' ci) = original ci

-- | Represents an enum of /known/ $BooleanEnumType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BooleanEnumType where
    toEnum i = case i of
        0 -> False'
        1 -> True'
        _ -> (error . showText) $ "Unknown index for BooleanEnumType: " <> toText i
    fromEnum x = case x of
        False' -> 0
        True' -> 1
        BooleanEnumType' name -> (error . showText) $ "Unknown BooleanEnumType: " <> original name

-- | Represents the bounds of /known/ $BooleanEnumType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BooleanEnumType where
    minBound = False'
    maxBound = True'

instance Hashable     BooleanEnumType
instance NFData       BooleanEnumType
instance ToByteString BooleanEnumType
instance ToQuery      BooleanEnumType
instance ToHeader     BooleanEnumType

instance ToJSON BooleanEnumType where
    toJSON = toJSONText

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.Select
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.Select (
  Select (
    ..
    , AllAttributes
    , AllProjectedAttributes
    , Count
    , SpecificAttributes
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data Select = Select' (CI Text)
                deriving (Eq, Ord, Read, Show, Data, Typeable,
                          Generic)

pattern AllAttributes :: Select
pattern AllAttributes = Select' "ALL_ATTRIBUTES"

pattern AllProjectedAttributes :: Select
pattern AllProjectedAttributes = Select' "ALL_PROJECTED_ATTRIBUTES"

pattern Count :: Select
pattern Count = Select' "COUNT"

pattern SpecificAttributes :: Select
pattern SpecificAttributes = Select' "SPECIFIC_ATTRIBUTES"

{-# COMPLETE
  AllAttributes,
  AllProjectedAttributes,
  Count,
  SpecificAttributes,
  Select' #-}

instance FromText Select where
    parser = (Select' . mk) <$> takeText

instance ToText Select where
    toText (Select' ci) = original ci

-- | Represents an enum of /known/ $Select.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Select where
    toEnum i = case i of
        0 -> AllAttributes
        1 -> AllProjectedAttributes
        2 -> Count
        3 -> SpecificAttributes
        _ -> (error . showText) $ "Unknown index for Select: " <> toText i
    fromEnum x = case x of
        AllAttributes -> 0
        AllProjectedAttributes -> 1
        Count -> 2
        SpecificAttributes -> 3
        Select' name -> (error . showText) $ "Unknown Select: " <> original name

-- | Represents the bounds of /known/ $Select.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Select where
    minBound = AllAttributes
    maxBound = SpecificAttributes

instance Hashable     Select
instance NFData       Select
instance ToByteString Select
instance ToQuery      Select
instance ToHeader     Select

instance ToJSON Select where
    toJSON = toJSONText

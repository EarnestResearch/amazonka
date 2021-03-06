{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.ChangeType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MQ.Types.ChangeType (
  ChangeType (
    ..
    , Create
    , Delete
    , Update
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The type of change pending for the ActiveMQ user.
data ChangeType = ChangeType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern Create :: ChangeType
pattern Create = ChangeType' "CREATE"

pattern Delete :: ChangeType
pattern Delete = ChangeType' "DELETE"

pattern Update :: ChangeType
pattern Update = ChangeType' "UPDATE"

{-# COMPLETE
  Create,
  Delete,
  Update,
  ChangeType' #-}

instance FromText ChangeType where
    parser = (ChangeType' . mk) <$> takeText

instance ToText ChangeType where
    toText (ChangeType' ci) = original ci

-- | Represents an enum of /known/ $ChangeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ChangeType where
    toEnum i = case i of
        0 -> Create
        1 -> Delete
        2 -> Update
        _ -> (error . showText) $ "Unknown index for ChangeType: " <> toText i
    fromEnum x = case x of
        Create -> 0
        Delete -> 1
        Update -> 2
        ChangeType' name -> (error . showText) $ "Unknown ChangeType: " <> original name

-- | Represents the bounds of /known/ $ChangeType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ChangeType where
    minBound = Create
    maxBound = Update

instance Hashable     ChangeType
instance NFData       ChangeType
instance ToByteString ChangeType
instance ToQuery      ChangeType
instance ToHeader     ChangeType

instance FromJSON ChangeType where
    parseJSON = parseJSONText "ChangeType"

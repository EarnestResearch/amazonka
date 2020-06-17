{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationFilterKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.AssociationFilterKey (
  AssociationFilterKey (
    ..
    , AFKAssociationId
    , AFKAssociationName
    , AFKAssociationStatusName
    , AFKInstanceId
    , AFKLastExecutedAfter
    , AFKLastExecutedBefore
    , AFKName
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data AssociationFilterKey = AssociationFilterKey' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern AFKAssociationId :: AssociationFilterKey
pattern AFKAssociationId = AssociationFilterKey' "AssociationId"

pattern AFKAssociationName :: AssociationFilterKey
pattern AFKAssociationName = AssociationFilterKey' "AssociationName"

pattern AFKAssociationStatusName :: AssociationFilterKey
pattern AFKAssociationStatusName = AssociationFilterKey' "AssociationStatusName"

pattern AFKInstanceId :: AssociationFilterKey
pattern AFKInstanceId = AssociationFilterKey' "InstanceId"

pattern AFKLastExecutedAfter :: AssociationFilterKey
pattern AFKLastExecutedAfter = AssociationFilterKey' "LastExecutedAfter"

pattern AFKLastExecutedBefore :: AssociationFilterKey
pattern AFKLastExecutedBefore = AssociationFilterKey' "LastExecutedBefore"

pattern AFKName :: AssociationFilterKey
pattern AFKName = AssociationFilterKey' "Name"

{-# COMPLETE
  AFKAssociationId,
  AFKAssociationName,
  AFKAssociationStatusName,
  AFKInstanceId,
  AFKLastExecutedAfter,
  AFKLastExecutedBefore,
  AFKName,
  AssociationFilterKey' #-}

instance FromText AssociationFilterKey where
    parser = (AssociationFilterKey' . mk) <$> takeText

instance ToText AssociationFilterKey where
    toText (AssociationFilterKey' ci) = original ci

-- | Represents an enum of /known/ $AssociationFilterKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum AssociationFilterKey where
    toEnum i = case i of
        0 -> AFKAssociationId
        1 -> AFKAssociationName
        2 -> AFKAssociationStatusName
        3 -> AFKInstanceId
        4 -> AFKLastExecutedAfter
        5 -> AFKLastExecutedBefore
        6 -> AFKName
        _ -> (error . showText) $ "Unknown index for AssociationFilterKey: " <> toText i
    fromEnum x = case x of
        AFKAssociationId -> 0
        AFKAssociationName -> 1
        AFKAssociationStatusName -> 2
        AFKInstanceId -> 3
        AFKLastExecutedAfter -> 4
        AFKLastExecutedBefore -> 5
        AFKName -> 6
        AssociationFilterKey' name -> (error . showText) $ "Unknown AssociationFilterKey: " <> original name

-- | Represents the bounds of /known/ $AssociationFilterKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded AssociationFilterKey where
    minBound = AFKAssociationId
    maxBound = AFKName

instance Hashable     AssociationFilterKey
instance NFData       AssociationFilterKey
instance ToByteString AssociationFilterKey
instance ToQuery      AssociationFilterKey
instance ToHeader     AssociationFilterKey

instance ToJSON AssociationFilterKey where
    toJSON = toJSONText

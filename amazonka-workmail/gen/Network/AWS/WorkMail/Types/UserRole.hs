{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.UserRole
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkMail.Types.UserRole (
  UserRole (
    ..
    , URResource
    , URSystemUser
    , URUser
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data UserRole = UserRole' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern URResource :: UserRole
pattern URResource = UserRole' "RESOURCE"

pattern URSystemUser :: UserRole
pattern URSystemUser = UserRole' "SYSTEM_USER"

pattern URUser :: UserRole
pattern URUser = UserRole' "USER"

{-# COMPLETE
  URResource,
  URSystemUser,
  URUser,
  UserRole' #-}

instance FromText UserRole where
    parser = (UserRole' . mk) <$> takeText

instance ToText UserRole where
    toText (UserRole' ci) = original ci

-- | Represents an enum of /known/ $UserRole.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum UserRole where
    toEnum i = case i of
        0 -> URResource
        1 -> URSystemUser
        2 -> URUser
        _ -> (error . showText) $ "Unknown index for UserRole: " <> toText i
    fromEnum x = case x of
        URResource -> 0
        URSystemUser -> 1
        URUser -> 2
        UserRole' name -> (error . showText) $ "Unknown UserRole: " <> original name

-- | Represents the bounds of /known/ $UserRole.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded UserRole where
    minBound = URResource
    maxBound = URUser

instance Hashable     UserRole
instance NFData       UserRole
instance ToByteString UserRole
instance ToQuery      UserRole
instance ToHeader     UserRole

instance FromJSON UserRole where
    parseJSON = parseJSONText "UserRole"

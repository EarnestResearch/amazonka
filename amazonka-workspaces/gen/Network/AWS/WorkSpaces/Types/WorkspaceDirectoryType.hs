{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceDirectoryType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.WorkspaceDirectoryType (
  WorkspaceDirectoryType (
    ..
    , AdConnector
    , SimpleAd
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data WorkspaceDirectoryType = WorkspaceDirectoryType' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern AdConnector :: WorkspaceDirectoryType
pattern AdConnector = WorkspaceDirectoryType' "AD_CONNECTOR"

pattern SimpleAd :: WorkspaceDirectoryType
pattern SimpleAd = WorkspaceDirectoryType' "SIMPLE_AD"

{-# COMPLETE
  AdConnector,
  SimpleAd,
  WorkspaceDirectoryType' #-}

instance FromText WorkspaceDirectoryType where
    parser = (WorkspaceDirectoryType' . mk) <$> takeText

instance ToText WorkspaceDirectoryType where
    toText (WorkspaceDirectoryType' ci) = original ci

-- | Represents an enum of /known/ $WorkspaceDirectoryType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum WorkspaceDirectoryType where
    toEnum i = case i of
        0 -> AdConnector
        1 -> SimpleAd
        _ -> (error . showText) $ "Unknown index for WorkspaceDirectoryType: " <> toText i
    fromEnum x = case x of
        AdConnector -> 0
        SimpleAd -> 1
        WorkspaceDirectoryType' name -> (error . showText) $ "Unknown WorkspaceDirectoryType: " <> original name

-- | Represents the bounds of /known/ $WorkspaceDirectoryType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded WorkspaceDirectoryType where
    minBound = AdConnector
    maxBound = SimpleAd

instance Hashable     WorkspaceDirectoryType
instance NFData       WorkspaceDirectoryType
instance ToByteString WorkspaceDirectoryType
instance ToQuery      WorkspaceDirectoryType
instance ToHeader     WorkspaceDirectoryType

instance FromJSON WorkspaceDirectoryType where
    parseJSON = parseJSONText "WorkspaceDirectoryType"

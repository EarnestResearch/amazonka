{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.LaunchType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchEvents.Types.LaunchType (
  LaunchType (
    ..
    , EC2
    , Fargate
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data LaunchType = LaunchType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern EC2 :: LaunchType
pattern EC2 = LaunchType' "EC2"

pattern Fargate :: LaunchType
pattern Fargate = LaunchType' "FARGATE"

{-# COMPLETE
  EC2,
  Fargate,
  LaunchType' #-}

instance FromText LaunchType where
    parser = (LaunchType' . mk) <$> takeText

instance ToText LaunchType where
    toText (LaunchType' ci) = original ci

-- | Represents an enum of /known/ $LaunchType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LaunchType where
    toEnum i = case i of
        0 -> EC2
        1 -> Fargate
        _ -> (error . showText) $ "Unknown index for LaunchType: " <> toText i
    fromEnum x = case x of
        EC2 -> 0
        Fargate -> 1
        LaunchType' name -> (error . showText) $ "Unknown LaunchType: " <> original name

-- | Represents the bounds of /known/ $LaunchType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LaunchType where
    minBound = EC2
    maxBound = Fargate

instance Hashable     LaunchType
instance NFData       LaunchType
instance ToByteString LaunchType
instance ToQuery      LaunchType
instance ToHeader     LaunchType

instance ToJSON LaunchType where
    toJSON = toJSONText

instance FromJSON LaunchType where
    parseJSON = parseJSONText "LaunchType"

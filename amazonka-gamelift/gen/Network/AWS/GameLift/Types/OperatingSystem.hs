{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.OperatingSystem
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.OperatingSystem (
  OperatingSystem (
    ..
    , AmazonLinux
    , AmazonLinux2
    , Windows2012
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data OperatingSystem = OperatingSystem' (CI Text)
                         deriving (Eq, Ord, Read, Show, Data, Typeable,
                                   Generic)

pattern AmazonLinux :: OperatingSystem
pattern AmazonLinux = OperatingSystem' "AMAZON_LINUX"

pattern AmazonLinux2 :: OperatingSystem
pattern AmazonLinux2 = OperatingSystem' "AMAZON_LINUX_2"

pattern Windows2012 :: OperatingSystem
pattern Windows2012 = OperatingSystem' "WINDOWS_2012"

{-# COMPLETE
  AmazonLinux,
  AmazonLinux2,
  Windows2012,
  OperatingSystem' #-}

instance FromText OperatingSystem where
    parser = (OperatingSystem' . mk) <$> takeText

instance ToText OperatingSystem where
    toText (OperatingSystem' ci) = original ci

-- | Represents an enum of /known/ $OperatingSystem.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum OperatingSystem where
    toEnum i = case i of
        0 -> AmazonLinux
        1 -> AmazonLinux2
        2 -> Windows2012
        _ -> (error . showText) $ "Unknown index for OperatingSystem: " <> toText i
    fromEnum x = case x of
        AmazonLinux -> 0
        AmazonLinux2 -> 1
        Windows2012 -> 2
        OperatingSystem' name -> (error . showText) $ "Unknown OperatingSystem: " <> original name

-- | Represents the bounds of /known/ $OperatingSystem.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded OperatingSystem where
    minBound = AmazonLinux
    maxBound = Windows2012

instance Hashable     OperatingSystem
instance NFData       OperatingSystem
instance ToByteString OperatingSystem
instance ToQuery      OperatingSystem
instance ToHeader     OperatingSystem

instance ToJSON OperatingSystem where
    toJSON = toJSONText

instance FromJSON OperatingSystem where
    parseJSON = parseJSONText "OperatingSystem"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateHTTPTokensState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchTemplateHTTPTokensState (
  LaunchTemplateHTTPTokensState (
    ..
    , Optional
    , Required
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data LaunchTemplateHTTPTokensState = LaunchTemplateHTTPTokensState' (CI
                                                                       Text)
                                       deriving (Eq, Ord, Read, Show, Data,
                                                 Typeable, Generic)

pattern Optional :: LaunchTemplateHTTPTokensState
pattern Optional = LaunchTemplateHTTPTokensState' "optional"

pattern Required :: LaunchTemplateHTTPTokensState
pattern Required = LaunchTemplateHTTPTokensState' "required"

{-# COMPLETE
  Optional,
  Required,
  LaunchTemplateHTTPTokensState' #-}

instance FromText LaunchTemplateHTTPTokensState where
    parser = (LaunchTemplateHTTPTokensState' . mk) <$> takeText

instance ToText LaunchTemplateHTTPTokensState where
    toText (LaunchTemplateHTTPTokensState' ci) = original ci

-- | Represents an enum of /known/ $LaunchTemplateHTTPTokensState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum LaunchTemplateHTTPTokensState where
    toEnum i = case i of
        0 -> Optional
        1 -> Required
        _ -> (error . showText) $ "Unknown index for LaunchTemplateHTTPTokensState: " <> toText i
    fromEnum x = case x of
        Optional -> 0
        Required -> 1
        LaunchTemplateHTTPTokensState' name -> (error . showText) $ "Unknown LaunchTemplateHTTPTokensState: " <> original name

-- | Represents the bounds of /known/ $LaunchTemplateHTTPTokensState.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded LaunchTemplateHTTPTokensState where
    minBound = Optional
    maxBound = Required

instance Hashable     LaunchTemplateHTTPTokensState
instance NFData       LaunchTemplateHTTPTokensState
instance ToByteString LaunchTemplateHTTPTokensState
instance ToQuery      LaunchTemplateHTTPTokensState
instance ToHeader     LaunchTemplateHTTPTokensState

instance FromXML LaunchTemplateHTTPTokensState where
    parseXML = parseXMLText "LaunchTemplateHTTPTokensState"

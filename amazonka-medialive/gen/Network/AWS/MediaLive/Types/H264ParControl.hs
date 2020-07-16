{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264ParControl
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H264ParControl (
  H264ParControl (
    ..
    , InitializeFromSource
    , Specified
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | H264 Par Control
data H264ParControl = H264ParControl' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern InitializeFromSource :: H264ParControl
pattern InitializeFromSource = H264ParControl' "INITIALIZE_FROM_SOURCE"

pattern Specified :: H264ParControl
pattern Specified = H264ParControl' "SPECIFIED"

{-# COMPLETE
  InitializeFromSource,
  Specified,
  H264ParControl' #-}

instance FromText H264ParControl where
    parser = (H264ParControl' . mk) <$> takeText

instance ToText H264ParControl where
    toText (H264ParControl' ci) = original ci

-- | Represents an enum of /known/ $H264ParControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H264ParControl where
    toEnum i = case i of
        0 -> InitializeFromSource
        1 -> Specified
        _ -> (error . showText) $ "Unknown index for H264ParControl: " <> toText i
    fromEnum x = case x of
        InitializeFromSource -> 0
        Specified -> 1
        H264ParControl' name -> (error . showText) $ "Unknown H264ParControl: " <> original name

-- | Represents the bounds of /known/ $H264ParControl.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H264ParControl where
    minBound = InitializeFromSource
    maxBound = Specified

instance Hashable     H264ParControl
instance NFData       H264ParControl
instance ToByteString H264ParControl
instance ToQuery      H264ParControl
instance ToHeader     H264ParControl

instance ToJSON H264ParControl where
    toJSON = toJSONText

instance FromJSON H264ParControl where
    parseJSON = parseJSONText "H264ParControl"

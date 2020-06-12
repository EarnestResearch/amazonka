{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264TimecodeInsertionBehavior
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H264TimecodeInsertionBehavior where

import Network.AWS.Prelude
  
-- | Placeholder documentation for H264TimecodeInsertionBehavior
data H264TimecodeInsertionBehavior = HTIBDisabled
                                   | HTIBPicTimingSei
                                       deriving (Eq, Ord, Read, Show, Enum,
                                                 Bounded, Data, Typeable,
                                                 Generic)

instance FromText H264TimecodeInsertionBehavior where
    parser = takeLowerText >>= \case
        "disabled" -> pure HTIBDisabled
        "pic_timing_sei" -> pure HTIBPicTimingSei
        e -> fromTextError $ "Failure parsing H264TimecodeInsertionBehavior from value: '" <> e
           <> "'. Accepted values: disabled, pic_timing_sei"

instance ToText H264TimecodeInsertionBehavior where
    toText = \case
        HTIBDisabled -> "DISABLED"
        HTIBPicTimingSei -> "PIC_TIMING_SEI"

instance Hashable     H264TimecodeInsertionBehavior
instance NFData       H264TimecodeInsertionBehavior
instance ToByteString H264TimecodeInsertionBehavior
instance ToQuery      H264TimecodeInsertionBehavior
instance ToHeader     H264TimecodeInsertionBehavior

instance ToJSON H264TimecodeInsertionBehavior where
    toJSON = toJSONText

instance FromJSON H264TimecodeInsertionBehavior where
    parseJSON = parseJSONText "H264TimecodeInsertionBehavior"

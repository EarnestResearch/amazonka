{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectLockRetentionMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.ObjectLockRetentionMode (
  ObjectLockRetentionMode (
    ..
    , OLRMCompliance
    , OLRMGovernance
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.S3.Internal

data ObjectLockRetentionMode = ObjectLockRetentionMode' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern OLRMCompliance :: ObjectLockRetentionMode
pattern OLRMCompliance = ObjectLockRetentionMode' "COMPLIANCE"

pattern OLRMGovernance :: ObjectLockRetentionMode
pattern OLRMGovernance = ObjectLockRetentionMode' "GOVERNANCE"

{-# COMPLETE
  OLRMCompliance,
  OLRMGovernance,
  ObjectLockRetentionMode' #-}

instance FromText ObjectLockRetentionMode where
    parser = (ObjectLockRetentionMode' . mk) <$> takeText

instance ToText ObjectLockRetentionMode where
    toText (ObjectLockRetentionMode' ci) = original ci

-- | Represents an enum of /known/ $ObjectLockRetentionMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ObjectLockRetentionMode where
    toEnum i = case i of
        0 -> OLRMCompliance
        1 -> OLRMGovernance
        _ -> (error . showText) $ "Unknown index for ObjectLockRetentionMode: " <> toText i
    fromEnum x = case x of
        OLRMCompliance -> 0
        OLRMGovernance -> 1
        ObjectLockRetentionMode' name -> (error . showText) $ "Unknown ObjectLockRetentionMode: " <> original name

-- | Represents the bounds of /known/ $ObjectLockRetentionMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ObjectLockRetentionMode where
    minBound = OLRMCompliance
    maxBound = OLRMGovernance

instance Hashable     ObjectLockRetentionMode
instance NFData       ObjectLockRetentionMode
instance ToByteString ObjectLockRetentionMode
instance ToQuery      ObjectLockRetentionMode
instance ToHeader     ObjectLockRetentionMode

instance FromXML ObjectLockRetentionMode where
    parseXML = parseXMLText "ObjectLockRetentionMode"

instance ToXML ObjectLockRetentionMode where
    toXML = toXMLText

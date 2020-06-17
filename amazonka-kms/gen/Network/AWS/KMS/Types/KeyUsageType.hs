{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.KeyUsageType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KMS.Types.KeyUsageType (
  KeyUsageType (
    ..
    , EncryptDecrypt
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data KeyUsageType = KeyUsageType' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern EncryptDecrypt :: KeyUsageType
pattern EncryptDecrypt = KeyUsageType' "ENCRYPT_DECRYPT"

{-# COMPLETE
  EncryptDecrypt,
  KeyUsageType' #-}

instance FromText KeyUsageType where
    parser = (KeyUsageType' . mk) <$> takeText

instance ToText KeyUsageType where
    toText (KeyUsageType' ci) = original ci

-- | Represents an enum of /known/ $KeyUsageType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum KeyUsageType where
    toEnum i = case i of
        0 -> EncryptDecrypt
        _ -> (error . showText) $ "Unknown index for KeyUsageType: " <> toText i
    fromEnum x = case x of
        EncryptDecrypt -> 0
        KeyUsageType' name -> (error . showText) $ "Unknown KeyUsageType: " <> original name

-- | Represents the bounds of /known/ $KeyUsageType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded KeyUsageType where
    minBound = EncryptDecrypt
    maxBound = EncryptDecrypt

instance Hashable     KeyUsageType
instance NFData       KeyUsageType
instance ToByteString KeyUsageType
instance ToQuery      KeyUsageType
instance ToHeader     KeyUsageType

instance ToJSON KeyUsageType where
    toJSON = toJSONText

instance FromJSON KeyUsageType where
    parseJSON = parseJSONText "KeyUsageType"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceCreationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.NetworkInterfaceCreationType (
  NetworkInterfaceCreationType (
    ..
    , Efa
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data NetworkInterfaceCreationType = NetworkInterfaceCreationType' (CI
                                                                     Text)
                                      deriving (Eq, Ord, Read, Show, Data,
                                                Typeable, Generic)

pattern Efa :: NetworkInterfaceCreationType
pattern Efa = NetworkInterfaceCreationType' "efa"

{-# COMPLETE
  Efa,
  NetworkInterfaceCreationType' #-}

instance FromText NetworkInterfaceCreationType where
    parser = (NetworkInterfaceCreationType' . mk) <$> takeText

instance ToText NetworkInterfaceCreationType where
    toText (NetworkInterfaceCreationType' ci) = original ci

-- | Represents an enum of /known/ $NetworkInterfaceCreationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum NetworkInterfaceCreationType where
    toEnum i = case i of
        0 -> Efa
        _ -> (error . showText) $ "Unknown index for NetworkInterfaceCreationType: " <> toText i
    fromEnum x = case x of
        Efa -> 0
        NetworkInterfaceCreationType' name -> (error . showText) $ "Unknown NetworkInterfaceCreationType: " <> original name

-- | Represents the bounds of /known/ $NetworkInterfaceCreationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded NetworkInterfaceCreationType where
    minBound = Efa
    maxBound = Efa

instance Hashable     NetworkInterfaceCreationType
instance NFData       NetworkInterfaceCreationType
instance ToByteString NetworkInterfaceCreationType
instance ToQuery      NetworkInterfaceCreationType
instance ToHeader     NetworkInterfaceCreationType

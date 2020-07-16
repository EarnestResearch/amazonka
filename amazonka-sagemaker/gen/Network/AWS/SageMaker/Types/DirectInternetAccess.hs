{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DirectInternetAccess
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.DirectInternetAccess (
  DirectInternetAccess (
    ..
    , DIADisabled
    , DIAEnabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DirectInternetAccess = DirectInternetAccess' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern DIADisabled :: DirectInternetAccess
pattern DIADisabled = DirectInternetAccess' "Disabled"

pattern DIAEnabled :: DirectInternetAccess
pattern DIAEnabled = DirectInternetAccess' "Enabled"

{-# COMPLETE
  DIADisabled,
  DIAEnabled,
  DirectInternetAccess' #-}

instance FromText DirectInternetAccess where
    parser = (DirectInternetAccess' . mk) <$> takeText

instance ToText DirectInternetAccess where
    toText (DirectInternetAccess' ci) = original ci

-- | Represents an enum of /known/ $DirectInternetAccess.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DirectInternetAccess where
    toEnum i = case i of
        0 -> DIADisabled
        1 -> DIAEnabled
        _ -> (error . showText) $ "Unknown index for DirectInternetAccess: " <> toText i
    fromEnum x = case x of
        DIADisabled -> 0
        DIAEnabled -> 1
        DirectInternetAccess' name -> (error . showText) $ "Unknown DirectInternetAccess: " <> original name

-- | Represents the bounds of /known/ $DirectInternetAccess.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DirectInternetAccess where
    minBound = DIADisabled
    maxBound = DIAEnabled

instance Hashable     DirectInternetAccess
instance NFData       DirectInternetAccess
instance ToByteString DirectInternetAccess
instance ToQuery      DirectInternetAccess
instance ToHeader     DirectInternetAccess

instance ToJSON DirectInternetAccess where
    toJSON = toJSONText

instance FromJSON DirectInternetAccess where
    parseJSON = parseJSONText "DirectInternetAccess"

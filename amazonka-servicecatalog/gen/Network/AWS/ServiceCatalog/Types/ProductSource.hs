{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProductSource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ProductSource (
  ProductSource (
    ..
    , PSAccount
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ProductSource = ProductSource' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern PSAccount :: ProductSource
pattern PSAccount = ProductSource' "ACCOUNT"

{-# COMPLETE
  PSAccount,
  ProductSource' #-}

instance FromText ProductSource where
    parser = (ProductSource' . mk) <$> takeText

instance ToText ProductSource where
    toText (ProductSource' ci) = original ci

-- | Represents an enum of /known/ $ProductSource.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProductSource where
    toEnum i = case i of
        0 -> PSAccount
        _ -> (error . showText) $ "Unknown index for ProductSource: " <> toText i
    fromEnum x = case x of
        PSAccount -> 0
        ProductSource' name -> (error . showText) $ "Unknown ProductSource: " <> original name

-- | Represents the bounds of /known/ $ProductSource.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProductSource where
    minBound = PSAccount
    maxBound = PSAccount

instance Hashable     ProductSource
instance NFData       ProductSource
instance ToByteString ProductSource
instance ToQuery      ProductSource
instance ToHeader     ProductSource

instance ToJSON ProductSource where
    toJSON = toJSONText

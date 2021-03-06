{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.SortByOrder
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.SortByOrder (
  SortByOrder (
    ..
    , Asc
    , Desc
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

data SortByOrder = SortByOrder' (CI Text)
                     deriving (Eq, Ord, Read, Show, Data, Typeable,
                               Generic)

pattern Asc :: SortByOrder
pattern Asc = SortByOrder' "ASC"

pattern Desc :: SortByOrder
pattern Desc = SortByOrder' "DESC"

{-# COMPLETE
  Asc,
  Desc,
  SortByOrder' #-}

instance FromText SortByOrder where
    parser = (SortByOrder' . mk) <$> takeText

instance ToText SortByOrder where
    toText (SortByOrder' ci) = original ci

-- | Represents an enum of /known/ $SortByOrder.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SortByOrder where
    toEnum i = case i of
        0 -> Asc
        1 -> Desc
        _ -> (error . showText) $ "Unknown index for SortByOrder: " <> toText i
    fromEnum x = case x of
        Asc -> 0
        Desc -> 1
        SortByOrder' name -> (error . showText) $ "Unknown SortByOrder: " <> original name

-- | Represents the bounds of /known/ $SortByOrder.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SortByOrder where
    minBound = Asc
    maxBound = Desc

instance Hashable     SortByOrder
instance NFData       SortByOrder
instance ToByteString SortByOrder
instance ToQuery      SortByOrder
instance ToHeader     SortByOrder

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ChangeTypeEnum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.ChangeTypeEnum (
  ChangeTypeEnum (
    ..
    , A
    , D
    , M
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ChangeTypeEnum = ChangeTypeEnum' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern A :: ChangeTypeEnum
pattern A = ChangeTypeEnum' "A"

pattern D :: ChangeTypeEnum
pattern D = ChangeTypeEnum' "D"

pattern M :: ChangeTypeEnum
pattern M = ChangeTypeEnum' "M"

{-# COMPLETE
  A,
  D,
  M,
  ChangeTypeEnum' #-}

instance FromText ChangeTypeEnum where
    parser = (ChangeTypeEnum' . mk) <$> takeText

instance ToText ChangeTypeEnum where
    toText (ChangeTypeEnum' ci) = original ci

-- | Represents an enum of /known/ $ChangeTypeEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ChangeTypeEnum where
    toEnum i = case i of
        0 -> A
        1 -> D
        2 -> M
        _ -> (error . showText) $ "Unknown index for ChangeTypeEnum: " <> toText i
    fromEnum x = case x of
        A -> 0
        D -> 1
        M -> 2
        ChangeTypeEnum' name -> (error . showText) $ "Unknown ChangeTypeEnum: " <> original name

-- | Represents the bounds of /known/ $ChangeTypeEnum.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ChangeTypeEnum where
    minBound = A
    maxBound = M

instance Hashable     ChangeTypeEnum
instance NFData       ChangeTypeEnum
instance ToByteString ChangeTypeEnum
instance ToQuery      ChangeTypeEnum
instance ToHeader     ChangeTypeEnum

instance FromJSON ChangeTypeEnum where
    parseJSON = parseJSONText "ChangeTypeEnum"

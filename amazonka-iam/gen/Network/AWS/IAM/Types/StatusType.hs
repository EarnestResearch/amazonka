{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.StatusType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.StatusType (
  StatusType (
    ..
    , Active
    , Inactive
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data StatusType = StatusType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern Active :: StatusType
pattern Active = StatusType' "Active"

pattern Inactive :: StatusType
pattern Inactive = StatusType' "Inactive"

{-# COMPLETE
  Active,
  Inactive,
  StatusType' #-}

instance FromText StatusType where
    parser = (StatusType' . mk) <$> takeText

instance ToText StatusType where
    toText (StatusType' ci) = original ci

-- | Represents an enum of /known/ $StatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum StatusType where
    toEnum i = case i of
        0 -> Active
        1 -> Inactive
        _ -> (error . showText) $ "Unknown index for StatusType: " <> toText i
    fromEnum x = case x of
        Active -> 0
        Inactive -> 1
        StatusType' name -> (error . showText) $ "Unknown StatusType: " <> original name

-- | Represents the bounds of /known/ $StatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded StatusType where
    minBound = Active
    maxBound = Inactive

instance Hashable     StatusType
instance NFData       StatusType
instance ToByteString StatusType
instance ToQuery      StatusType
instance ToHeader     StatusType

instance FromXML StatusType where
    parseXML = parseXMLText "StatusType"

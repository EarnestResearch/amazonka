{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.EndpointConfigSortKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.EndpointConfigSortKey (
  EndpointConfigSortKey (
    ..
    , ECSKCreationTime
    , ECSKName
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data EndpointConfigSortKey = EndpointConfigSortKey' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern ECSKCreationTime :: EndpointConfigSortKey
pattern ECSKCreationTime = EndpointConfigSortKey' "CreationTime"

pattern ECSKName :: EndpointConfigSortKey
pattern ECSKName = EndpointConfigSortKey' "Name"

{-# COMPLETE
  ECSKCreationTime,
  ECSKName,
  EndpointConfigSortKey' #-}

instance FromText EndpointConfigSortKey where
    parser = (EndpointConfigSortKey' . mk) <$> takeText

instance ToText EndpointConfigSortKey where
    toText (EndpointConfigSortKey' ci) = original ci

-- | Represents an enum of /known/ $EndpointConfigSortKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EndpointConfigSortKey where
    toEnum i = case i of
        0 -> ECSKCreationTime
        1 -> ECSKName
        _ -> (error . showText) $ "Unknown index for EndpointConfigSortKey: " <> toText i
    fromEnum x = case x of
        ECSKCreationTime -> 0
        ECSKName -> 1
        EndpointConfigSortKey' name -> (error . showText) $ "Unknown EndpointConfigSortKey: " <> original name

-- | Represents the bounds of /known/ $EndpointConfigSortKey.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EndpointConfigSortKey where
    minBound = ECSKCreationTime
    maxBound = ECSKName

instance Hashable     EndpointConfigSortKey
instance NFData       EndpointConfigSortKey
instance ToByteString EndpointConfigSortKey
instance ToQuery      EndpointConfigSortKey
instance ToHeader     EndpointConfigSortKey

instance ToJSON EndpointConfigSortKey where
    toJSON = toJSONText

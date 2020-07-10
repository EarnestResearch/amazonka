{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingS3DataDistributionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ProcessingS3DataDistributionType (
  ProcessingS3DataDistributionType (
    ..
    , PSDDTFullyReplicated
    , PSDDTShardedByS3Key
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ProcessingS3DataDistributionType = ProcessingS3DataDistributionType' (CI
                                                                             Text)
                                          deriving (Eq, Ord, Read, Show, Data,
                                                    Typeable, Generic)

pattern PSDDTFullyReplicated :: ProcessingS3DataDistributionType
pattern PSDDTFullyReplicated = ProcessingS3DataDistributionType' "FullyReplicated"

pattern PSDDTShardedByS3Key :: ProcessingS3DataDistributionType
pattern PSDDTShardedByS3Key = ProcessingS3DataDistributionType' "ShardedByS3Key"

{-# COMPLETE
  PSDDTFullyReplicated,
  PSDDTShardedByS3Key,
  ProcessingS3DataDistributionType' #-}

instance FromText ProcessingS3DataDistributionType where
    parser = (ProcessingS3DataDistributionType' . mk) <$> takeText

instance ToText ProcessingS3DataDistributionType where
    toText (ProcessingS3DataDistributionType' ci) = original ci

-- | Represents an enum of /known/ $ProcessingS3DataDistributionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ProcessingS3DataDistributionType where
    toEnum i = case i of
        0 -> PSDDTFullyReplicated
        1 -> PSDDTShardedByS3Key
        _ -> (error . showText) $ "Unknown index for ProcessingS3DataDistributionType: " <> toText i
    fromEnum x = case x of
        PSDDTFullyReplicated -> 0
        PSDDTShardedByS3Key -> 1
        ProcessingS3DataDistributionType' name -> (error . showText) $ "Unknown ProcessingS3DataDistributionType: " <> original name

-- | Represents the bounds of /known/ $ProcessingS3DataDistributionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ProcessingS3DataDistributionType where
    minBound = PSDDTFullyReplicated
    maxBound = PSDDTShardedByS3Key

instance Hashable     ProcessingS3DataDistributionType
instance NFData       ProcessingS3DataDistributionType
instance ToByteString ProcessingS3DataDistributionType
instance ToQuery      ProcessingS3DataDistributionType
instance ToHeader     ProcessingS3DataDistributionType

instance ToJSON ProcessingS3DataDistributionType where
    toJSON = toJSONText

instance FromJSON ProcessingS3DataDistributionType where
    parseJSON = parseJSONText "ProcessingS3DataDistributionType"

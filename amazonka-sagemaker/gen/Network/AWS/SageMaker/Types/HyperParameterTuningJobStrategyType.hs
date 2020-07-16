{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTuningJobStrategyType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.HyperParameterTuningJobStrategyType (
  HyperParameterTuningJobStrategyType (
    ..
    , Bayesian
    , Random
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The strategy hyperparameter tuning uses to find the best combination of hyperparameters for your model. Currently, the only supported value is @Bayesian@ .
--
--
data HyperParameterTuningJobStrategyType = HyperParameterTuningJobStrategyType' (CI
                                                                                   Text)
                                             deriving (Eq, Ord, Read, Show,
                                                       Data, Typeable, Generic)

pattern Bayesian :: HyperParameterTuningJobStrategyType
pattern Bayesian = HyperParameterTuningJobStrategyType' "Bayesian"

pattern Random :: HyperParameterTuningJobStrategyType
pattern Random = HyperParameterTuningJobStrategyType' "Random"

{-# COMPLETE
  Bayesian,
  Random,
  HyperParameterTuningJobStrategyType' #-}

instance FromText HyperParameterTuningJobStrategyType where
    parser = (HyperParameterTuningJobStrategyType' . mk) <$> takeText

instance ToText HyperParameterTuningJobStrategyType where
    toText (HyperParameterTuningJobStrategyType' ci) = original ci

-- | Represents an enum of /known/ $HyperParameterTuningJobStrategyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum HyperParameterTuningJobStrategyType where
    toEnum i = case i of
        0 -> Bayesian
        1 -> Random
        _ -> (error . showText) $ "Unknown index for HyperParameterTuningJobStrategyType: " <> toText i
    fromEnum x = case x of
        Bayesian -> 0
        Random -> 1
        HyperParameterTuningJobStrategyType' name -> (error . showText) $ "Unknown HyperParameterTuningJobStrategyType: " <> original name

-- | Represents the bounds of /known/ $HyperParameterTuningJobStrategyType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded HyperParameterTuningJobStrategyType where
    minBound = Bayesian
    maxBound = Random

instance Hashable     HyperParameterTuningJobStrategyType
instance NFData       HyperParameterTuningJobStrategyType
instance ToByteString HyperParameterTuningJobStrategyType
instance ToQuery      HyperParameterTuningJobStrategyType
instance ToHeader     HyperParameterTuningJobStrategyType

instance ToJSON HyperParameterTuningJobStrategyType where
    toJSON = toJSONText

instance FromJSON HyperParameterTuningJobStrategyType where
    parseJSON = parseJSONText "HyperParameterTuningJobStrategyType"

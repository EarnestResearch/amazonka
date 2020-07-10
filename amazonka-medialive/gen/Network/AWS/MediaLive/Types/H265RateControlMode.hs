{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265RateControlMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H265RateControlMode (
  H265RateControlMode (
    ..
    , Cbr
    , Multiplex
    , Qvbr
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | H265 Rate Control Mode
data H265RateControlMode = H265RateControlMode' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern Cbr :: H265RateControlMode
pattern Cbr = H265RateControlMode' "CBR"

pattern Multiplex :: H265RateControlMode
pattern Multiplex = H265RateControlMode' "MULTIPLEX"

pattern Qvbr :: H265RateControlMode
pattern Qvbr = H265RateControlMode' "QVBR"

{-# COMPLETE
  Cbr,
  Multiplex,
  Qvbr,
  H265RateControlMode' #-}

instance FromText H265RateControlMode where
    parser = (H265RateControlMode' . mk) <$> takeText

instance ToText H265RateControlMode where
    toText (H265RateControlMode' ci) = original ci

-- | Represents an enum of /known/ $H265RateControlMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H265RateControlMode where
    toEnum i = case i of
        0 -> Cbr
        1 -> Multiplex
        2 -> Qvbr
        _ -> (error . showText) $ "Unknown index for H265RateControlMode: " <> toText i
    fromEnum x = case x of
        Cbr -> 0
        Multiplex -> 1
        Qvbr -> 2
        H265RateControlMode' name -> (error . showText) $ "Unknown H265RateControlMode: " <> original name

-- | Represents the bounds of /known/ $H265RateControlMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H265RateControlMode where
    minBound = Cbr
    maxBound = Qvbr

instance Hashable     H265RateControlMode
instance NFData       H265RateControlMode
instance ToByteString H265RateControlMode
instance ToQuery      H265RateControlMode
instance ToHeader     H265RateControlMode

instance ToJSON H265RateControlMode where
    toJSON = toJSONText

instance FromJSON H265RateControlMode where
    parseJSON = parseJSONText "H265RateControlMode"

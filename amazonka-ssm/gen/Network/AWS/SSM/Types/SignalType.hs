{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.SignalType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.SignalType (
  SignalType (
    ..
    , Approve
    , Reject
    , Resume
    , StartStep
    , StopStep
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SignalType = SignalType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern Approve :: SignalType
pattern Approve = SignalType' "Approve"

pattern Reject :: SignalType
pattern Reject = SignalType' "Reject"

pattern Resume :: SignalType
pattern Resume = SignalType' "Resume"

pattern StartStep :: SignalType
pattern StartStep = SignalType' "StartStep"

pattern StopStep :: SignalType
pattern StopStep = SignalType' "StopStep"

{-# COMPLETE
  Approve,
  Reject,
  Resume,
  StartStep,
  StopStep,
  SignalType' #-}

instance FromText SignalType where
    parser = (SignalType' . mk) <$> takeText

instance ToText SignalType where
    toText (SignalType' ci) = original ci

-- | Represents an enum of /known/ $SignalType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SignalType where
    toEnum i = case i of
        0 -> Approve
        1 -> Reject
        2 -> Resume
        3 -> StartStep
        4 -> StopStep
        _ -> (error . showText) $ "Unknown index for SignalType: " <> toText i
    fromEnum x = case x of
        Approve -> 0
        Reject -> 1
        Resume -> 2
        StartStep -> 3
        StopStep -> 4
        SignalType' name -> (error . showText) $ "Unknown SignalType: " <> original name

-- | Represents the bounds of /known/ $SignalType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SignalType where
    minBound = Approve
    maxBound = StopStep

instance Hashable     SignalType
instance NFData       SignalType
instance ToByteString SignalType
instance ToQuery      SignalType
instance ToHeader     SignalType

instance ToJSON SignalType where
    toJSON = toJSONText

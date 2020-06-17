{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficMirrorRuleAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TrafficMirrorRuleAction (
  TrafficMirrorRuleAction (
    ..
    , Accept
    , Reject
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data TrafficMirrorRuleAction = TrafficMirrorRuleAction' (CI
                                                           Text)
                                 deriving (Eq, Ord, Read, Show, Data, Typeable,
                                           Generic)

pattern Accept :: TrafficMirrorRuleAction
pattern Accept = TrafficMirrorRuleAction' "accept"

pattern Reject :: TrafficMirrorRuleAction
pattern Reject = TrafficMirrorRuleAction' "reject"

{-# COMPLETE
  Accept,
  Reject,
  TrafficMirrorRuleAction' #-}

instance FromText TrafficMirrorRuleAction where
    parser = (TrafficMirrorRuleAction' . mk) <$> takeText

instance ToText TrafficMirrorRuleAction where
    toText (TrafficMirrorRuleAction' ci) = original ci

-- | Represents an enum of /known/ $TrafficMirrorRuleAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TrafficMirrorRuleAction where
    toEnum i = case i of
        0 -> Accept
        1 -> Reject
        _ -> (error . showText) $ "Unknown index for TrafficMirrorRuleAction: " <> toText i
    fromEnum x = case x of
        Accept -> 0
        Reject -> 1
        TrafficMirrorRuleAction' name -> (error . showText) $ "Unknown TrafficMirrorRuleAction: " <> original name

-- | Represents the bounds of /known/ $TrafficMirrorRuleAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TrafficMirrorRuleAction where
    minBound = Accept
    maxBound = Reject

instance Hashable     TrafficMirrorRuleAction
instance NFData       TrafficMirrorRuleAction
instance ToByteString TrafficMirrorRuleAction
instance ToQuery      TrafficMirrorRuleAction
instance ToHeader     TrafficMirrorRuleAction

instance FromXML TrafficMirrorRuleAction where
    parseXML = parseXMLText "TrafficMirrorRuleAction"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudTrail.Types.Sum where

import Network.AWS.Prelude

data EventCategory =
  Insight
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EventCategory where
    parser = takeLowerText >>= \case
        "insight" -> pure Insight
        e -> fromTextError $ "Failure parsing EventCategory from value: '" <> e
           <> "'. Accepted values: insight"

instance ToText EventCategory where
    toText = \case
        Insight -> "insight"

instance Hashable     EventCategory
instance NFData       EventCategory
instance ToByteString EventCategory
instance ToQuery      EventCategory
instance ToHeader     EventCategory

instance ToJSON EventCategory where
    toJSON = toJSONText

data InsightType =
  APICallRateInsight
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InsightType where
    parser = takeLowerText >>= \case
        "apicallrateinsight" -> pure APICallRateInsight
        e -> fromTextError $ "Failure parsing InsightType from value: '" <> e
           <> "'. Accepted values: apicallrateinsight"

instance ToText InsightType where
    toText = \case
        APICallRateInsight -> "ApiCallRateInsight"

instance Hashable     InsightType
instance NFData       InsightType
instance ToByteString InsightType
instance ToQuery      InsightType
instance ToHeader     InsightType

instance ToJSON InsightType where
    toJSON = toJSONText

instance FromJSON InsightType where
    parseJSON = parseJSONText "InsightType"

data LookupAttributeKey
  = AccessKeyId
  | EventId
  | EventName
  | EventSource
  | ReadOnly
  | ResourceName
  | ResourceType
  | Username
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LookupAttributeKey where
    parser = takeLowerText >>= \case
        "accesskeyid" -> pure AccessKeyId
        "eventid" -> pure EventId
        "eventname" -> pure EventName
        "eventsource" -> pure EventSource
        "readonly" -> pure ReadOnly
        "resourcename" -> pure ResourceName
        "resourcetype" -> pure ResourceType
        "username" -> pure Username
        e -> fromTextError $ "Failure parsing LookupAttributeKey from value: '" <> e
           <> "'. Accepted values: accesskeyid, eventid, eventname, eventsource, readonly, resourcename, resourcetype, username"

instance ToText LookupAttributeKey where
    toText = \case
        AccessKeyId -> "AccessKeyId"
        EventId -> "EventId"
        EventName -> "EventName"
        EventSource -> "EventSource"
        ReadOnly -> "ReadOnly"
        ResourceName -> "ResourceName"
        ResourceType -> "ResourceType"
        Username -> "Username"

instance Hashable     LookupAttributeKey
instance NFData       LookupAttributeKey
instance ToByteString LookupAttributeKey
instance ToQuery      LookupAttributeKey
instance ToHeader     LookupAttributeKey

instance ToJSON LookupAttributeKey where
    toJSON = toJSONText

data ReadWriteType
  = RWTAll
  | RWTReadOnly
  | RWTWriteOnly
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReadWriteType where
    parser = takeLowerText >>= \case
        "all" -> pure RWTAll
        "readonly" -> pure RWTReadOnly
        "writeonly" -> pure RWTWriteOnly
        e -> fromTextError $ "Failure parsing ReadWriteType from value: '" <> e
           <> "'. Accepted values: all, readonly, writeonly"

instance ToText ReadWriteType where
    toText = \case
        RWTAll -> "All"
        RWTReadOnly -> "ReadOnly"
        RWTWriteOnly -> "WriteOnly"

instance Hashable     ReadWriteType
instance NFData       ReadWriteType
instance ToByteString ReadWriteType
instance ToQuery      ReadWriteType
instance ToHeader     ReadWriteType

instance ToJSON ReadWriteType where
    toJSON = toJSONText

instance FromJSON ReadWriteType where
    parseJSON = parseJSONText "ReadWriteType"

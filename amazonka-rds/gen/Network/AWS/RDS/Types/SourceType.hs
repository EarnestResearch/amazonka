{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.SourceType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.SourceType where

import Network.AWS.Prelude
  
data SourceType = DBCluster
                | DBClusterSnapshot
                | DBInstance
                | DBParameterGroup
                | DBSecurityGroup
                | DBSnapshot
                    deriving (Eq, Ord, Read, Show, Enum, Bounded, Data,
                              Typeable, Generic)

instance FromText SourceType where
    parser = takeLowerText >>= \case
        "db-cluster" -> pure DBCluster
        "db-cluster-snapshot" -> pure DBClusterSnapshot
        "db-instance" -> pure DBInstance
        "db-parameter-group" -> pure DBParameterGroup
        "db-security-group" -> pure DBSecurityGroup
        "db-snapshot" -> pure DBSnapshot
        e -> fromTextError $ "Failure parsing SourceType from value: '" <> e
           <> "'. Accepted values: db-cluster, db-cluster-snapshot, db-instance, db-parameter-group, db-security-group, db-snapshot"

instance ToText SourceType where
    toText = \case
        DBCluster -> "db-cluster"
        DBClusterSnapshot -> "db-cluster-snapshot"
        DBInstance -> "db-instance"
        DBParameterGroup -> "db-parameter-group"
        DBSecurityGroup -> "db-security-group"
        DBSnapshot -> "db-snapshot"

instance Hashable     SourceType
instance NFData       SourceType
instance ToByteString SourceType
instance ToQuery      SourceType
instance ToHeader     SourceType

instance FromXML SourceType where
    parseXML = parseXMLText "SourceType"

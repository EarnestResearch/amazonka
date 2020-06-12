{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.OrderableDBInstanceOption
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.OrderableDBInstanceOption where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.AvailabilityZone

-- | Contains a list of available options for a DB instance.
--
--
-- This data type is used as a response element in the 'DescribeOrderableDBInstanceOptions' action. 
--
--
-- /See:/ 'orderableDBInstanceOption' smart constructor.
data OrderableDBInstanceOption = OrderableDBInstanceOption'{_odioEngineVersion
                                                            :: !(Maybe Text),
                                                            _odioMinIOPSPerGib
                                                            :: !(Maybe Double),
                                                            _odioSupportsIAMDatabaseAuthentication
                                                            :: !(Maybe Bool),
                                                            _odioMinIOPSPerDBInstance
                                                            :: !(Maybe Int),
                                                            _odioMultiAZCapable
                                                            :: !(Maybe Bool),
                                                            _odioMaxStorageSize
                                                            :: !(Maybe Int),
                                                            _odioEngine ::
                                                            !(Maybe Text),
                                                            _odioMinStorageSize
                                                            :: !(Maybe Int),
                                                            _odioSupportsIOPS ::
                                                            !(Maybe Bool),
                                                            _odioSupportsPerformanceInsights
                                                            :: !(Maybe Bool),
                                                            _odioDBInstanceClass
                                                            :: !(Maybe Text),
                                                            _odioLicenseModel ::
                                                            !(Maybe Text),
                                                            _odioAvailabilityZones
                                                            ::
                                                            !(Maybe
                                                                [AvailabilityZone]),
                                                            _odioSupportsStorageEncryption
                                                            :: !(Maybe Bool),
                                                            _odioReadReplicaCapable
                                                            :: !(Maybe Bool),
                                                            _odioMaxIOPSPerGib
                                                            :: !(Maybe Double),
                                                            _odioVPC ::
                                                            !(Maybe Bool),
                                                            _odioSupportsEnhancedMonitoring
                                                            :: !(Maybe Bool),
                                                            _odioMaxIOPSPerDBInstance
                                                            :: !(Maybe Int),
                                                            _odioStorageType ::
                                                            !(Maybe Text)}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'OrderableDBInstanceOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'odioEngineVersion' - The engine version of a DB instance.
--
-- * 'odioMinIOPSPerGib' - Minimum provisioned IOPS per GiB for a DB instance.
--
-- * 'odioSupportsIAMDatabaseAuthentication' - Indicates whether a DB instance supports IAM database authentication.
--
-- * 'odioMinIOPSPerDBInstance' - Minimum total provisioned IOPS for a DB instance.
--
-- * 'odioMultiAZCapable' - Indicates whether a DB instance is Multi-AZ capable.
--
-- * 'odioMaxStorageSize' - Maximum storage size for a DB instance.
--
-- * 'odioEngine' - The engine type of a DB instance.
--
-- * 'odioMinStorageSize' - Minimum storage size for a DB instance.
--
-- * 'odioSupportsIOPS' - Indicates whether a DB instance supports provisioned IOPS.
--
-- * 'odioSupportsPerformanceInsights' - True if a DB instance supports Performance Insights, otherwise false.
--
-- * 'odioDBInstanceClass' - The DB instance class for a DB instance.
--
-- * 'odioLicenseModel' - The license model for a DB instance.
--
-- * 'odioAvailabilityZones' - A list of Availability Zones for a DB instance.
--
-- * 'odioSupportsStorageEncryption' - Indicates whether a DB instance supports encrypted storage.
--
-- * 'odioReadReplicaCapable' - Indicates whether a DB instance can have a Read Replica.
--
-- * 'odioMaxIOPSPerGib' - Maximum provisioned IOPS per GiB for a DB instance.
--
-- * 'odioVPC' - Indicates whether a DB instance is in a VPC.
--
-- * 'odioSupportsEnhancedMonitoring' - Indicates whether a DB instance supports Enhanced Monitoring at intervals from 1 to 60 seconds.
--
-- * 'odioMaxIOPSPerDBInstance' - Maximum total provisioned IOPS for a DB instance.
--
-- * 'odioStorageType' - Indicates the storage type for a DB instance.
orderableDBInstanceOption
    :: OrderableDBInstanceOption
orderableDBInstanceOption
  = OrderableDBInstanceOption'{_odioEngineVersion =
                                 Nothing,
                               _odioMinIOPSPerGib = Nothing,
                               _odioSupportsIAMDatabaseAuthentication = Nothing,
                               _odioMinIOPSPerDBInstance = Nothing,
                               _odioMultiAZCapable = Nothing,
                               _odioMaxStorageSize = Nothing,
                               _odioEngine = Nothing,
                               _odioMinStorageSize = Nothing,
                               _odioSupportsIOPS = Nothing,
                               _odioSupportsPerformanceInsights = Nothing,
                               _odioDBInstanceClass = Nothing,
                               _odioLicenseModel = Nothing,
                               _odioAvailabilityZones = Nothing,
                               _odioSupportsStorageEncryption = Nothing,
                               _odioReadReplicaCapable = Nothing,
                               _odioMaxIOPSPerGib = Nothing, _odioVPC = Nothing,
                               _odioSupportsEnhancedMonitoring = Nothing,
                               _odioMaxIOPSPerDBInstance = Nothing,
                               _odioStorageType = Nothing}

-- | The engine version of a DB instance.
odioEngineVersion :: Lens' OrderableDBInstanceOption (Maybe Text)
odioEngineVersion = lens _odioEngineVersion (\ s a -> s{_odioEngineVersion = a})

-- | Minimum provisioned IOPS per GiB for a DB instance.
odioMinIOPSPerGib :: Lens' OrderableDBInstanceOption (Maybe Double)
odioMinIOPSPerGib = lens _odioMinIOPSPerGib (\ s a -> s{_odioMinIOPSPerGib = a})

-- | Indicates whether a DB instance supports IAM database authentication.
odioSupportsIAMDatabaseAuthentication :: Lens' OrderableDBInstanceOption (Maybe Bool)
odioSupportsIAMDatabaseAuthentication = lens _odioSupportsIAMDatabaseAuthentication (\ s a -> s{_odioSupportsIAMDatabaseAuthentication = a})

-- | Minimum total provisioned IOPS for a DB instance.
odioMinIOPSPerDBInstance :: Lens' OrderableDBInstanceOption (Maybe Int)
odioMinIOPSPerDBInstance = lens _odioMinIOPSPerDBInstance (\ s a -> s{_odioMinIOPSPerDBInstance = a})

-- | Indicates whether a DB instance is Multi-AZ capable.
odioMultiAZCapable :: Lens' OrderableDBInstanceOption (Maybe Bool)
odioMultiAZCapable = lens _odioMultiAZCapable (\ s a -> s{_odioMultiAZCapable = a})

-- | Maximum storage size for a DB instance.
odioMaxStorageSize :: Lens' OrderableDBInstanceOption (Maybe Int)
odioMaxStorageSize = lens _odioMaxStorageSize (\ s a -> s{_odioMaxStorageSize = a})

-- | The engine type of a DB instance.
odioEngine :: Lens' OrderableDBInstanceOption (Maybe Text)
odioEngine = lens _odioEngine (\ s a -> s{_odioEngine = a})

-- | Minimum storage size for a DB instance.
odioMinStorageSize :: Lens' OrderableDBInstanceOption (Maybe Int)
odioMinStorageSize = lens _odioMinStorageSize (\ s a -> s{_odioMinStorageSize = a})

-- | Indicates whether a DB instance supports provisioned IOPS.
odioSupportsIOPS :: Lens' OrderableDBInstanceOption (Maybe Bool)
odioSupportsIOPS = lens _odioSupportsIOPS (\ s a -> s{_odioSupportsIOPS = a})

-- | True if a DB instance supports Performance Insights, otherwise false.
odioSupportsPerformanceInsights :: Lens' OrderableDBInstanceOption (Maybe Bool)
odioSupportsPerformanceInsights = lens _odioSupportsPerformanceInsights (\ s a -> s{_odioSupportsPerformanceInsights = a})

-- | The DB instance class for a DB instance.
odioDBInstanceClass :: Lens' OrderableDBInstanceOption (Maybe Text)
odioDBInstanceClass = lens _odioDBInstanceClass (\ s a -> s{_odioDBInstanceClass = a})

-- | The license model for a DB instance.
odioLicenseModel :: Lens' OrderableDBInstanceOption (Maybe Text)
odioLicenseModel = lens _odioLicenseModel (\ s a -> s{_odioLicenseModel = a})

-- | A list of Availability Zones for a DB instance.
odioAvailabilityZones :: Lens' OrderableDBInstanceOption [AvailabilityZone]
odioAvailabilityZones = lens _odioAvailabilityZones (\ s a -> s{_odioAvailabilityZones = a}) . _Default . _Coerce

-- | Indicates whether a DB instance supports encrypted storage.
odioSupportsStorageEncryption :: Lens' OrderableDBInstanceOption (Maybe Bool)
odioSupportsStorageEncryption = lens _odioSupportsStorageEncryption (\ s a -> s{_odioSupportsStorageEncryption = a})

-- | Indicates whether a DB instance can have a Read Replica.
odioReadReplicaCapable :: Lens' OrderableDBInstanceOption (Maybe Bool)
odioReadReplicaCapable = lens _odioReadReplicaCapable (\ s a -> s{_odioReadReplicaCapable = a})

-- | Maximum provisioned IOPS per GiB for a DB instance.
odioMaxIOPSPerGib :: Lens' OrderableDBInstanceOption (Maybe Double)
odioMaxIOPSPerGib = lens _odioMaxIOPSPerGib (\ s a -> s{_odioMaxIOPSPerGib = a})

-- | Indicates whether a DB instance is in a VPC.
odioVPC :: Lens' OrderableDBInstanceOption (Maybe Bool)
odioVPC = lens _odioVPC (\ s a -> s{_odioVPC = a})

-- | Indicates whether a DB instance supports Enhanced Monitoring at intervals from 1 to 60 seconds.
odioSupportsEnhancedMonitoring :: Lens' OrderableDBInstanceOption (Maybe Bool)
odioSupportsEnhancedMonitoring = lens _odioSupportsEnhancedMonitoring (\ s a -> s{_odioSupportsEnhancedMonitoring = a})

-- | Maximum total provisioned IOPS for a DB instance.
odioMaxIOPSPerDBInstance :: Lens' OrderableDBInstanceOption (Maybe Int)
odioMaxIOPSPerDBInstance = lens _odioMaxIOPSPerDBInstance (\ s a -> s{_odioMaxIOPSPerDBInstance = a})

-- | Indicates the storage type for a DB instance.
odioStorageType :: Lens' OrderableDBInstanceOption (Maybe Text)
odioStorageType = lens _odioStorageType (\ s a -> s{_odioStorageType = a})

instance FromXML OrderableDBInstanceOption where
        parseXML x
          = OrderableDBInstanceOption' <$>
              (x .@? "EngineVersion") <*> (x .@? "MinIopsPerGib")
                <*> (x .@? "SupportsIAMDatabaseAuthentication")
                <*> (x .@? "MinIopsPerDbInstance")
                <*> (x .@? "MultiAZCapable")
                <*> (x .@? "MaxStorageSize")
                <*> (x .@? "Engine")
                <*> (x .@? "MinStorageSize")
                <*> (x .@? "SupportsIops")
                <*> (x .@? "SupportsPerformanceInsights")
                <*> (x .@? "DBInstanceClass")
                <*> (x .@? "LicenseModel")
                <*>
                (x .@? "AvailabilityZones" .!@ mempty >>=
                   may (parseXMLList "AvailabilityZone"))
                <*> (x .@? "SupportsStorageEncryption")
                <*> (x .@? "ReadReplicaCapable")
                <*> (x .@? "MaxIopsPerGib")
                <*> (x .@? "Vpc")
                <*> (x .@? "SupportsEnhancedMonitoring")
                <*> (x .@? "MaxIopsPerDbInstance")
                <*> (x .@? "StorageType")

instance Hashable OrderableDBInstanceOption where

instance NFData OrderableDBInstanceOption where

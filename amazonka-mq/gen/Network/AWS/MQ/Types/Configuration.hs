{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.Configuration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MQ.Types.Configuration where

import Network.AWS.Lens
import Network.AWS.MQ.Types.ConfigurationRevision
import Network.AWS.MQ.Types.EngineType
import Network.AWS.Prelude

-- | Returns information about all configurations.
--
-- /See:/ 'configuration' smart constructor.
data Configuration = Configuration'{_cEngineVersion
                                    :: !(Maybe Text),
                                    _cARN :: !(Maybe Text),
                                    _cLatestRevision ::
                                    !(Maybe ConfigurationRevision),
                                    _cName :: !(Maybe Text),
                                    _cId :: !(Maybe Text),
                                    _cDescription :: !(Maybe Text),
                                    _cEngineType :: !(Maybe EngineType)}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Configuration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cEngineVersion' - Required. The version of the broker engine.
--
-- * 'cARN' - Required. The ARN of the configuration.
--
-- * 'cLatestRevision' - Required. The latest revision of the configuration.
--
-- * 'cName' - Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
--
-- * 'cId' - Required. The unique ID that Amazon MQ generates for the configuration.
--
-- * 'cDescription' - Required. The description of the configuration.
--
-- * 'cEngineType' - Required. The type of broker engine. Note: Currently, Amazon MQ supports only ACTIVEMQ.
configuration
    :: Configuration
configuration
  = Configuration'{_cEngineVersion = Nothing,
                   _cARN = Nothing, _cLatestRevision = Nothing,
                   _cName = Nothing, _cId = Nothing,
                   _cDescription = Nothing, _cEngineType = Nothing}

-- | Required. The version of the broker engine.
cEngineVersion :: Lens' Configuration (Maybe Text)
cEngineVersion = lens _cEngineVersion (\ s a -> s{_cEngineVersion = a})

-- | Required. The ARN of the configuration.
cARN :: Lens' Configuration (Maybe Text)
cARN = lens _cARN (\ s a -> s{_cARN = a})

-- | Required. The latest revision of the configuration.
cLatestRevision :: Lens' Configuration (Maybe ConfigurationRevision)
cLatestRevision = lens _cLatestRevision (\ s a -> s{_cLatestRevision = a})

-- | Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
cName :: Lens' Configuration (Maybe Text)
cName = lens _cName (\ s a -> s{_cName = a})

-- | Required. The unique ID that Amazon MQ generates for the configuration.
cId :: Lens' Configuration (Maybe Text)
cId = lens _cId (\ s a -> s{_cId = a})

-- | Required. The description of the configuration.
cDescription :: Lens' Configuration (Maybe Text)
cDescription = lens _cDescription (\ s a -> s{_cDescription = a})

-- | Required. The type of broker engine. Note: Currently, Amazon MQ supports only ACTIVEMQ.
cEngineType :: Lens' Configuration (Maybe EngineType)
cEngineType = lens _cEngineType (\ s a -> s{_cEngineType = a})

instance FromJSON Configuration where
        parseJSON
          = withObject "Configuration"
              (\ x ->
                 Configuration' <$>
                   (x .:? "engineVersion") <*> (x .:? "arn") <*>
                     (x .:? "latestRevision")
                     <*> (x .:? "name")
                     <*> (x .:? "id")
                     <*> (x .:? "description")
                     <*> (x .:? "engineType"))

instance Hashable Configuration where

instance NFData Configuration where

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ContainerOverride
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.ContainerOverride where

import Network.AWS.ECS.Types.KeyValuePair
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The overrides that should be sent to a container.
--
--
--
-- /See:/ 'containerOverride' smart constructor.
data ContainerOverride = ContainerOverride'{_coCommand
                                            :: !(Maybe [Text]),
                                            _coEnvironment ::
                                            !(Maybe [KeyValuePair]),
                                            _coMemory :: !(Maybe Int),
                                            _coName :: !(Maybe Text),
                                            _coCpu :: !(Maybe Int),
                                            _coMemoryReservation ::
                                            !(Maybe Int)}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContainerOverride' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coCommand' - The command to send to the container that overrides the default command from the Docker image or the task definition. You must also specify a container name.
--
-- * 'coEnvironment' - The environment variables to send to the container. You can add new environment variables, which are added to the container at launch, or you can override the existing environment variables from the Docker image or the task definition. You must also specify a container name.
--
-- * 'coMemory' - The hard limit (in MiB) of memory to present to the container, instead of the default value from the task definition. If your container attempts to exceed the memory specified here, the container is killed. You must also specify a container name.
--
-- * 'coName' - The name of the container that receives the override. This parameter is required if any override is specified.
--
-- * 'coCpu' - The number of @cpu@ units reserved for the container, instead of the default value from the task definition. You must also specify a container name.
--
-- * 'coMemoryReservation' - The soft limit (in MiB) of memory to reserve for the container, instead of the default value from the task definition. You must also specify a container name.
containerOverride
    :: ContainerOverride
containerOverride
  = ContainerOverride'{_coCommand = Nothing,
                       _coEnvironment = Nothing, _coMemory = Nothing,
                       _coName = Nothing, _coCpu = Nothing,
                       _coMemoryReservation = Nothing}

-- | The command to send to the container that overrides the default command from the Docker image or the task definition. You must also specify a container name.
coCommand :: Lens' ContainerOverride [Text]
coCommand = lens _coCommand (\ s a -> s{_coCommand = a}) . _Default . _Coerce

-- | The environment variables to send to the container. You can add new environment variables, which are added to the container at launch, or you can override the existing environment variables from the Docker image or the task definition. You must also specify a container name.
coEnvironment :: Lens' ContainerOverride [KeyValuePair]
coEnvironment = lens _coEnvironment (\ s a -> s{_coEnvironment = a}) . _Default . _Coerce

-- | The hard limit (in MiB) of memory to present to the container, instead of the default value from the task definition. If your container attempts to exceed the memory specified here, the container is killed. You must also specify a container name.
coMemory :: Lens' ContainerOverride (Maybe Int)
coMemory = lens _coMemory (\ s a -> s{_coMemory = a})

-- | The name of the container that receives the override. This parameter is required if any override is specified.
coName :: Lens' ContainerOverride (Maybe Text)
coName = lens _coName (\ s a -> s{_coName = a})

-- | The number of @cpu@ units reserved for the container, instead of the default value from the task definition. You must also specify a container name.
coCpu :: Lens' ContainerOverride (Maybe Int)
coCpu = lens _coCpu (\ s a -> s{_coCpu = a})

-- | The soft limit (in MiB) of memory to reserve for the container, instead of the default value from the task definition. You must also specify a container name.
coMemoryReservation :: Lens' ContainerOverride (Maybe Int)
coMemoryReservation = lens _coMemoryReservation (\ s a -> s{_coMemoryReservation = a})

instance FromJSON ContainerOverride where
        parseJSON
          = withObject "ContainerOverride"
              (\ x ->
                 ContainerOverride' <$>
                   (x .:? "command" .!= mempty) <*>
                     (x .:? "environment" .!= mempty)
                     <*> (x .:? "memory")
                     <*> (x .:? "name")
                     <*> (x .:? "cpu")
                     <*> (x .:? "memoryReservation"))

instance Hashable ContainerOverride where

instance NFData ContainerOverride where

instance ToJSON ContainerOverride where
        toJSON ContainerOverride'{..}
          = object
              (catMaybes
                 [("command" .=) <$> _coCommand,
                  ("environment" .=) <$> _coEnvironment,
                  ("memory" .=) <$> _coMemory, ("name" .=) <$> _coName,
                  ("cpu" .=) <$> _coCpu,
                  ("memoryReservation" .=) <$> _coMemoryReservation])

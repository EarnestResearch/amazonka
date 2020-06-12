{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.Target
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.Target where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An array of search criteria that targets instances using a Key,Value combination that you specify. @Targets@ is required if you don't provide one or more instance IDs in the call.
--
--
--
--
--
-- /See:/ 'target' smart constructor.
data Target = Target'{_tValues :: !(Maybe [Text]),
                      _tKey :: !(Maybe Text)}
                deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Target' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tValues' - User-defined criteria that maps to Key. For example, if you specified tag:ServerRole, you could specify value:WebServer to execute a command on instances that include Amazon EC2 tags of ServerRole,WebServer. For more information about how to send commands that target instances using Key,Value parameters, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Executing a Command Using Systems Manager Run Command> .
--
-- * 'tKey' - User-defined criteria for sending commands that target instances that meet the criteria. Key can be tag:<Amazon EC2 tag> or InstanceIds. For more information about how to send commands that target instances using Key,Value parameters, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Executing a Command Using Systems Manager Run Command> .
target
    :: Target
target = Target'{_tValues = Nothing, _tKey = Nothing}

-- | User-defined criteria that maps to Key. For example, if you specified tag:ServerRole, you could specify value:WebServer to execute a command on instances that include Amazon EC2 tags of ServerRole,WebServer. For more information about how to send commands that target instances using Key,Value parameters, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Executing a Command Using Systems Manager Run Command> .
tValues :: Lens' Target [Text]
tValues = lens _tValues (\ s a -> s{_tValues = a}) . _Default . _Coerce

-- | User-defined criteria for sending commands that target instances that meet the criteria. Key can be tag:<Amazon EC2 tag> or InstanceIds. For more information about how to send commands that target instances using Key,Value parameters, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Executing a Command Using Systems Manager Run Command> .
tKey :: Lens' Target (Maybe Text)
tKey = lens _tKey (\ s a -> s{_tKey = a})

instance FromJSON Target where
        parseJSON
          = withObject "Target"
              (\ x ->
                 Target' <$>
                   (x .:? "Values" .!= mempty) <*> (x .:? "Key"))

instance Hashable Target where

instance NFData Target where

instance ToJSON Target where
        toJSON Target'{..}
          = object
              (catMaybes
                 [("Values" .=) <$> _tValues, ("Key" .=) <$> _tKey])

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.SoftwareTokenMFASettingsType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.SoftwareTokenMFASettingsType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The type used for enabling software token MFA at the user level.
--
--
--
-- /See:/ 'softwareTokenMFASettingsType' smart constructor.
data SoftwareTokenMFASettingsType = SoftwareTokenMFASettingsType'{_stmstEnabled
                                                                  ::
                                                                  !(Maybe Bool),
                                                                  _stmstPreferredMFA
                                                                  ::
                                                                  !(Maybe Bool)}
                                      deriving (Eq, Read, Show, Data, Typeable,
                                                Generic)

-- | Creates a value of 'SoftwareTokenMFASettingsType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stmstEnabled' - Specifies whether software token MFA is enabled.
--
-- * 'stmstPreferredMFA' - Specifies whether software token MFA is the preferred MFA method.
softwareTokenMFASettingsType
    :: SoftwareTokenMFASettingsType
softwareTokenMFASettingsType
  = SoftwareTokenMFASettingsType'{_stmstEnabled =
                                    Nothing,
                                  _stmstPreferredMFA = Nothing}

-- | Specifies whether software token MFA is enabled.
stmstEnabled :: Lens' SoftwareTokenMFASettingsType (Maybe Bool)
stmstEnabled = lens _stmstEnabled (\ s a -> s{_stmstEnabled = a})

-- | Specifies whether software token MFA is the preferred MFA method.
stmstPreferredMFA :: Lens' SoftwareTokenMFASettingsType (Maybe Bool)
stmstPreferredMFA = lens _stmstPreferredMFA (\ s a -> s{_stmstPreferredMFA = a})

instance Hashable SoftwareTokenMFASettingsType where

instance NFData SoftwareTokenMFASettingsType where

instance ToJSON SoftwareTokenMFASettingsType where
        toJSON SoftwareTokenMFASettingsType'{..}
          = object
              (catMaybes
                 [("Enabled" .=) <$> _stmstEnabled,
                  ("PreferredMfa" .=) <$> _stmstPreferredMFA])

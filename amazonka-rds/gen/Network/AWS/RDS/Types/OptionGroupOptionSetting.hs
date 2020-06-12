{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.OptionGroupOptionSetting
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.OptionGroupOptionSetting where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Option group option settings are used to display settings available for each option with their default values and other information. These values are used with the DescribeOptionGroupOptions action.
--
--
--
-- /See:/ 'optionGroupOptionSetting' smart constructor.
data OptionGroupOptionSetting = OptionGroupOptionSetting'{_ogosApplyType
                                                          :: !(Maybe Text),
                                                          _ogosSettingName ::
                                                          !(Maybe Text),
                                                          _ogosDefaultValue ::
                                                          !(Maybe Text),
                                                          _ogosIsModifiable ::
                                                          !(Maybe Bool),
                                                          _ogosSettingDescription
                                                          :: !(Maybe Text),
                                                          _ogosAllowedValues ::
                                                          !(Maybe Text)}
                                  deriving (Eq, Read, Show, Data, Typeable,
                                            Generic)

-- | Creates a value of 'OptionGroupOptionSetting' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogosApplyType' - The DB engine specific parameter type for the option group option.
--
-- * 'ogosSettingName' - The name of the option group option.
--
-- * 'ogosDefaultValue' - The default value for the option group option.
--
-- * 'ogosIsModifiable' - Boolean value where true indicates that this option group option can be changed from the default value.
--
-- * 'ogosSettingDescription' - The description of the option group option.
--
-- * 'ogosAllowedValues' - Indicates the acceptable values for the option group option.
optionGroupOptionSetting
    :: OptionGroupOptionSetting
optionGroupOptionSetting
  = OptionGroupOptionSetting'{_ogosApplyType = Nothing,
                              _ogosSettingName = Nothing,
                              _ogosDefaultValue = Nothing,
                              _ogosIsModifiable = Nothing,
                              _ogosSettingDescription = Nothing,
                              _ogosAllowedValues = Nothing}

-- | The DB engine specific parameter type for the option group option.
ogosApplyType :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosApplyType = lens _ogosApplyType (\ s a -> s{_ogosApplyType = a})

-- | The name of the option group option.
ogosSettingName :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosSettingName = lens _ogosSettingName (\ s a -> s{_ogosSettingName = a})

-- | The default value for the option group option.
ogosDefaultValue :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosDefaultValue = lens _ogosDefaultValue (\ s a -> s{_ogosDefaultValue = a})

-- | Boolean value where true indicates that this option group option can be changed from the default value.
ogosIsModifiable :: Lens' OptionGroupOptionSetting (Maybe Bool)
ogosIsModifiable = lens _ogosIsModifiable (\ s a -> s{_ogosIsModifiable = a})

-- | The description of the option group option.
ogosSettingDescription :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosSettingDescription = lens _ogosSettingDescription (\ s a -> s{_ogosSettingDescription = a})

-- | Indicates the acceptable values for the option group option.
ogosAllowedValues :: Lens' OptionGroupOptionSetting (Maybe Text)
ogosAllowedValues = lens _ogosAllowedValues (\ s a -> s{_ogosAllowedValues = a})

instance FromXML OptionGroupOptionSetting where
        parseXML x
          = OptionGroupOptionSetting' <$>
              (x .@? "ApplyType") <*> (x .@? "SettingName") <*>
                (x .@? "DefaultValue")
                <*> (x .@? "IsModifiable")
                <*> (x .@? "SettingDescription")
                <*> (x .@? "AllowedValues")

instance Hashable OptionGroupOptionSetting where

instance NFData OptionGroupOptionSetting where

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAccelerator
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchTemplateElasticInferenceAccelerator where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an elastic inference accelerator. 
--
--
--
-- /See:/ 'launchTemplateElasticInferenceAccelerator' smart constructor.
data LaunchTemplateElasticInferenceAccelerator = LaunchTemplateElasticInferenceAccelerator'{_lteiaCount
                                                                                            ::
                                                                                            !(Maybe
                                                                                                Nat),
                                                                                            _lteiaType
                                                                                            ::
                                                                                            !Text}
                                                   deriving (Eq, Read, Show,
                                                             Data, Typeable,
                                                             Generic)

-- | Creates a value of 'LaunchTemplateElasticInferenceAccelerator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lteiaCount' - The number of elastic inference accelerators to attach to the instance.  Default: 1
--
-- * 'lteiaType' - The type of elastic inference accelerator. The possible values are eia1.medium, eia1.large, and eia1.xlarge. 
launchTemplateElasticInferenceAccelerator
    :: Text -- ^ 'lteiaType'
    -> LaunchTemplateElasticInferenceAccelerator
launchTemplateElasticInferenceAccelerator pType_
  = LaunchTemplateElasticInferenceAccelerator'{_lteiaCount
                                                 = Nothing,
                                               _lteiaType = pType_}

-- | The number of elastic inference accelerators to attach to the instance.  Default: 1
lteiaCount :: Lens' LaunchTemplateElasticInferenceAccelerator (Maybe Natural)
lteiaCount = lens _lteiaCount (\ s a -> s{_lteiaCount = a}) . mapping _Nat

-- | The type of elastic inference accelerator. The possible values are eia1.medium, eia1.large, and eia1.xlarge. 
lteiaType :: Lens' LaunchTemplateElasticInferenceAccelerator Text
lteiaType = lens _lteiaType (\ s a -> s{_lteiaType = a})

instance Hashable
           LaunchTemplateElasticInferenceAccelerator
         where

instance NFData
           LaunchTemplateElasticInferenceAccelerator
         where

instance ToQuery
           LaunchTemplateElasticInferenceAccelerator
         where
        toQuery
          LaunchTemplateElasticInferenceAccelerator'{..}
          = mconcat
              ["Count" =: _lteiaCount, "Type" =: _lteiaType]

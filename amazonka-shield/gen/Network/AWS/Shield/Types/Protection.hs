{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.Protection
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types.Protection where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that represents a resource that is under DDoS protection.
--
--
--
-- /See:/ 'protection' smart constructor.
data Protection = Protection'{_pResourceARN ::
                              !(Maybe Text),
                              _pName :: !(Maybe Text), _pId :: !(Maybe Text)}
                    deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Protection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pResourceARN' - The ARN (Amazon Resource Name) of the AWS resource that is protected.
--
-- * 'pName' - The friendly name of the protection. For example, @My CloudFront distributions@ .
--
-- * 'pId' - The unique identifier (ID) of the protection.
protection
    :: Protection
protection
  = Protection'{_pResourceARN = Nothing,
                _pName = Nothing, _pId = Nothing}

-- | The ARN (Amazon Resource Name) of the AWS resource that is protected.
pResourceARN :: Lens' Protection (Maybe Text)
pResourceARN = lens _pResourceARN (\ s a -> s{_pResourceARN = a})

-- | The friendly name of the protection. For example, @My CloudFront distributions@ .
pName :: Lens' Protection (Maybe Text)
pName = lens _pName (\ s a -> s{_pName = a})

-- | The unique identifier (ID) of the protection.
pId :: Lens' Protection (Maybe Text)
pId = lens _pId (\ s a -> s{_pId = a})

instance FromJSON Protection where
        parseJSON
          = withObject "Protection"
              (\ x ->
                 Protection' <$>
                   (x .:? "ResourceArn") <*> (x .:? "Name") <*>
                     (x .:? "Id"))

instance Hashable Protection where

instance NFData Protection where

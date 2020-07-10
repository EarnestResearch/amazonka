{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase2DHGroupNumbersRequestListValue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.Phase2DHGroupNumbersRequestListValue where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies a Diffie-Hellman group number for the VPN tunnel for phase 2 IKE negotiations.
--
--
--
-- /See:/ 'phase2DHGroupNumbersRequestListValue' smart constructor.
newtype Phase2DHGroupNumbersRequestListValue = Phase2DHGroupNumbersRequestListValue'{_phs2dhgrpnmbrsrqstlstvlValue
                                                                                     ::
                                                                                     Maybe
                                                                                       Int}
                                                 deriving (Eq, Read, Show, Data,
                                                           Typeable, Generic)

-- | Creates a value of 'Phase2DHGroupNumbersRequestListValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'phs2dhgrpnmbrsrqstlstvlValue' - The Diffie-Hellmann group number.
phase2DHGroupNumbersRequestListValue
    :: Phase2DHGroupNumbersRequestListValue
phase2DHGroupNumbersRequestListValue
  = Phase2DHGroupNumbersRequestListValue'{_phs2dhgrpnmbrsrqstlstvlValue
                                            = Nothing}

-- | The Diffie-Hellmann group number.
phs2dhgrpnmbrsrqstlstvlValue :: Lens' Phase2DHGroupNumbersRequestListValue (Maybe Int)
phs2dhgrpnmbrsrqstlstvlValue = lens _phs2dhgrpnmbrsrqstlstvlValue (\ s a -> s{_phs2dhgrpnmbrsrqstlstvlValue = a})

instance Hashable
           Phase2DHGroupNumbersRequestListValue
         where

instance NFData Phase2DHGroupNumbersRequestListValue
         where

instance ToQuery Phase2DHGroupNumbersRequestListValue
         where
        toQuery Phase2DHGroupNumbersRequestListValue'{..}
          = mconcat ["Value" =: _phs2dhgrpnmbrsrqstlstvlValue]

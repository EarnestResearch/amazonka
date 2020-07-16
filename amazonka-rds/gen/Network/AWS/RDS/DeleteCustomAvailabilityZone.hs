{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteCustomAvailabilityZone
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a custom Availability Zone (AZ).
--
--
-- A custom AZ is an on-premises AZ that is integrated with a VMware vSphere cluster.
--
-- For more information about RDS on VMware, see the <https://docs.aws.amazon.com/AmazonRDS/latest/RDSonVMwareUserGuide/rds-on-vmware.html /RDS on VMware User Guide./ > 
--
module Network.AWS.RDS.DeleteCustomAvailabilityZone
    (
    -- * Creating a Request
      deleteCustomAvailabilityZone
    , DeleteCustomAvailabilityZone
    -- * Request Lenses
    , dCustomAvailabilityZoneId

    -- * Destructuring the Response
    , deleteCustomAvailabilityZoneResponse
    , DeleteCustomAvailabilityZoneResponse
    -- * Response Lenses
    , dltcstmavlbltyznrsCustomAvailabilityZone
    , dltcstmavlbltyznrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteCustomAvailabilityZone' smart constructor.
newtype DeleteCustomAvailabilityZone = DeleteCustomAvailabilityZone'{_dCustomAvailabilityZoneId
                                                                     :: Text}
                                         deriving (Eq, Read, Show, Data,
                                                   Typeable, Generic)

-- | Creates a value of 'DeleteCustomAvailabilityZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dCustomAvailabilityZoneId' - The custom AZ identifier.
deleteCustomAvailabilityZone
    :: Text -- ^ 'dCustomAvailabilityZoneId'
    -> DeleteCustomAvailabilityZone
deleteCustomAvailabilityZone
  pCustomAvailabilityZoneId_
  = DeleteCustomAvailabilityZone'{_dCustomAvailabilityZoneId
                                    = pCustomAvailabilityZoneId_}

-- | The custom AZ identifier.
dCustomAvailabilityZoneId :: Lens' DeleteCustomAvailabilityZone Text
dCustomAvailabilityZoneId = lens _dCustomAvailabilityZoneId (\ s a -> s{_dCustomAvailabilityZoneId = a})

instance AWSRequest DeleteCustomAvailabilityZone
         where
        type Rs DeleteCustomAvailabilityZone =
             DeleteCustomAvailabilityZoneResponse
        request = postQuery rds
        response
          = receiveXMLWrapper
              "DeleteCustomAvailabilityZoneResult"
              (\ s h x ->
                 DeleteCustomAvailabilityZoneResponse' <$>
                   (x .@? "CustomAvailabilityZone") <*>
                     (pure (fromEnum s)))

instance Hashable DeleteCustomAvailabilityZone where

instance NFData DeleteCustomAvailabilityZone where

instance ToHeaders DeleteCustomAvailabilityZone where
        toHeaders = const mempty

instance ToPath DeleteCustomAvailabilityZone where
        toPath = const "/"

instance ToQuery DeleteCustomAvailabilityZone where
        toQuery DeleteCustomAvailabilityZone'{..}
          = mconcat
              ["Action" =:
                 ("DeleteCustomAvailabilityZone" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "CustomAvailabilityZoneId" =:
                 _dCustomAvailabilityZoneId]

-- | /See:/ 'deleteCustomAvailabilityZoneResponse' smart constructor.
data DeleteCustomAvailabilityZoneResponse = DeleteCustomAvailabilityZoneResponse'{_dltcstmavlbltyznrsCustomAvailabilityZone
                                                                                  ::
                                                                                  !(Maybe
                                                                                      CustomAvailabilityZone),
                                                                                  _dltcstmavlbltyznrsResponseStatus
                                                                                  ::
                                                                                  !Int}
                                              deriving (Eq, Show, Data,
                                                        Typeable, Generic)

-- | Creates a value of 'DeleteCustomAvailabilityZoneResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltcstmavlbltyznrsCustomAvailabilityZone' - Undocumented member.
--
-- * 'dltcstmavlbltyznrsResponseStatus' - -- | The response status code.
deleteCustomAvailabilityZoneResponse
    :: Int -- ^ 'dltcstmavlbltyznrsResponseStatus'
    -> DeleteCustomAvailabilityZoneResponse
deleteCustomAvailabilityZoneResponse pResponseStatus_
  = DeleteCustomAvailabilityZoneResponse'{_dltcstmavlbltyznrsCustomAvailabilityZone
                                            = Nothing,
                                          _dltcstmavlbltyznrsResponseStatus =
                                            pResponseStatus_}

-- | Undocumented member.
dltcstmavlbltyznrsCustomAvailabilityZone :: Lens' DeleteCustomAvailabilityZoneResponse (Maybe CustomAvailabilityZone)
dltcstmavlbltyznrsCustomAvailabilityZone = lens _dltcstmavlbltyznrsCustomAvailabilityZone (\ s a -> s{_dltcstmavlbltyznrsCustomAvailabilityZone = a})

-- | -- | The response status code.
dltcstmavlbltyznrsResponseStatus :: Lens' DeleteCustomAvailabilityZoneResponse Int
dltcstmavlbltyznrsResponseStatus = lens _dltcstmavlbltyznrsResponseStatus (\ s a -> s{_dltcstmavlbltyznrsResponseStatus = a})

instance NFData DeleteCustomAvailabilityZoneResponse
         where

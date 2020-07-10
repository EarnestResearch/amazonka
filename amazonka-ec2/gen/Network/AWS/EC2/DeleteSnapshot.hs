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
-- Module      : Network.AWS.EC2.DeleteSnapshot
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified snapshot.
--
--
-- When you make periodic snapshots of a volume, the snapshots are incremental, and only the blocks on the device that have changed since your last snapshot are saved in the new snapshot. When you delete a snapshot, only the data not needed for any other snapshot is removed. So regardless of which prior snapshots have been deleted, all active snapshots will have access to all the information needed to restore the volume.
--
-- You cannot delete a snapshot of the root device of an EBS volume used by a registered AMI. You must first de-register the AMI before you can delete the snapshot.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-deleting-snapshot.html Deleting an Amazon EBS Snapshot> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.DeleteSnapshot
    (
    -- * Creating a Request
      deleteSnapshot
    , DeleteSnapshot
    -- * Request Lenses
    , dltsnpshtDryRun
    , dltsnpshtSnapshotId

    -- * Destructuring the Response
    , deleteSnapshotResponse
    , DeleteSnapshotResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteSnapshot' smart constructor.
data DeleteSnapshot = DeleteSnapshot'{_dltsnpshtDryRun
                                      :: !(Maybe Bool),
                                      _dltsnpshtSnapshotId :: !Text}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltsnpshtDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dltsnpshtSnapshotId' - The ID of the EBS snapshot.
deleteSnapshot
    :: Text -- ^ 'dltsnpshtSnapshotId'
    -> DeleteSnapshot
deleteSnapshot pSnapshotId_
  = DeleteSnapshot'{_dltsnpshtDryRun = Nothing,
                    _dltsnpshtSnapshotId = pSnapshotId_}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dltsnpshtDryRun :: Lens' DeleteSnapshot (Maybe Bool)
dltsnpshtDryRun = lens _dltsnpshtDryRun (\ s a -> s{_dltsnpshtDryRun = a})

-- | The ID of the EBS snapshot.
dltsnpshtSnapshotId :: Lens' DeleteSnapshot Text
dltsnpshtSnapshotId = lens _dltsnpshtSnapshotId (\ s a -> s{_dltsnpshtSnapshotId = a})

instance AWSRequest DeleteSnapshot where
        type Rs DeleteSnapshot = DeleteSnapshotResponse
        request = postQuery ec2
        response = receiveNull DeleteSnapshotResponse'

instance Hashable DeleteSnapshot where

instance NFData DeleteSnapshot where

instance ToHeaders DeleteSnapshot where
        toHeaders = const mempty

instance ToPath DeleteSnapshot where
        toPath = const "/"

instance ToQuery DeleteSnapshot where
        toQuery DeleteSnapshot'{..}
          = mconcat
              ["Action" =: ("DeleteSnapshot" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dltsnpshtDryRun,
               "SnapshotId" =: _dltsnpshtSnapshotId]

-- | /See:/ 'deleteSnapshotResponse' smart constructor.
data DeleteSnapshotResponse = DeleteSnapshotResponse'
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'DeleteSnapshotResponse' with the minimum fields required to make a request.
--
deleteSnapshotResponse
    :: DeleteSnapshotResponse
deleteSnapshotResponse = DeleteSnapshotResponse'

instance NFData DeleteSnapshotResponse where

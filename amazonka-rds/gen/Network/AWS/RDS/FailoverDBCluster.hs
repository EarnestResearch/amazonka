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
-- Module      : Network.AWS.RDS.FailoverDBCluster
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Forces a failover for a DB cluster.
--
--
-- A failover for a DB cluster promotes one of the Aurora Replicas (read-only instances) in the DB cluster to be the primary instance (the cluster writer).
--
-- Amazon Aurora will automatically fail over to an Aurora Replica, if one exists, when the primary instance fails. You can force a failover when you want to simulate a failure of a primary instance for testing. Because each instance in a DB cluster has its own endpoint address, you will need to clean up and re-establish any existing connections that use those endpoint addresses when the failover is complete.
--
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./ 
--
module Network.AWS.RDS.FailoverDBCluster
    (
    -- * Creating a Request
      failoverDBCluster
    , FailoverDBCluster
    -- * Request Lenses
    , fdcTargetDBInstanceIdentifier
    , fdcDBClusterIdentifier

    -- * Destructuring the Response
    , failoverDBClusterResponse
    , FailoverDBClusterResponse
    -- * Response Lenses
    , fdcrsDBCluster
    , fdcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | 
--
--
--
-- /See:/ 'failoverDBCluster' smart constructor.
data FailoverDBCluster = FailoverDBCluster'{_fdcTargetDBInstanceIdentifier
                                            :: !(Maybe Text),
                                            _fdcDBClusterIdentifier :: !Text}
                           deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FailoverDBCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fdcTargetDBInstanceIdentifier' - The name of the instance to promote to the primary instance. You must specify the instance identifier for an Aurora Replica in the DB cluster. For example, @mydbcluster-replica1@ .
--
-- * 'fdcDBClusterIdentifier' - A DB cluster identifier to force a failover for. This parameter isn't case-sensitive. Constraints:     * Must match the identifier of an existing DBCluster.
failoverDBCluster
    :: Text -- ^ 'fdcDBClusterIdentifier'
    -> FailoverDBCluster
failoverDBCluster pDBClusterIdentifier_
  = FailoverDBCluster'{_fdcTargetDBInstanceIdentifier =
                         Nothing,
                       _fdcDBClusterIdentifier = pDBClusterIdentifier_}

-- | The name of the instance to promote to the primary instance. You must specify the instance identifier for an Aurora Replica in the DB cluster. For example, @mydbcluster-replica1@ .
fdcTargetDBInstanceIdentifier :: Lens' FailoverDBCluster (Maybe Text)
fdcTargetDBInstanceIdentifier = lens _fdcTargetDBInstanceIdentifier (\ s a -> s{_fdcTargetDBInstanceIdentifier = a})

-- | A DB cluster identifier to force a failover for. This parameter isn't case-sensitive. Constraints:     * Must match the identifier of an existing DBCluster.
fdcDBClusterIdentifier :: Lens' FailoverDBCluster Text
fdcDBClusterIdentifier = lens _fdcDBClusterIdentifier (\ s a -> s{_fdcDBClusterIdentifier = a})

instance AWSRequest FailoverDBCluster where
        type Rs FailoverDBCluster = FailoverDBClusterResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "FailoverDBClusterResult"
              (\ s h x ->
                 FailoverDBClusterResponse' <$>
                   (x .@? "DBCluster") <*> (pure (fromEnum s)))

instance Hashable FailoverDBCluster where

instance NFData FailoverDBCluster where

instance ToHeaders FailoverDBCluster where
        toHeaders = const mempty

instance ToPath FailoverDBCluster where
        toPath = const "/"

instance ToQuery FailoverDBCluster where
        toQuery FailoverDBCluster'{..}
          = mconcat
              ["Action" =: ("FailoverDBCluster" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "TargetDBInstanceIdentifier" =:
                 _fdcTargetDBInstanceIdentifier,
               "DBClusterIdentifier" =: _fdcDBClusterIdentifier]

-- | /See:/ 'failoverDBClusterResponse' smart constructor.
data FailoverDBClusterResponse = FailoverDBClusterResponse'{_fdcrsDBCluster
                                                            ::
                                                            !(Maybe DBCluster),
                                                            _fdcrsResponseStatus
                                                            :: !Int}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'FailoverDBClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fdcrsDBCluster' - Undocumented member.
--
-- * 'fdcrsResponseStatus' - -- | The response status code.
failoverDBClusterResponse
    :: Int -- ^ 'fdcrsResponseStatus'
    -> FailoverDBClusterResponse
failoverDBClusterResponse pResponseStatus_
  = FailoverDBClusterResponse'{_fdcrsDBCluster =
                                 Nothing,
                               _fdcrsResponseStatus = pResponseStatus_}

-- | Undocumented member.
fdcrsDBCluster :: Lens' FailoverDBClusterResponse (Maybe DBCluster)
fdcrsDBCluster = lens _fdcrsDBCluster (\ s a -> s{_fdcrsDBCluster = a})

-- | -- | The response status code.
fdcrsResponseStatus :: Lens' FailoverDBClusterResponse Int
fdcrsResponseStatus = lens _fdcrsResponseStatus (\ s a -> s{_fdcrsResponseStatus = a})

instance NFData FailoverDBClusterResponse where

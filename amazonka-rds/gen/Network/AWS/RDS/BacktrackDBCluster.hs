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
-- Module      : Network.AWS.RDS.BacktrackDBCluster
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Backtracks a DB cluster to a specific time, without creating a new DB cluster.
--
--
-- For more information on backtracking, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/AuroraMySQL.Managing.Backtrack.html Backtracking an Aurora DB Cluster> in the /Amazon Aurora User Guide./ 
--
module Network.AWS.RDS.BacktrackDBCluster
    (
    -- * Creating a Request
      backtrackDBCluster
    , BacktrackDBCluster
    -- * Request Lenses
    , bdcForce
    , bdcUseEarliestTimeOnPointInTimeUnavailable
    , bdcDBClusterIdentifier
    , bdcBacktrackTo

    -- * Destructuring the Response
    , dbClusterBacktrack
    , DBClusterBacktrack
    -- * Response Lenses
    , dcbStatus
    , dcbBacktrackIdentifier
    , dcbBacktrackTo
    , dcbDBClusterIdentifier
    , dcbBacktrackedFrom
    , dcbBacktrackRequestCreationTime
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
-- /See:/ 'backtrackDBCluster' smart constructor.
data BacktrackDBCluster = BacktrackDBCluster'{_bdcForce
                                              :: !(Maybe Bool),
                                              _bdcUseEarliestTimeOnPointInTimeUnavailable
                                              :: !(Maybe Bool),
                                              _bdcDBClusterIdentifier :: !Text,
                                              _bdcBacktrackTo :: !ISO8601}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BacktrackDBCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdcForce' - A value that indicates whether to force the DB cluster to backtrack when binary logging is enabled. Otherwise, an error occurs when binary logging is enabled.
--
-- * 'bdcUseEarliestTimeOnPointInTimeUnavailable' - A value that indicates whether to backtrack the DB cluster to the earliest possible backtrack time when /BacktrackTo/ is set to a timestamp earlier than the earliest backtrack time. When this parameter is disabled and /BacktrackTo/ is set to a timestamp earlier than the earliest backtrack time, an error occurs.
--
-- * 'bdcDBClusterIdentifier' - The DB cluster identifier of the DB cluster to be backtracked. This parameter is stored as a lowercase string. Constraints:     * Must contain from 1 to 63 alphanumeric characters or hyphens.     * First character must be a letter.     * Can't end with a hyphen or contain two consecutive hyphens. Example: @my-cluster1@ 
--
-- * 'bdcBacktrackTo' - The timestamp of the time to backtrack the DB cluster to, specified in ISO 8601 format. For more information about ISO 8601, see the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>  Constraints:     * Must contain a valid ISO 8601 timestamp.     * Can't contain a timestamp set in the future. Example: @2017-07-08T18:00Z@ 
backtrackDBCluster
    :: Text -- ^ 'bdcDBClusterIdentifier'
    -> UTCTime -- ^ 'bdcBacktrackTo'
    -> BacktrackDBCluster
backtrackDBCluster pDBClusterIdentifier_
  pBacktrackTo_
  = BacktrackDBCluster'{_bdcForce = Nothing,
                        _bdcUseEarliestTimeOnPointInTimeUnavailable =
                          Nothing,
                        _bdcDBClusterIdentifier = pDBClusterIdentifier_,
                        _bdcBacktrackTo = _Time # pBacktrackTo_}

-- | A value that indicates whether to force the DB cluster to backtrack when binary logging is enabled. Otherwise, an error occurs when binary logging is enabled.
bdcForce :: Lens' BacktrackDBCluster (Maybe Bool)
bdcForce = lens _bdcForce (\ s a -> s{_bdcForce = a})

-- | A value that indicates whether to backtrack the DB cluster to the earliest possible backtrack time when /BacktrackTo/ is set to a timestamp earlier than the earliest backtrack time. When this parameter is disabled and /BacktrackTo/ is set to a timestamp earlier than the earliest backtrack time, an error occurs.
bdcUseEarliestTimeOnPointInTimeUnavailable :: Lens' BacktrackDBCluster (Maybe Bool)
bdcUseEarliestTimeOnPointInTimeUnavailable = lens _bdcUseEarliestTimeOnPointInTimeUnavailable (\ s a -> s{_bdcUseEarliestTimeOnPointInTimeUnavailable = a})

-- | The DB cluster identifier of the DB cluster to be backtracked. This parameter is stored as a lowercase string. Constraints:     * Must contain from 1 to 63 alphanumeric characters or hyphens.     * First character must be a letter.     * Can't end with a hyphen or contain two consecutive hyphens. Example: @my-cluster1@ 
bdcDBClusterIdentifier :: Lens' BacktrackDBCluster Text
bdcDBClusterIdentifier = lens _bdcDBClusterIdentifier (\ s a -> s{_bdcDBClusterIdentifier = a})

-- | The timestamp of the time to backtrack the DB cluster to, specified in ISO 8601 format. For more information about ISO 8601, see the <http://en.wikipedia.org/wiki/ISO_8601 ISO8601 Wikipedia page.>  Constraints:     * Must contain a valid ISO 8601 timestamp.     * Can't contain a timestamp set in the future. Example: @2017-07-08T18:00Z@ 
bdcBacktrackTo :: Lens' BacktrackDBCluster UTCTime
bdcBacktrackTo = lens _bdcBacktrackTo (\ s a -> s{_bdcBacktrackTo = a}) . _Time

instance AWSRequest BacktrackDBCluster where
        type Rs BacktrackDBCluster = DBClusterBacktrack
        request = postQuery rds
        response
          = receiveXMLWrapper "BacktrackDBClusterResult"
              (\ s h x -> parseXML x)

instance Hashable BacktrackDBCluster where

instance NFData BacktrackDBCluster where

instance ToHeaders BacktrackDBCluster where
        toHeaders = const mempty

instance ToPath BacktrackDBCluster where
        toPath = const "/"

instance ToQuery BacktrackDBCluster where
        toQuery BacktrackDBCluster'{..}
          = mconcat
              ["Action" =: ("BacktrackDBCluster" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Force" =: _bdcForce,
               "UseEarliestTimeOnPointInTimeUnavailable" =:
                 _bdcUseEarliestTimeOnPointInTimeUnavailable,
               "DBClusterIdentifier" =: _bdcDBClusterIdentifier,
               "BacktrackTo" =: _bdcBacktrackTo]

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CostAndUsageReport
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CostAndUsageReport where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.CostAndUsageReport
import Test.AWS.CostAndUsageReport.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestPutReportDefinition $
--             putReportDefinition
--
--         , requestDeleteReportDefinition $
--             deleteReportDefinition
--
--         , requestModifyReportDefinition $
--             modifyReportDefinition
--
--         , requestDescribeReportDefinitions $
--             describeReportDefinitions
--
--           ]

--     , testGroup "response"
--         [ responsePutReportDefinition $
--             putReportDefinitionResponse
--
--         , responseDeleteReportDefinition $
--             deleteReportDefinitionResponse
--
--         , responseModifyReportDefinition $
--             modifyReportDefinitionResponse
--
--         , responseDescribeReportDefinitions $
--             describeReportDefinitionsResponse
--
--           ]
--     ]

-- Requests

requestPutReportDefinition :: PutReportDefinition -> TestTree
requestPutReportDefinition = req
    "PutReportDefinition"
    "fixture/PutReportDefinition.yaml"

requestDeleteReportDefinition :: DeleteReportDefinition -> TestTree
requestDeleteReportDefinition = req
    "DeleteReportDefinition"
    "fixture/DeleteReportDefinition.yaml"

requestModifyReportDefinition :: ModifyReportDefinition -> TestTree
requestModifyReportDefinition = req
    "ModifyReportDefinition"
    "fixture/ModifyReportDefinition.yaml"

requestDescribeReportDefinitions :: DescribeReportDefinitions -> TestTree
requestDescribeReportDefinitions = req
    "DescribeReportDefinitions"
    "fixture/DescribeReportDefinitions.yaml"

-- Responses

responsePutReportDefinition :: PutReportDefinitionResponse -> TestTree
responsePutReportDefinition = res
    "PutReportDefinitionResponse"
    "fixture/PutReportDefinitionResponse.proto"
    costAndUsageReport
    (Proxy :: Proxy PutReportDefinition)

responseDeleteReportDefinition :: DeleteReportDefinitionResponse -> TestTree
responseDeleteReportDefinition = res
    "DeleteReportDefinitionResponse"
    "fixture/DeleteReportDefinitionResponse.proto"
    costAndUsageReport
    (Proxy :: Proxy DeleteReportDefinition)

responseModifyReportDefinition :: ModifyReportDefinitionResponse -> TestTree
responseModifyReportDefinition = res
    "ModifyReportDefinitionResponse"
    "fixture/ModifyReportDefinitionResponse.proto"
    costAndUsageReport
    (Proxy :: Proxy ModifyReportDefinition)

responseDescribeReportDefinitions :: DescribeReportDefinitionsResponse -> TestTree
responseDescribeReportDefinitions = res
    "DescribeReportDefinitionsResponse"
    "fixture/DescribeReportDefinitionsResponse.proto"
    costAndUsageReport
    (Proxy :: Proxy DescribeReportDefinitions)

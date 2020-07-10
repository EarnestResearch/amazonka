{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SSM
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.SSM where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.SSM
import Test.AWS.SSM.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetConnectionStatus $
--             getConnectionStatus
--
--         , requestDescribeInstancePatches $
--             describeInstancePatches
--
--         , requestGetInventory $
--             getInventory
--
--         , requestGetParameters $
--             getParameters
--
--         , requestDeletePatchBaseline $
--             deletePatchBaseline
--
--         , requestUpdatePatchBaseline $
--             updatePatchBaseline
--
--         , requestTerminateSession $
--             terminateSession
--
--         , requestGetParameter $
--             getParameter
--
--         , requestUpdateDocumentDefaultVersion $
--             updateDocumentDefaultVersion
--
--         , requestListResourceDataSync $
--             listResourceDataSync
--
--         , requestGetOpsItem $
--             getOpsItem
--
--         , requestResumeSession $
--             resumeSession
--
--         , requestGetDeployablePatchSnapshotForInstance $
--             getDeployablePatchSnapshotForInstance
--
--         , requestDescribeParameters $
--             describeParameters
--
--         , requestDescribeOpsItems $
--             describeOpsItems
--
--         , requestGetParametersByPath $
--             getParametersByPath
--
--         , requestPutComplianceItems $
--             putComplianceItems
--
--         , requestDescribeActivations $
--             describeActivations
--
--         , requestGetMaintenanceWindowTask $
--             getMaintenanceWindowTask
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestDescribeDocument $
--             describeDocument
--
--         , requestDescribePatchProperties $
--             describePatchProperties
--
--         , requestCreateAssociation $
--             createAssociation
--
--         , requestDeleteActivation $
--             deleteActivation
--
--         , requestDescribeMaintenanceWindowExecutions $
--             describeMaintenanceWindowExecutions
--
--         , requestDescribeMaintenanceWindowsForTarget $
--             describeMaintenanceWindowsForTarget
--
--         , requestCancelMaintenanceWindowExecution $
--             cancelMaintenanceWindowExecution
--
--         , requestGetInventorySchema $
--             getInventorySchema
--
--         , requestListComplianceSummaries $
--             listComplianceSummaries
--
--         , requestStartAutomationExecution $
--             startAutomationExecution
--
--         , requestCreateOpsItem $
--             createOpsItem
--
--         , requestCreateActivation $
--             createActivation
--
--         , requestDeleteMaintenanceWindow $
--             deleteMaintenanceWindow
--
--         , requestUpdateMaintenanceWindow $
--             updateMaintenanceWindow
--
--         , requestDescribeSessions $
--             describeSessions
--
--         , requestDescribeMaintenanceWindowExecutionTasks $
--             describeMaintenanceWindowExecutionTasks
--
--         , requestGetDefaultPatchBaseline $
--             getDefaultPatchBaseline
--
--         , requestGetMaintenanceWindowExecutionTask $
--             getMaintenanceWindowExecutionTask
--
--         , requestCreateDocument $
--             createDocument
--
--         , requestRemoveTagsFromResource $
--             removeTagsFromResource
--
--         , requestGetCalendarState $
--             getCalendarState
--
--         , requestDeleteParameters $
--             deleteParameters
--
--         , requestDescribePatchGroupState $
--             describePatchGroupState
--
--         , requestListCommandInvocations $
--             listCommandInvocations
--
--         , requestDeregisterTargetFromMaintenanceWindow $
--             deregisterTargetFromMaintenanceWindow
--
--         , requestDescribeEffectivePatchesForPatchBaseline $
--             describeEffectivePatchesForPatchBaseline
--
--         , requestDescribeMaintenanceWindowTargets $
--             describeMaintenanceWindowTargets
--
--         , requestResetServiceSetting $
--             resetServiceSetting
--
--         , requestRegisterPatchBaselineForPatchGroup $
--             registerPatchBaselineForPatchGroup
--
--         , requestListDocuments $
--             listDocuments
--
--         , requestDescribeInstancePatchStates $
--             describeInstancePatchStates
--
--         , requestGetOpsSummary $
--             getOpsSummary
--
--         , requestGetPatchBaselineForPatchGroup $
--             getPatchBaselineForPatchGroup
--
--         , requestUpdateManagedInstanceRole $
--             updateManagedInstanceRole
--
--         , requestListComplianceItems $
--             listComplianceItems
--
--         , requestGetDocument $
--             getDocument
--
--         , requestDescribeMaintenanceWindowSchedule $
--             describeMaintenanceWindowSchedule
--
--         , requestAddTagsToResource $
--             addTagsToResource
--
--         , requestCancelCommand $
--             cancelCommand
--
--         , requestDescribeAutomationStepExecutions $
--             describeAutomationStepExecutions
--
--         , requestGetCommandInvocation $
--             getCommandInvocation
--
--         , requestDescribeInstancePatchStatesForPatchGroup $
--             describeInstancePatchStatesForPatchGroup
--
--         , requestDeregisterManagedInstance $
--             deregisterManagedInstance
--
--         , requestDescribeAssociation $
--             describeAssociation
--
--         , requestDescribeAssociationExecutionTargets $
--             describeAssociationExecutionTargets
--
--         , requestModifyDocumentPermission $
--             modifyDocumentPermission
--
--         , requestUpdateResourceDataSync $
--             updateResourceDataSync
--
--         , requestDeleteResourceDataSync $
--             deleteResourceDataSync
--
--         , requestUpdateAssociationStatus $
--             updateAssociationStatus
--
--         , requestDescribeAvailablePatches $
--             describeAvailablePatches
--
--         , requestListDocumentVersions $
--             listDocumentVersions
--
--         , requestDeregisterPatchBaselineForPatchGroup $
--             deregisterPatchBaselineForPatchGroup
--
--         , requestDescribePatchGroups $
--             describePatchGroups
--
--         , requestGetMaintenanceWindow $
--             getMaintenanceWindow
--
--         , requestDescribeMaintenanceWindows $
--             describeMaintenanceWindows
--
--         , requestRegisterTaskWithMaintenanceWindow $
--             registerTaskWithMaintenanceWindow
--
--         , requestRegisterDefaultPatchBaseline $
--             registerDefaultPatchBaseline
--
--         , requestListResourceComplianceSummaries $
--             listResourceComplianceSummaries
--
--         , requestListAssociationVersions $
--             listAssociationVersions
--
--         , requestUpdateServiceSetting $
--             updateServiceSetting
--
--         , requestDescribeMaintenanceWindowTasks $
--             describeMaintenanceWindowTasks
--
--         , requestDescribeInstanceAssociationsStatus $
--             describeInstanceAssociationsStatus
--
--         , requestDeregisterTaskFromMaintenanceWindow $
--             deregisterTaskFromMaintenanceWindow
--
--         , requestListInventoryEntries $
--             listInventoryEntries
--
--         , requestLabelParameterVersion $
--             labelParameterVersion
--
--         , requestUpdateMaintenanceWindowTask $
--             updateMaintenanceWindowTask
--
--         , requestGetParameterHistory $
--             getParameterHistory
--
--         , requestDescribeAssociationExecutions $
--             describeAssociationExecutions
--
--         , requestGetServiceSetting $
--             getServiceSetting
--
--         , requestStartAssociationsOnce $
--             startAssociationsOnce
--
--         , requestCreateMaintenanceWindow $
--             createMaintenanceWindow
--
--         , requestStopAutomationExecution $
--             stopAutomationExecution
--
--         , requestGetMaintenanceWindowExecution $
--             getMaintenanceWindowExecution
--
--         , requestSendAutomationSignal $
--             sendAutomationSignal
--
--         , requestPutParameter $
--             putParameter
--
--         , requestDescribeMaintenanceWindowExecutionTaskInvocations $
--             describeMaintenanceWindowExecutionTaskInvocations
--
--         , requestGetMaintenanceWindowExecutionTaskInvocation $
--             getMaintenanceWindowExecutionTaskInvocation
--
--         , requestDeleteParameter $
--             deleteParameter
--
--         , requestDescribeInstanceInformation $
--             describeInstanceInformation
--
--         , requestListAssociations $
--             listAssociations
--
--         , requestUpdateOpsItem $
--             updateOpsItem
--
--         , requestDeleteAssociation $
--             deleteAssociation
--
--         , requestUpdateAssociation $
--             updateAssociation
--
--         , requestDescribeInventoryDeletions $
--             describeInventoryDeletions
--
--         , requestDeleteInventory $
--             deleteInventory
--
--         , requestPutInventory $
--             putInventory
--
--         , requestDescribeEffectiveInstanceAssociations $
--             describeEffectiveInstanceAssociations
--
--         , requestDescribeAutomationExecutions $
--             describeAutomationExecutions
--
--         , requestGetAutomationExecution $
--             getAutomationExecution
--
--         , requestSendCommand $
--             sendCommand
--
--         , requestDescribePatchBaselines $
--             describePatchBaselines
--
--         , requestGetPatchBaseline $
--             getPatchBaseline
--
--         , requestRegisterTargetWithMaintenanceWindow $
--             registerTargetWithMaintenanceWindow
--
--         , requestStartSession $
--             startSession
--
--         , requestListCommands $
--             listCommands
--
--         , requestUpdateDocument $
--             updateDocument
--
--         , requestDeleteDocument $
--             deleteDocument
--
--         , requestDescribeDocumentPermission $
--             describeDocumentPermission
--
--         , requestCreateAssociationBatch $
--             createAssociationBatch
--
--         , requestUpdateMaintenanceWindowTarget $
--             updateMaintenanceWindowTarget
--
--         , requestCreateResourceDataSync $
--             createResourceDataSync
--
--         , requestCreatePatchBaseline $
--             createPatchBaseline
--
--           ]

--     , testGroup "response"
--         [ responseGetConnectionStatus $
--             getConnectionStatusResponse
--
--         , responseDescribeInstancePatches $
--             describeInstancePatchesResponse
--
--         , responseGetInventory $
--             getInventoryResponse
--
--         , responseGetParameters $
--             getParametersResponse
--
--         , responseDeletePatchBaseline $
--             deletePatchBaselineResponse
--
--         , responseUpdatePatchBaseline $
--             updatePatchBaselineResponse
--
--         , responseTerminateSession $
--             terminateSessionResponse
--
--         , responseGetParameter $
--             getParameterResponse
--
--         , responseUpdateDocumentDefaultVersion $
--             updateDocumentDefaultVersionResponse
--
--         , responseListResourceDataSync $
--             listResourceDataSyncResponse
--
--         , responseGetOpsItem $
--             getOpsItemResponse
--
--         , responseResumeSession $
--             resumeSessionResponse
--
--         , responseGetDeployablePatchSnapshotForInstance $
--             getDeployablePatchSnapshotForInstanceResponse
--
--         , responseDescribeParameters $
--             describeParametersResponse
--
--         , responseDescribeOpsItems $
--             describeOpsItemsResponse
--
--         , responseGetParametersByPath $
--             getParametersByPathResponse
--
--         , responsePutComplianceItems $
--             putComplianceItemsResponse
--
--         , responseDescribeActivations $
--             describeActivationsResponse
--
--         , responseGetMaintenanceWindowTask $
--             getMaintenanceWindowTaskResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseDescribeDocument $
--             describeDocumentResponse
--
--         , responseDescribePatchProperties $
--             describePatchPropertiesResponse
--
--         , responseCreateAssociation $
--             createAssociationResponse
--
--         , responseDeleteActivation $
--             deleteActivationResponse
--
--         , responseDescribeMaintenanceWindowExecutions $
--             describeMaintenanceWindowExecutionsResponse
--
--         , responseDescribeMaintenanceWindowsForTarget $
--             describeMaintenanceWindowsForTargetResponse
--
--         , responseCancelMaintenanceWindowExecution $
--             cancelMaintenanceWindowExecutionResponse
--
--         , responseGetInventorySchema $
--             getInventorySchemaResponse
--
--         , responseListComplianceSummaries $
--             listComplianceSummariesResponse
--
--         , responseStartAutomationExecution $
--             startAutomationExecutionResponse
--
--         , responseCreateOpsItem $
--             createOpsItemResponse
--
--         , responseCreateActivation $
--             createActivationResponse
--
--         , responseDeleteMaintenanceWindow $
--             deleteMaintenanceWindowResponse
--
--         , responseUpdateMaintenanceWindow $
--             updateMaintenanceWindowResponse
--
--         , responseDescribeSessions $
--             describeSessionsResponse
--
--         , responseDescribeMaintenanceWindowExecutionTasks $
--             describeMaintenanceWindowExecutionTasksResponse
--
--         , responseGetDefaultPatchBaseline $
--             getDefaultPatchBaselineResponse
--
--         , responseGetMaintenanceWindowExecutionTask $
--             getMaintenanceWindowExecutionTaskResponse
--
--         , responseCreateDocument $
--             createDocumentResponse
--
--         , responseRemoveTagsFromResource $
--             removeTagsFromResourceResponse
--
--         , responseGetCalendarState $
--             getCalendarStateResponse
--
--         , responseDeleteParameters $
--             deleteParametersResponse
--
--         , responseDescribePatchGroupState $
--             describePatchGroupStateResponse
--
--         , responseListCommandInvocations $
--             listCommandInvocationsResponse
--
--         , responseDeregisterTargetFromMaintenanceWindow $
--             deregisterTargetFromMaintenanceWindowResponse
--
--         , responseDescribeEffectivePatchesForPatchBaseline $
--             describeEffectivePatchesForPatchBaselineResponse
--
--         , responseDescribeMaintenanceWindowTargets $
--             describeMaintenanceWindowTargetsResponse
--
--         , responseResetServiceSetting $
--             resetServiceSettingResponse
--
--         , responseRegisterPatchBaselineForPatchGroup $
--             registerPatchBaselineForPatchGroupResponse
--
--         , responseListDocuments $
--             listDocumentsResponse
--
--         , responseDescribeInstancePatchStates $
--             describeInstancePatchStatesResponse
--
--         , responseGetOpsSummary $
--             getOpsSummaryResponse
--
--         , responseGetPatchBaselineForPatchGroup $
--             getPatchBaselineForPatchGroupResponse
--
--         , responseUpdateManagedInstanceRole $
--             updateManagedInstanceRoleResponse
--
--         , responseListComplianceItems $
--             listComplianceItemsResponse
--
--         , responseGetDocument $
--             getDocumentResponse
--
--         , responseDescribeMaintenanceWindowSchedule $
--             describeMaintenanceWindowScheduleResponse
--
--         , responseAddTagsToResource $
--             addTagsToResourceResponse
--
--         , responseCancelCommand $
--             cancelCommandResponse
--
--         , responseDescribeAutomationStepExecutions $
--             describeAutomationStepExecutionsResponse
--
--         , responseGetCommandInvocation $
--             getCommandInvocationResponse
--
--         , responseDescribeInstancePatchStatesForPatchGroup $
--             describeInstancePatchStatesForPatchGroupResponse
--
--         , responseDeregisterManagedInstance $
--             deregisterManagedInstanceResponse
--
--         , responseDescribeAssociation $
--             describeAssociationResponse
--
--         , responseDescribeAssociationExecutionTargets $
--             describeAssociationExecutionTargetsResponse
--
--         , responseModifyDocumentPermission $
--             modifyDocumentPermissionResponse
--
--         , responseUpdateResourceDataSync $
--             updateResourceDataSyncResponse
--
--         , responseDeleteResourceDataSync $
--             deleteResourceDataSyncResponse
--
--         , responseUpdateAssociationStatus $
--             updateAssociationStatusResponse
--
--         , responseDescribeAvailablePatches $
--             describeAvailablePatchesResponse
--
--         , responseListDocumentVersions $
--             listDocumentVersionsResponse
--
--         , responseDeregisterPatchBaselineForPatchGroup $
--             deregisterPatchBaselineForPatchGroupResponse
--
--         , responseDescribePatchGroups $
--             describePatchGroupsResponse
--
--         , responseGetMaintenanceWindow $
--             getMaintenanceWindowResponse
--
--         , responseDescribeMaintenanceWindows $
--             describeMaintenanceWindowsResponse
--
--         , responseRegisterTaskWithMaintenanceWindow $
--             registerTaskWithMaintenanceWindowResponse
--
--         , responseRegisterDefaultPatchBaseline $
--             registerDefaultPatchBaselineResponse
--
--         , responseListResourceComplianceSummaries $
--             listResourceComplianceSummariesResponse
--
--         , responseListAssociationVersions $
--             listAssociationVersionsResponse
--
--         , responseUpdateServiceSetting $
--             updateServiceSettingResponse
--
--         , responseDescribeMaintenanceWindowTasks $
--             describeMaintenanceWindowTasksResponse
--
--         , responseDescribeInstanceAssociationsStatus $
--             describeInstanceAssociationsStatusResponse
--
--         , responseDeregisterTaskFromMaintenanceWindow $
--             deregisterTaskFromMaintenanceWindowResponse
--
--         , responseListInventoryEntries $
--             listInventoryEntriesResponse
--
--         , responseLabelParameterVersion $
--             labelParameterVersionResponse
--
--         , responseUpdateMaintenanceWindowTask $
--             updateMaintenanceWindowTaskResponse
--
--         , responseGetParameterHistory $
--             getParameterHistoryResponse
--
--         , responseDescribeAssociationExecutions $
--             describeAssociationExecutionsResponse
--
--         , responseGetServiceSetting $
--             getServiceSettingResponse
--
--         , responseStartAssociationsOnce $
--             startAssociationsOnceResponse
--
--         , responseCreateMaintenanceWindow $
--             createMaintenanceWindowResponse
--
--         , responseStopAutomationExecution $
--             stopAutomationExecutionResponse
--
--         , responseGetMaintenanceWindowExecution $
--             getMaintenanceWindowExecutionResponse
--
--         , responseSendAutomationSignal $
--             sendAutomationSignalResponse
--
--         , responsePutParameter $
--             putParameterResponse
--
--         , responseDescribeMaintenanceWindowExecutionTaskInvocations $
--             describeMaintenanceWindowExecutionTaskInvocationsResponse
--
--         , responseGetMaintenanceWindowExecutionTaskInvocation $
--             getMaintenanceWindowExecutionTaskInvocationResponse
--
--         , responseDeleteParameter $
--             deleteParameterResponse
--
--         , responseDescribeInstanceInformation $
--             describeInstanceInformationResponse
--
--         , responseListAssociations $
--             listAssociationsResponse
--
--         , responseUpdateOpsItem $
--             updateOpsItemResponse
--
--         , responseDeleteAssociation $
--             deleteAssociationResponse
--
--         , responseUpdateAssociation $
--             updateAssociationResponse
--
--         , responseDescribeInventoryDeletions $
--             describeInventoryDeletionsResponse
--
--         , responseDeleteInventory $
--             deleteInventoryResponse
--
--         , responsePutInventory $
--             putInventoryResponse
--
--         , responseDescribeEffectiveInstanceAssociations $
--             describeEffectiveInstanceAssociationsResponse
--
--         , responseDescribeAutomationExecutions $
--             describeAutomationExecutionsResponse
--
--         , responseGetAutomationExecution $
--             getAutomationExecutionResponse
--
--         , responseSendCommand $
--             sendCommandResponse
--
--         , responseDescribePatchBaselines $
--             describePatchBaselinesResponse
--
--         , responseGetPatchBaseline $
--             getPatchBaselineResponse
--
--         , responseRegisterTargetWithMaintenanceWindow $
--             registerTargetWithMaintenanceWindowResponse
--
--         , responseStartSession $
--             startSessionResponse
--
--         , responseListCommands $
--             listCommandsResponse
--
--         , responseUpdateDocument $
--             updateDocumentResponse
--
--         , responseDeleteDocument $
--             deleteDocumentResponse
--
--         , responseDescribeDocumentPermission $
--             describeDocumentPermissionResponse
--
--         , responseCreateAssociationBatch $
--             createAssociationBatchResponse
--
--         , responseUpdateMaintenanceWindowTarget $
--             updateMaintenanceWindowTargetResponse
--
--         , responseCreateResourceDataSync $
--             createResourceDataSyncResponse
--
--         , responseCreatePatchBaseline $
--             createPatchBaselineResponse
--
--           ]
--     ]

-- Requests

requestGetConnectionStatus :: GetConnectionStatus -> TestTree
requestGetConnectionStatus = req
    "GetConnectionStatus"
    "fixture/GetConnectionStatus.yaml"

requestDescribeInstancePatches :: DescribeInstancePatches -> TestTree
requestDescribeInstancePatches = req
    "DescribeInstancePatches"
    "fixture/DescribeInstancePatches.yaml"

requestGetInventory :: GetInventory -> TestTree
requestGetInventory = req
    "GetInventory"
    "fixture/GetInventory.yaml"

requestGetParameters :: GetParameters -> TestTree
requestGetParameters = req
    "GetParameters"
    "fixture/GetParameters.yaml"

requestDeletePatchBaseline :: DeletePatchBaseline -> TestTree
requestDeletePatchBaseline = req
    "DeletePatchBaseline"
    "fixture/DeletePatchBaseline.yaml"

requestUpdatePatchBaseline :: UpdatePatchBaseline -> TestTree
requestUpdatePatchBaseline = req
    "UpdatePatchBaseline"
    "fixture/UpdatePatchBaseline.yaml"

requestTerminateSession :: TerminateSession -> TestTree
requestTerminateSession = req
    "TerminateSession"
    "fixture/TerminateSession.yaml"

requestGetParameter :: GetParameter -> TestTree
requestGetParameter = req
    "GetParameter"
    "fixture/GetParameter.yaml"

requestUpdateDocumentDefaultVersion :: UpdateDocumentDefaultVersion -> TestTree
requestUpdateDocumentDefaultVersion = req
    "UpdateDocumentDefaultVersion"
    "fixture/UpdateDocumentDefaultVersion.yaml"

requestListResourceDataSync :: ListResourceDataSync -> TestTree
requestListResourceDataSync = req
    "ListResourceDataSync"
    "fixture/ListResourceDataSync.yaml"

requestGetOpsItem :: GetOpsItem -> TestTree
requestGetOpsItem = req
    "GetOpsItem"
    "fixture/GetOpsItem.yaml"

requestResumeSession :: ResumeSession -> TestTree
requestResumeSession = req
    "ResumeSession"
    "fixture/ResumeSession.yaml"

requestGetDeployablePatchSnapshotForInstance :: GetDeployablePatchSnapshotForInstance -> TestTree
requestGetDeployablePatchSnapshotForInstance = req
    "GetDeployablePatchSnapshotForInstance"
    "fixture/GetDeployablePatchSnapshotForInstance.yaml"

requestDescribeParameters :: DescribeParameters -> TestTree
requestDescribeParameters = req
    "DescribeParameters"
    "fixture/DescribeParameters.yaml"

requestDescribeOpsItems :: DescribeOpsItems -> TestTree
requestDescribeOpsItems = req
    "DescribeOpsItems"
    "fixture/DescribeOpsItems.yaml"

requestGetParametersByPath :: GetParametersByPath -> TestTree
requestGetParametersByPath = req
    "GetParametersByPath"
    "fixture/GetParametersByPath.yaml"

requestPutComplianceItems :: PutComplianceItems -> TestTree
requestPutComplianceItems = req
    "PutComplianceItems"
    "fixture/PutComplianceItems.yaml"

requestDescribeActivations :: DescribeActivations -> TestTree
requestDescribeActivations = req
    "DescribeActivations"
    "fixture/DescribeActivations.yaml"

requestGetMaintenanceWindowTask :: GetMaintenanceWindowTask -> TestTree
requestGetMaintenanceWindowTask = req
    "GetMaintenanceWindowTask"
    "fixture/GetMaintenanceWindowTask.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDescribeDocument :: DescribeDocument -> TestTree
requestDescribeDocument = req
    "DescribeDocument"
    "fixture/DescribeDocument.yaml"

requestDescribePatchProperties :: DescribePatchProperties -> TestTree
requestDescribePatchProperties = req
    "DescribePatchProperties"
    "fixture/DescribePatchProperties.yaml"

requestCreateAssociation :: CreateAssociation -> TestTree
requestCreateAssociation = req
    "CreateAssociation"
    "fixture/CreateAssociation.yaml"

requestDeleteActivation :: DeleteActivation -> TestTree
requestDeleteActivation = req
    "DeleteActivation"
    "fixture/DeleteActivation.yaml"

requestDescribeMaintenanceWindowExecutions :: DescribeMaintenanceWindowExecutions -> TestTree
requestDescribeMaintenanceWindowExecutions = req
    "DescribeMaintenanceWindowExecutions"
    "fixture/DescribeMaintenanceWindowExecutions.yaml"

requestDescribeMaintenanceWindowsForTarget :: DescribeMaintenanceWindowsForTarget -> TestTree
requestDescribeMaintenanceWindowsForTarget = req
    "DescribeMaintenanceWindowsForTarget"
    "fixture/DescribeMaintenanceWindowsForTarget.yaml"

requestCancelMaintenanceWindowExecution :: CancelMaintenanceWindowExecution -> TestTree
requestCancelMaintenanceWindowExecution = req
    "CancelMaintenanceWindowExecution"
    "fixture/CancelMaintenanceWindowExecution.yaml"

requestGetInventorySchema :: GetInventorySchema -> TestTree
requestGetInventorySchema = req
    "GetInventorySchema"
    "fixture/GetInventorySchema.yaml"

requestListComplianceSummaries :: ListComplianceSummaries -> TestTree
requestListComplianceSummaries = req
    "ListComplianceSummaries"
    "fixture/ListComplianceSummaries.yaml"

requestStartAutomationExecution :: StartAutomationExecution -> TestTree
requestStartAutomationExecution = req
    "StartAutomationExecution"
    "fixture/StartAutomationExecution.yaml"

requestCreateOpsItem :: CreateOpsItem -> TestTree
requestCreateOpsItem = req
    "CreateOpsItem"
    "fixture/CreateOpsItem.yaml"

requestCreateActivation :: CreateActivation -> TestTree
requestCreateActivation = req
    "CreateActivation"
    "fixture/CreateActivation.yaml"

requestDeleteMaintenanceWindow :: DeleteMaintenanceWindow -> TestTree
requestDeleteMaintenanceWindow = req
    "DeleteMaintenanceWindow"
    "fixture/DeleteMaintenanceWindow.yaml"

requestUpdateMaintenanceWindow :: UpdateMaintenanceWindow -> TestTree
requestUpdateMaintenanceWindow = req
    "UpdateMaintenanceWindow"
    "fixture/UpdateMaintenanceWindow.yaml"

requestDescribeSessions :: DescribeSessions -> TestTree
requestDescribeSessions = req
    "DescribeSessions"
    "fixture/DescribeSessions.yaml"

requestDescribeMaintenanceWindowExecutionTasks :: DescribeMaintenanceWindowExecutionTasks -> TestTree
requestDescribeMaintenanceWindowExecutionTasks = req
    "DescribeMaintenanceWindowExecutionTasks"
    "fixture/DescribeMaintenanceWindowExecutionTasks.yaml"

requestGetDefaultPatchBaseline :: GetDefaultPatchBaseline -> TestTree
requestGetDefaultPatchBaseline = req
    "GetDefaultPatchBaseline"
    "fixture/GetDefaultPatchBaseline.yaml"

requestGetMaintenanceWindowExecutionTask :: GetMaintenanceWindowExecutionTask -> TestTree
requestGetMaintenanceWindowExecutionTask = req
    "GetMaintenanceWindowExecutionTask"
    "fixture/GetMaintenanceWindowExecutionTask.yaml"

requestCreateDocument :: CreateDocument -> TestTree
requestCreateDocument = req
    "CreateDocument"
    "fixture/CreateDocument.yaml"

requestRemoveTagsFromResource :: RemoveTagsFromResource -> TestTree
requestRemoveTagsFromResource = req
    "RemoveTagsFromResource"
    "fixture/RemoveTagsFromResource.yaml"

requestGetCalendarState :: GetCalendarState -> TestTree
requestGetCalendarState = req
    "GetCalendarState"
    "fixture/GetCalendarState.yaml"

requestDeleteParameters :: DeleteParameters -> TestTree
requestDeleteParameters = req
    "DeleteParameters"
    "fixture/DeleteParameters.yaml"

requestDescribePatchGroupState :: DescribePatchGroupState -> TestTree
requestDescribePatchGroupState = req
    "DescribePatchGroupState"
    "fixture/DescribePatchGroupState.yaml"

requestListCommandInvocations :: ListCommandInvocations -> TestTree
requestListCommandInvocations = req
    "ListCommandInvocations"
    "fixture/ListCommandInvocations.yaml"

requestDeregisterTargetFromMaintenanceWindow :: DeregisterTargetFromMaintenanceWindow -> TestTree
requestDeregisterTargetFromMaintenanceWindow = req
    "DeregisterTargetFromMaintenanceWindow"
    "fixture/DeregisterTargetFromMaintenanceWindow.yaml"

requestDescribeEffectivePatchesForPatchBaseline :: DescribeEffectivePatchesForPatchBaseline -> TestTree
requestDescribeEffectivePatchesForPatchBaseline = req
    "DescribeEffectivePatchesForPatchBaseline"
    "fixture/DescribeEffectivePatchesForPatchBaseline.yaml"

requestDescribeMaintenanceWindowTargets :: DescribeMaintenanceWindowTargets -> TestTree
requestDescribeMaintenanceWindowTargets = req
    "DescribeMaintenanceWindowTargets"
    "fixture/DescribeMaintenanceWindowTargets.yaml"

requestResetServiceSetting :: ResetServiceSetting -> TestTree
requestResetServiceSetting = req
    "ResetServiceSetting"
    "fixture/ResetServiceSetting.yaml"

requestRegisterPatchBaselineForPatchGroup :: RegisterPatchBaselineForPatchGroup -> TestTree
requestRegisterPatchBaselineForPatchGroup = req
    "RegisterPatchBaselineForPatchGroup"
    "fixture/RegisterPatchBaselineForPatchGroup.yaml"

requestListDocuments :: ListDocuments -> TestTree
requestListDocuments = req
    "ListDocuments"
    "fixture/ListDocuments.yaml"

requestDescribeInstancePatchStates :: DescribeInstancePatchStates -> TestTree
requestDescribeInstancePatchStates = req
    "DescribeInstancePatchStates"
    "fixture/DescribeInstancePatchStates.yaml"

requestGetOpsSummary :: GetOpsSummary -> TestTree
requestGetOpsSummary = req
    "GetOpsSummary"
    "fixture/GetOpsSummary.yaml"

requestGetPatchBaselineForPatchGroup :: GetPatchBaselineForPatchGroup -> TestTree
requestGetPatchBaselineForPatchGroup = req
    "GetPatchBaselineForPatchGroup"
    "fixture/GetPatchBaselineForPatchGroup.yaml"

requestUpdateManagedInstanceRole :: UpdateManagedInstanceRole -> TestTree
requestUpdateManagedInstanceRole = req
    "UpdateManagedInstanceRole"
    "fixture/UpdateManagedInstanceRole.yaml"

requestListComplianceItems :: ListComplianceItems -> TestTree
requestListComplianceItems = req
    "ListComplianceItems"
    "fixture/ListComplianceItems.yaml"

requestGetDocument :: GetDocument -> TestTree
requestGetDocument = req
    "GetDocument"
    "fixture/GetDocument.yaml"

requestDescribeMaintenanceWindowSchedule :: DescribeMaintenanceWindowSchedule -> TestTree
requestDescribeMaintenanceWindowSchedule = req
    "DescribeMaintenanceWindowSchedule"
    "fixture/DescribeMaintenanceWindowSchedule.yaml"

requestAddTagsToResource :: AddTagsToResource -> TestTree
requestAddTagsToResource = req
    "AddTagsToResource"
    "fixture/AddTagsToResource.yaml"

requestCancelCommand :: CancelCommand -> TestTree
requestCancelCommand = req
    "CancelCommand"
    "fixture/CancelCommand.yaml"

requestDescribeAutomationStepExecutions :: DescribeAutomationStepExecutions -> TestTree
requestDescribeAutomationStepExecutions = req
    "DescribeAutomationStepExecutions"
    "fixture/DescribeAutomationStepExecutions.yaml"

requestGetCommandInvocation :: GetCommandInvocation -> TestTree
requestGetCommandInvocation = req
    "GetCommandInvocation"
    "fixture/GetCommandInvocation.yaml"

requestDescribeInstancePatchStatesForPatchGroup :: DescribeInstancePatchStatesForPatchGroup -> TestTree
requestDescribeInstancePatchStatesForPatchGroup = req
    "DescribeInstancePatchStatesForPatchGroup"
    "fixture/DescribeInstancePatchStatesForPatchGroup.yaml"

requestDeregisterManagedInstance :: DeregisterManagedInstance -> TestTree
requestDeregisterManagedInstance = req
    "DeregisterManagedInstance"
    "fixture/DeregisterManagedInstance.yaml"

requestDescribeAssociation :: DescribeAssociation -> TestTree
requestDescribeAssociation = req
    "DescribeAssociation"
    "fixture/DescribeAssociation.yaml"

requestDescribeAssociationExecutionTargets :: DescribeAssociationExecutionTargets -> TestTree
requestDescribeAssociationExecutionTargets = req
    "DescribeAssociationExecutionTargets"
    "fixture/DescribeAssociationExecutionTargets.yaml"

requestModifyDocumentPermission :: ModifyDocumentPermission -> TestTree
requestModifyDocumentPermission = req
    "ModifyDocumentPermission"
    "fixture/ModifyDocumentPermission.yaml"

requestUpdateResourceDataSync :: UpdateResourceDataSync -> TestTree
requestUpdateResourceDataSync = req
    "UpdateResourceDataSync"
    "fixture/UpdateResourceDataSync.yaml"

requestDeleteResourceDataSync :: DeleteResourceDataSync -> TestTree
requestDeleteResourceDataSync = req
    "DeleteResourceDataSync"
    "fixture/DeleteResourceDataSync.yaml"

requestUpdateAssociationStatus :: UpdateAssociationStatus -> TestTree
requestUpdateAssociationStatus = req
    "UpdateAssociationStatus"
    "fixture/UpdateAssociationStatus.yaml"

requestDescribeAvailablePatches :: DescribeAvailablePatches -> TestTree
requestDescribeAvailablePatches = req
    "DescribeAvailablePatches"
    "fixture/DescribeAvailablePatches.yaml"

requestListDocumentVersions :: ListDocumentVersions -> TestTree
requestListDocumentVersions = req
    "ListDocumentVersions"
    "fixture/ListDocumentVersions.yaml"

requestDeregisterPatchBaselineForPatchGroup :: DeregisterPatchBaselineForPatchGroup -> TestTree
requestDeregisterPatchBaselineForPatchGroup = req
    "DeregisterPatchBaselineForPatchGroup"
    "fixture/DeregisterPatchBaselineForPatchGroup.yaml"

requestDescribePatchGroups :: DescribePatchGroups -> TestTree
requestDescribePatchGroups = req
    "DescribePatchGroups"
    "fixture/DescribePatchGroups.yaml"

requestGetMaintenanceWindow :: GetMaintenanceWindow -> TestTree
requestGetMaintenanceWindow = req
    "GetMaintenanceWindow"
    "fixture/GetMaintenanceWindow.yaml"

requestDescribeMaintenanceWindows :: DescribeMaintenanceWindows -> TestTree
requestDescribeMaintenanceWindows = req
    "DescribeMaintenanceWindows"
    "fixture/DescribeMaintenanceWindows.yaml"

requestRegisterTaskWithMaintenanceWindow :: RegisterTaskWithMaintenanceWindow -> TestTree
requestRegisterTaskWithMaintenanceWindow = req
    "RegisterTaskWithMaintenanceWindow"
    "fixture/RegisterTaskWithMaintenanceWindow.yaml"

requestRegisterDefaultPatchBaseline :: RegisterDefaultPatchBaseline -> TestTree
requestRegisterDefaultPatchBaseline = req
    "RegisterDefaultPatchBaseline"
    "fixture/RegisterDefaultPatchBaseline.yaml"

requestListResourceComplianceSummaries :: ListResourceComplianceSummaries -> TestTree
requestListResourceComplianceSummaries = req
    "ListResourceComplianceSummaries"
    "fixture/ListResourceComplianceSummaries.yaml"

requestListAssociationVersions :: ListAssociationVersions -> TestTree
requestListAssociationVersions = req
    "ListAssociationVersions"
    "fixture/ListAssociationVersions.yaml"

requestUpdateServiceSetting :: UpdateServiceSetting -> TestTree
requestUpdateServiceSetting = req
    "UpdateServiceSetting"
    "fixture/UpdateServiceSetting.yaml"

requestDescribeMaintenanceWindowTasks :: DescribeMaintenanceWindowTasks -> TestTree
requestDescribeMaintenanceWindowTasks = req
    "DescribeMaintenanceWindowTasks"
    "fixture/DescribeMaintenanceWindowTasks.yaml"

requestDescribeInstanceAssociationsStatus :: DescribeInstanceAssociationsStatus -> TestTree
requestDescribeInstanceAssociationsStatus = req
    "DescribeInstanceAssociationsStatus"
    "fixture/DescribeInstanceAssociationsStatus.yaml"

requestDeregisterTaskFromMaintenanceWindow :: DeregisterTaskFromMaintenanceWindow -> TestTree
requestDeregisterTaskFromMaintenanceWindow = req
    "DeregisterTaskFromMaintenanceWindow"
    "fixture/DeregisterTaskFromMaintenanceWindow.yaml"

requestListInventoryEntries :: ListInventoryEntries -> TestTree
requestListInventoryEntries = req
    "ListInventoryEntries"
    "fixture/ListInventoryEntries.yaml"

requestLabelParameterVersion :: LabelParameterVersion -> TestTree
requestLabelParameterVersion = req
    "LabelParameterVersion"
    "fixture/LabelParameterVersion.yaml"

requestUpdateMaintenanceWindowTask :: UpdateMaintenanceWindowTask -> TestTree
requestUpdateMaintenanceWindowTask = req
    "UpdateMaintenanceWindowTask"
    "fixture/UpdateMaintenanceWindowTask.yaml"

requestGetParameterHistory :: GetParameterHistory -> TestTree
requestGetParameterHistory = req
    "GetParameterHistory"
    "fixture/GetParameterHistory.yaml"

requestDescribeAssociationExecutions :: DescribeAssociationExecutions -> TestTree
requestDescribeAssociationExecutions = req
    "DescribeAssociationExecutions"
    "fixture/DescribeAssociationExecutions.yaml"

requestGetServiceSetting :: GetServiceSetting -> TestTree
requestGetServiceSetting = req
    "GetServiceSetting"
    "fixture/GetServiceSetting.yaml"

requestStartAssociationsOnce :: StartAssociationsOnce -> TestTree
requestStartAssociationsOnce = req
    "StartAssociationsOnce"
    "fixture/StartAssociationsOnce.yaml"

requestCreateMaintenanceWindow :: CreateMaintenanceWindow -> TestTree
requestCreateMaintenanceWindow = req
    "CreateMaintenanceWindow"
    "fixture/CreateMaintenanceWindow.yaml"

requestStopAutomationExecution :: StopAutomationExecution -> TestTree
requestStopAutomationExecution = req
    "StopAutomationExecution"
    "fixture/StopAutomationExecution.yaml"

requestGetMaintenanceWindowExecution :: GetMaintenanceWindowExecution -> TestTree
requestGetMaintenanceWindowExecution = req
    "GetMaintenanceWindowExecution"
    "fixture/GetMaintenanceWindowExecution.yaml"

requestSendAutomationSignal :: SendAutomationSignal -> TestTree
requestSendAutomationSignal = req
    "SendAutomationSignal"
    "fixture/SendAutomationSignal.yaml"

requestPutParameter :: PutParameter -> TestTree
requestPutParameter = req
    "PutParameter"
    "fixture/PutParameter.yaml"

requestDescribeMaintenanceWindowExecutionTaskInvocations :: DescribeMaintenanceWindowExecutionTaskInvocations -> TestTree
requestDescribeMaintenanceWindowExecutionTaskInvocations = req
    "DescribeMaintenanceWindowExecutionTaskInvocations"
    "fixture/DescribeMaintenanceWindowExecutionTaskInvocations.yaml"

requestGetMaintenanceWindowExecutionTaskInvocation :: GetMaintenanceWindowExecutionTaskInvocation -> TestTree
requestGetMaintenanceWindowExecutionTaskInvocation = req
    "GetMaintenanceWindowExecutionTaskInvocation"
    "fixture/GetMaintenanceWindowExecutionTaskInvocation.yaml"

requestDeleteParameter :: DeleteParameter -> TestTree
requestDeleteParameter = req
    "DeleteParameter"
    "fixture/DeleteParameter.yaml"

requestDescribeInstanceInformation :: DescribeInstanceInformation -> TestTree
requestDescribeInstanceInformation = req
    "DescribeInstanceInformation"
    "fixture/DescribeInstanceInformation.yaml"

requestListAssociations :: ListAssociations -> TestTree
requestListAssociations = req
    "ListAssociations"
    "fixture/ListAssociations.yaml"

requestUpdateOpsItem :: UpdateOpsItem -> TestTree
requestUpdateOpsItem = req
    "UpdateOpsItem"
    "fixture/UpdateOpsItem.yaml"

requestDeleteAssociation :: DeleteAssociation -> TestTree
requestDeleteAssociation = req
    "DeleteAssociation"
    "fixture/DeleteAssociation.yaml"

requestUpdateAssociation :: UpdateAssociation -> TestTree
requestUpdateAssociation = req
    "UpdateAssociation"
    "fixture/UpdateAssociation.yaml"

requestDescribeInventoryDeletions :: DescribeInventoryDeletions -> TestTree
requestDescribeInventoryDeletions = req
    "DescribeInventoryDeletions"
    "fixture/DescribeInventoryDeletions.yaml"

requestDeleteInventory :: DeleteInventory -> TestTree
requestDeleteInventory = req
    "DeleteInventory"
    "fixture/DeleteInventory.yaml"

requestPutInventory :: PutInventory -> TestTree
requestPutInventory = req
    "PutInventory"
    "fixture/PutInventory.yaml"

requestDescribeEffectiveInstanceAssociations :: DescribeEffectiveInstanceAssociations -> TestTree
requestDescribeEffectiveInstanceAssociations = req
    "DescribeEffectiveInstanceAssociations"
    "fixture/DescribeEffectiveInstanceAssociations.yaml"

requestDescribeAutomationExecutions :: DescribeAutomationExecutions -> TestTree
requestDescribeAutomationExecutions = req
    "DescribeAutomationExecutions"
    "fixture/DescribeAutomationExecutions.yaml"

requestGetAutomationExecution :: GetAutomationExecution -> TestTree
requestGetAutomationExecution = req
    "GetAutomationExecution"
    "fixture/GetAutomationExecution.yaml"

requestSendCommand :: SendCommand -> TestTree
requestSendCommand = req
    "SendCommand"
    "fixture/SendCommand.yaml"

requestDescribePatchBaselines :: DescribePatchBaselines -> TestTree
requestDescribePatchBaselines = req
    "DescribePatchBaselines"
    "fixture/DescribePatchBaselines.yaml"

requestGetPatchBaseline :: GetPatchBaseline -> TestTree
requestGetPatchBaseline = req
    "GetPatchBaseline"
    "fixture/GetPatchBaseline.yaml"

requestRegisterTargetWithMaintenanceWindow :: RegisterTargetWithMaintenanceWindow -> TestTree
requestRegisterTargetWithMaintenanceWindow = req
    "RegisterTargetWithMaintenanceWindow"
    "fixture/RegisterTargetWithMaintenanceWindow.yaml"

requestStartSession :: StartSession -> TestTree
requestStartSession = req
    "StartSession"
    "fixture/StartSession.yaml"

requestListCommands :: ListCommands -> TestTree
requestListCommands = req
    "ListCommands"
    "fixture/ListCommands.yaml"

requestUpdateDocument :: UpdateDocument -> TestTree
requestUpdateDocument = req
    "UpdateDocument"
    "fixture/UpdateDocument.yaml"

requestDeleteDocument :: DeleteDocument -> TestTree
requestDeleteDocument = req
    "DeleteDocument"
    "fixture/DeleteDocument.yaml"

requestDescribeDocumentPermission :: DescribeDocumentPermission -> TestTree
requestDescribeDocumentPermission = req
    "DescribeDocumentPermission"
    "fixture/DescribeDocumentPermission.yaml"

requestCreateAssociationBatch :: CreateAssociationBatch -> TestTree
requestCreateAssociationBatch = req
    "CreateAssociationBatch"
    "fixture/CreateAssociationBatch.yaml"

requestUpdateMaintenanceWindowTarget :: UpdateMaintenanceWindowTarget -> TestTree
requestUpdateMaintenanceWindowTarget = req
    "UpdateMaintenanceWindowTarget"
    "fixture/UpdateMaintenanceWindowTarget.yaml"

requestCreateResourceDataSync :: CreateResourceDataSync -> TestTree
requestCreateResourceDataSync = req
    "CreateResourceDataSync"
    "fixture/CreateResourceDataSync.yaml"

requestCreatePatchBaseline :: CreatePatchBaseline -> TestTree
requestCreatePatchBaseline = req
    "CreatePatchBaseline"
    "fixture/CreatePatchBaseline.yaml"

-- Responses

responseGetConnectionStatus :: GetConnectionStatusResponse -> TestTree
responseGetConnectionStatus = res
    "GetConnectionStatusResponse"
    "fixture/GetConnectionStatusResponse.proto"
    ssm
    (Proxy :: Proxy GetConnectionStatus)

responseDescribeInstancePatches :: DescribeInstancePatchesResponse -> TestTree
responseDescribeInstancePatches = res
    "DescribeInstancePatchesResponse"
    "fixture/DescribeInstancePatchesResponse.proto"
    ssm
    (Proxy :: Proxy DescribeInstancePatches)

responseGetInventory :: GetInventoryResponse -> TestTree
responseGetInventory = res
    "GetInventoryResponse"
    "fixture/GetInventoryResponse.proto"
    ssm
    (Proxy :: Proxy GetInventory)

responseGetParameters :: GetParametersResponse -> TestTree
responseGetParameters = res
    "GetParametersResponse"
    "fixture/GetParametersResponse.proto"
    ssm
    (Proxy :: Proxy GetParameters)

responseDeletePatchBaseline :: DeletePatchBaselineResponse -> TestTree
responseDeletePatchBaseline = res
    "DeletePatchBaselineResponse"
    "fixture/DeletePatchBaselineResponse.proto"
    ssm
    (Proxy :: Proxy DeletePatchBaseline)

responseUpdatePatchBaseline :: UpdatePatchBaselineResponse -> TestTree
responseUpdatePatchBaseline = res
    "UpdatePatchBaselineResponse"
    "fixture/UpdatePatchBaselineResponse.proto"
    ssm
    (Proxy :: Proxy UpdatePatchBaseline)

responseTerminateSession :: TerminateSessionResponse -> TestTree
responseTerminateSession = res
    "TerminateSessionResponse"
    "fixture/TerminateSessionResponse.proto"
    ssm
    (Proxy :: Proxy TerminateSession)

responseGetParameter :: GetParameterResponse -> TestTree
responseGetParameter = res
    "GetParameterResponse"
    "fixture/GetParameterResponse.proto"
    ssm
    (Proxy :: Proxy GetParameter)

responseUpdateDocumentDefaultVersion :: UpdateDocumentDefaultVersionResponse -> TestTree
responseUpdateDocumentDefaultVersion = res
    "UpdateDocumentDefaultVersionResponse"
    "fixture/UpdateDocumentDefaultVersionResponse.proto"
    ssm
    (Proxy :: Proxy UpdateDocumentDefaultVersion)

responseListResourceDataSync :: ListResourceDataSyncResponse -> TestTree
responseListResourceDataSync = res
    "ListResourceDataSyncResponse"
    "fixture/ListResourceDataSyncResponse.proto"
    ssm
    (Proxy :: Proxy ListResourceDataSync)

responseGetOpsItem :: GetOpsItemResponse -> TestTree
responseGetOpsItem = res
    "GetOpsItemResponse"
    "fixture/GetOpsItemResponse.proto"
    ssm
    (Proxy :: Proxy GetOpsItem)

responseResumeSession :: ResumeSessionResponse -> TestTree
responseResumeSession = res
    "ResumeSessionResponse"
    "fixture/ResumeSessionResponse.proto"
    ssm
    (Proxy :: Proxy ResumeSession)

responseGetDeployablePatchSnapshotForInstance :: GetDeployablePatchSnapshotForInstanceResponse -> TestTree
responseGetDeployablePatchSnapshotForInstance = res
    "GetDeployablePatchSnapshotForInstanceResponse"
    "fixture/GetDeployablePatchSnapshotForInstanceResponse.proto"
    ssm
    (Proxy :: Proxy GetDeployablePatchSnapshotForInstance)

responseDescribeParameters :: DescribeParametersResponse -> TestTree
responseDescribeParameters = res
    "DescribeParametersResponse"
    "fixture/DescribeParametersResponse.proto"
    ssm
    (Proxy :: Proxy DescribeParameters)

responseDescribeOpsItems :: DescribeOpsItemsResponse -> TestTree
responseDescribeOpsItems = res
    "DescribeOpsItemsResponse"
    "fixture/DescribeOpsItemsResponse.proto"
    ssm
    (Proxy :: Proxy DescribeOpsItems)

responseGetParametersByPath :: GetParametersByPathResponse -> TestTree
responseGetParametersByPath = res
    "GetParametersByPathResponse"
    "fixture/GetParametersByPathResponse.proto"
    ssm
    (Proxy :: Proxy GetParametersByPath)

responsePutComplianceItems :: PutComplianceItemsResponse -> TestTree
responsePutComplianceItems = res
    "PutComplianceItemsResponse"
    "fixture/PutComplianceItemsResponse.proto"
    ssm
    (Proxy :: Proxy PutComplianceItems)

responseDescribeActivations :: DescribeActivationsResponse -> TestTree
responseDescribeActivations = res
    "DescribeActivationsResponse"
    "fixture/DescribeActivationsResponse.proto"
    ssm
    (Proxy :: Proxy DescribeActivations)

responseGetMaintenanceWindowTask :: GetMaintenanceWindowTaskResponse -> TestTree
responseGetMaintenanceWindowTask = res
    "GetMaintenanceWindowTaskResponse"
    "fixture/GetMaintenanceWindowTaskResponse.proto"
    ssm
    (Proxy :: Proxy GetMaintenanceWindowTask)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    ssm
    (Proxy :: Proxy ListTagsForResource)

responseDescribeDocument :: DescribeDocumentResponse -> TestTree
responseDescribeDocument = res
    "DescribeDocumentResponse"
    "fixture/DescribeDocumentResponse.proto"
    ssm
    (Proxy :: Proxy DescribeDocument)

responseDescribePatchProperties :: DescribePatchPropertiesResponse -> TestTree
responseDescribePatchProperties = res
    "DescribePatchPropertiesResponse"
    "fixture/DescribePatchPropertiesResponse.proto"
    ssm
    (Proxy :: Proxy DescribePatchProperties)

responseCreateAssociation :: CreateAssociationResponse -> TestTree
responseCreateAssociation = res
    "CreateAssociationResponse"
    "fixture/CreateAssociationResponse.proto"
    ssm
    (Proxy :: Proxy CreateAssociation)

responseDeleteActivation :: DeleteActivationResponse -> TestTree
responseDeleteActivation = res
    "DeleteActivationResponse"
    "fixture/DeleteActivationResponse.proto"
    ssm
    (Proxy :: Proxy DeleteActivation)

responseDescribeMaintenanceWindowExecutions :: DescribeMaintenanceWindowExecutionsResponse -> TestTree
responseDescribeMaintenanceWindowExecutions = res
    "DescribeMaintenanceWindowExecutionsResponse"
    "fixture/DescribeMaintenanceWindowExecutionsResponse.proto"
    ssm
    (Proxy :: Proxy DescribeMaintenanceWindowExecutions)

responseDescribeMaintenanceWindowsForTarget :: DescribeMaintenanceWindowsForTargetResponse -> TestTree
responseDescribeMaintenanceWindowsForTarget = res
    "DescribeMaintenanceWindowsForTargetResponse"
    "fixture/DescribeMaintenanceWindowsForTargetResponse.proto"
    ssm
    (Proxy :: Proxy DescribeMaintenanceWindowsForTarget)

responseCancelMaintenanceWindowExecution :: CancelMaintenanceWindowExecutionResponse -> TestTree
responseCancelMaintenanceWindowExecution = res
    "CancelMaintenanceWindowExecutionResponse"
    "fixture/CancelMaintenanceWindowExecutionResponse.proto"
    ssm
    (Proxy :: Proxy CancelMaintenanceWindowExecution)

responseGetInventorySchema :: GetInventorySchemaResponse -> TestTree
responseGetInventorySchema = res
    "GetInventorySchemaResponse"
    "fixture/GetInventorySchemaResponse.proto"
    ssm
    (Proxy :: Proxy GetInventorySchema)

responseListComplianceSummaries :: ListComplianceSummariesResponse -> TestTree
responseListComplianceSummaries = res
    "ListComplianceSummariesResponse"
    "fixture/ListComplianceSummariesResponse.proto"
    ssm
    (Proxy :: Proxy ListComplianceSummaries)

responseStartAutomationExecution :: StartAutomationExecutionResponse -> TestTree
responseStartAutomationExecution = res
    "StartAutomationExecutionResponse"
    "fixture/StartAutomationExecutionResponse.proto"
    ssm
    (Proxy :: Proxy StartAutomationExecution)

responseCreateOpsItem :: CreateOpsItemResponse -> TestTree
responseCreateOpsItem = res
    "CreateOpsItemResponse"
    "fixture/CreateOpsItemResponse.proto"
    ssm
    (Proxy :: Proxy CreateOpsItem)

responseCreateActivation :: CreateActivationResponse -> TestTree
responseCreateActivation = res
    "CreateActivationResponse"
    "fixture/CreateActivationResponse.proto"
    ssm
    (Proxy :: Proxy CreateActivation)

responseDeleteMaintenanceWindow :: DeleteMaintenanceWindowResponse -> TestTree
responseDeleteMaintenanceWindow = res
    "DeleteMaintenanceWindowResponse"
    "fixture/DeleteMaintenanceWindowResponse.proto"
    ssm
    (Proxy :: Proxy DeleteMaintenanceWindow)

responseUpdateMaintenanceWindow :: UpdateMaintenanceWindowResponse -> TestTree
responseUpdateMaintenanceWindow = res
    "UpdateMaintenanceWindowResponse"
    "fixture/UpdateMaintenanceWindowResponse.proto"
    ssm
    (Proxy :: Proxy UpdateMaintenanceWindow)

responseDescribeSessions :: DescribeSessionsResponse -> TestTree
responseDescribeSessions = res
    "DescribeSessionsResponse"
    "fixture/DescribeSessionsResponse.proto"
    ssm
    (Proxy :: Proxy DescribeSessions)

responseDescribeMaintenanceWindowExecutionTasks :: DescribeMaintenanceWindowExecutionTasksResponse -> TestTree
responseDescribeMaintenanceWindowExecutionTasks = res
    "DescribeMaintenanceWindowExecutionTasksResponse"
    "fixture/DescribeMaintenanceWindowExecutionTasksResponse.proto"
    ssm
    (Proxy :: Proxy DescribeMaintenanceWindowExecutionTasks)

responseGetDefaultPatchBaseline :: GetDefaultPatchBaselineResponse -> TestTree
responseGetDefaultPatchBaseline = res
    "GetDefaultPatchBaselineResponse"
    "fixture/GetDefaultPatchBaselineResponse.proto"
    ssm
    (Proxy :: Proxy GetDefaultPatchBaseline)

responseGetMaintenanceWindowExecutionTask :: GetMaintenanceWindowExecutionTaskResponse -> TestTree
responseGetMaintenanceWindowExecutionTask = res
    "GetMaintenanceWindowExecutionTaskResponse"
    "fixture/GetMaintenanceWindowExecutionTaskResponse.proto"
    ssm
    (Proxy :: Proxy GetMaintenanceWindowExecutionTask)

responseCreateDocument :: CreateDocumentResponse -> TestTree
responseCreateDocument = res
    "CreateDocumentResponse"
    "fixture/CreateDocumentResponse.proto"
    ssm
    (Proxy :: Proxy CreateDocument)

responseRemoveTagsFromResource :: RemoveTagsFromResourceResponse -> TestTree
responseRemoveTagsFromResource = res
    "RemoveTagsFromResourceResponse"
    "fixture/RemoveTagsFromResourceResponse.proto"
    ssm
    (Proxy :: Proxy RemoveTagsFromResource)

responseGetCalendarState :: GetCalendarStateResponse -> TestTree
responseGetCalendarState = res
    "GetCalendarStateResponse"
    "fixture/GetCalendarStateResponse.proto"
    ssm
    (Proxy :: Proxy GetCalendarState)

responseDeleteParameters :: DeleteParametersResponse -> TestTree
responseDeleteParameters = res
    "DeleteParametersResponse"
    "fixture/DeleteParametersResponse.proto"
    ssm
    (Proxy :: Proxy DeleteParameters)

responseDescribePatchGroupState :: DescribePatchGroupStateResponse -> TestTree
responseDescribePatchGroupState = res
    "DescribePatchGroupStateResponse"
    "fixture/DescribePatchGroupStateResponse.proto"
    ssm
    (Proxy :: Proxy DescribePatchGroupState)

responseListCommandInvocations :: ListCommandInvocationsResponse -> TestTree
responseListCommandInvocations = res
    "ListCommandInvocationsResponse"
    "fixture/ListCommandInvocationsResponse.proto"
    ssm
    (Proxy :: Proxy ListCommandInvocations)

responseDeregisterTargetFromMaintenanceWindow :: DeregisterTargetFromMaintenanceWindowResponse -> TestTree
responseDeregisterTargetFromMaintenanceWindow = res
    "DeregisterTargetFromMaintenanceWindowResponse"
    "fixture/DeregisterTargetFromMaintenanceWindowResponse.proto"
    ssm
    (Proxy :: Proxy DeregisterTargetFromMaintenanceWindow)

responseDescribeEffectivePatchesForPatchBaseline :: DescribeEffectivePatchesForPatchBaselineResponse -> TestTree
responseDescribeEffectivePatchesForPatchBaseline = res
    "DescribeEffectivePatchesForPatchBaselineResponse"
    "fixture/DescribeEffectivePatchesForPatchBaselineResponse.proto"
    ssm
    (Proxy :: Proxy DescribeEffectivePatchesForPatchBaseline)

responseDescribeMaintenanceWindowTargets :: DescribeMaintenanceWindowTargetsResponse -> TestTree
responseDescribeMaintenanceWindowTargets = res
    "DescribeMaintenanceWindowTargetsResponse"
    "fixture/DescribeMaintenanceWindowTargetsResponse.proto"
    ssm
    (Proxy :: Proxy DescribeMaintenanceWindowTargets)

responseResetServiceSetting :: ResetServiceSettingResponse -> TestTree
responseResetServiceSetting = res
    "ResetServiceSettingResponse"
    "fixture/ResetServiceSettingResponse.proto"
    ssm
    (Proxy :: Proxy ResetServiceSetting)

responseRegisterPatchBaselineForPatchGroup :: RegisterPatchBaselineForPatchGroupResponse -> TestTree
responseRegisterPatchBaselineForPatchGroup = res
    "RegisterPatchBaselineForPatchGroupResponse"
    "fixture/RegisterPatchBaselineForPatchGroupResponse.proto"
    ssm
    (Proxy :: Proxy RegisterPatchBaselineForPatchGroup)

responseListDocuments :: ListDocumentsResponse -> TestTree
responseListDocuments = res
    "ListDocumentsResponse"
    "fixture/ListDocumentsResponse.proto"
    ssm
    (Proxy :: Proxy ListDocuments)

responseDescribeInstancePatchStates :: DescribeInstancePatchStatesResponse -> TestTree
responseDescribeInstancePatchStates = res
    "DescribeInstancePatchStatesResponse"
    "fixture/DescribeInstancePatchStatesResponse.proto"
    ssm
    (Proxy :: Proxy DescribeInstancePatchStates)

responseGetOpsSummary :: GetOpsSummaryResponse -> TestTree
responseGetOpsSummary = res
    "GetOpsSummaryResponse"
    "fixture/GetOpsSummaryResponse.proto"
    ssm
    (Proxy :: Proxy GetOpsSummary)

responseGetPatchBaselineForPatchGroup :: GetPatchBaselineForPatchGroupResponse -> TestTree
responseGetPatchBaselineForPatchGroup = res
    "GetPatchBaselineForPatchGroupResponse"
    "fixture/GetPatchBaselineForPatchGroupResponse.proto"
    ssm
    (Proxy :: Proxy GetPatchBaselineForPatchGroup)

responseUpdateManagedInstanceRole :: UpdateManagedInstanceRoleResponse -> TestTree
responseUpdateManagedInstanceRole = res
    "UpdateManagedInstanceRoleResponse"
    "fixture/UpdateManagedInstanceRoleResponse.proto"
    ssm
    (Proxy :: Proxy UpdateManagedInstanceRole)

responseListComplianceItems :: ListComplianceItemsResponse -> TestTree
responseListComplianceItems = res
    "ListComplianceItemsResponse"
    "fixture/ListComplianceItemsResponse.proto"
    ssm
    (Proxy :: Proxy ListComplianceItems)

responseGetDocument :: GetDocumentResponse -> TestTree
responseGetDocument = res
    "GetDocumentResponse"
    "fixture/GetDocumentResponse.proto"
    ssm
    (Proxy :: Proxy GetDocument)

responseDescribeMaintenanceWindowSchedule :: DescribeMaintenanceWindowScheduleResponse -> TestTree
responseDescribeMaintenanceWindowSchedule = res
    "DescribeMaintenanceWindowScheduleResponse"
    "fixture/DescribeMaintenanceWindowScheduleResponse.proto"
    ssm
    (Proxy :: Proxy DescribeMaintenanceWindowSchedule)

responseAddTagsToResource :: AddTagsToResourceResponse -> TestTree
responseAddTagsToResource = res
    "AddTagsToResourceResponse"
    "fixture/AddTagsToResourceResponse.proto"
    ssm
    (Proxy :: Proxy AddTagsToResource)

responseCancelCommand :: CancelCommandResponse -> TestTree
responseCancelCommand = res
    "CancelCommandResponse"
    "fixture/CancelCommandResponse.proto"
    ssm
    (Proxy :: Proxy CancelCommand)

responseDescribeAutomationStepExecutions :: DescribeAutomationStepExecutionsResponse -> TestTree
responseDescribeAutomationStepExecutions = res
    "DescribeAutomationStepExecutionsResponse"
    "fixture/DescribeAutomationStepExecutionsResponse.proto"
    ssm
    (Proxy :: Proxy DescribeAutomationStepExecutions)

responseGetCommandInvocation :: GetCommandInvocationResponse -> TestTree
responseGetCommandInvocation = res
    "GetCommandInvocationResponse"
    "fixture/GetCommandInvocationResponse.proto"
    ssm
    (Proxy :: Proxy GetCommandInvocation)

responseDescribeInstancePatchStatesForPatchGroup :: DescribeInstancePatchStatesForPatchGroupResponse -> TestTree
responseDescribeInstancePatchStatesForPatchGroup = res
    "DescribeInstancePatchStatesForPatchGroupResponse"
    "fixture/DescribeInstancePatchStatesForPatchGroupResponse.proto"
    ssm
    (Proxy :: Proxy DescribeInstancePatchStatesForPatchGroup)

responseDeregisterManagedInstance :: DeregisterManagedInstanceResponse -> TestTree
responseDeregisterManagedInstance = res
    "DeregisterManagedInstanceResponse"
    "fixture/DeregisterManagedInstanceResponse.proto"
    ssm
    (Proxy :: Proxy DeregisterManagedInstance)

responseDescribeAssociation :: DescribeAssociationResponse -> TestTree
responseDescribeAssociation = res
    "DescribeAssociationResponse"
    "fixture/DescribeAssociationResponse.proto"
    ssm
    (Proxy :: Proxy DescribeAssociation)

responseDescribeAssociationExecutionTargets :: DescribeAssociationExecutionTargetsResponse -> TestTree
responseDescribeAssociationExecutionTargets = res
    "DescribeAssociationExecutionTargetsResponse"
    "fixture/DescribeAssociationExecutionTargetsResponse.proto"
    ssm
    (Proxy :: Proxy DescribeAssociationExecutionTargets)

responseModifyDocumentPermission :: ModifyDocumentPermissionResponse -> TestTree
responseModifyDocumentPermission = res
    "ModifyDocumentPermissionResponse"
    "fixture/ModifyDocumentPermissionResponse.proto"
    ssm
    (Proxy :: Proxy ModifyDocumentPermission)

responseUpdateResourceDataSync :: UpdateResourceDataSyncResponse -> TestTree
responseUpdateResourceDataSync = res
    "UpdateResourceDataSyncResponse"
    "fixture/UpdateResourceDataSyncResponse.proto"
    ssm
    (Proxy :: Proxy UpdateResourceDataSync)

responseDeleteResourceDataSync :: DeleteResourceDataSyncResponse -> TestTree
responseDeleteResourceDataSync = res
    "DeleteResourceDataSyncResponse"
    "fixture/DeleteResourceDataSyncResponse.proto"
    ssm
    (Proxy :: Proxy DeleteResourceDataSync)

responseUpdateAssociationStatus :: UpdateAssociationStatusResponse -> TestTree
responseUpdateAssociationStatus = res
    "UpdateAssociationStatusResponse"
    "fixture/UpdateAssociationStatusResponse.proto"
    ssm
    (Proxy :: Proxy UpdateAssociationStatus)

responseDescribeAvailablePatches :: DescribeAvailablePatchesResponse -> TestTree
responseDescribeAvailablePatches = res
    "DescribeAvailablePatchesResponse"
    "fixture/DescribeAvailablePatchesResponse.proto"
    ssm
    (Proxy :: Proxy DescribeAvailablePatches)

responseListDocumentVersions :: ListDocumentVersionsResponse -> TestTree
responseListDocumentVersions = res
    "ListDocumentVersionsResponse"
    "fixture/ListDocumentVersionsResponse.proto"
    ssm
    (Proxy :: Proxy ListDocumentVersions)

responseDeregisterPatchBaselineForPatchGroup :: DeregisterPatchBaselineForPatchGroupResponse -> TestTree
responseDeregisterPatchBaselineForPatchGroup = res
    "DeregisterPatchBaselineForPatchGroupResponse"
    "fixture/DeregisterPatchBaselineForPatchGroupResponse.proto"
    ssm
    (Proxy :: Proxy DeregisterPatchBaselineForPatchGroup)

responseDescribePatchGroups :: DescribePatchGroupsResponse -> TestTree
responseDescribePatchGroups = res
    "DescribePatchGroupsResponse"
    "fixture/DescribePatchGroupsResponse.proto"
    ssm
    (Proxy :: Proxy DescribePatchGroups)

responseGetMaintenanceWindow :: GetMaintenanceWindowResponse -> TestTree
responseGetMaintenanceWindow = res
    "GetMaintenanceWindowResponse"
    "fixture/GetMaintenanceWindowResponse.proto"
    ssm
    (Proxy :: Proxy GetMaintenanceWindow)

responseDescribeMaintenanceWindows :: DescribeMaintenanceWindowsResponse -> TestTree
responseDescribeMaintenanceWindows = res
    "DescribeMaintenanceWindowsResponse"
    "fixture/DescribeMaintenanceWindowsResponse.proto"
    ssm
    (Proxy :: Proxy DescribeMaintenanceWindows)

responseRegisterTaskWithMaintenanceWindow :: RegisterTaskWithMaintenanceWindowResponse -> TestTree
responseRegisterTaskWithMaintenanceWindow = res
    "RegisterTaskWithMaintenanceWindowResponse"
    "fixture/RegisterTaskWithMaintenanceWindowResponse.proto"
    ssm
    (Proxy :: Proxy RegisterTaskWithMaintenanceWindow)

responseRegisterDefaultPatchBaseline :: RegisterDefaultPatchBaselineResponse -> TestTree
responseRegisterDefaultPatchBaseline = res
    "RegisterDefaultPatchBaselineResponse"
    "fixture/RegisterDefaultPatchBaselineResponse.proto"
    ssm
    (Proxy :: Proxy RegisterDefaultPatchBaseline)

responseListResourceComplianceSummaries :: ListResourceComplianceSummariesResponse -> TestTree
responseListResourceComplianceSummaries = res
    "ListResourceComplianceSummariesResponse"
    "fixture/ListResourceComplianceSummariesResponse.proto"
    ssm
    (Proxy :: Proxy ListResourceComplianceSummaries)

responseListAssociationVersions :: ListAssociationVersionsResponse -> TestTree
responseListAssociationVersions = res
    "ListAssociationVersionsResponse"
    "fixture/ListAssociationVersionsResponse.proto"
    ssm
    (Proxy :: Proxy ListAssociationVersions)

responseUpdateServiceSetting :: UpdateServiceSettingResponse -> TestTree
responseUpdateServiceSetting = res
    "UpdateServiceSettingResponse"
    "fixture/UpdateServiceSettingResponse.proto"
    ssm
    (Proxy :: Proxy UpdateServiceSetting)

responseDescribeMaintenanceWindowTasks :: DescribeMaintenanceWindowTasksResponse -> TestTree
responseDescribeMaintenanceWindowTasks = res
    "DescribeMaintenanceWindowTasksResponse"
    "fixture/DescribeMaintenanceWindowTasksResponse.proto"
    ssm
    (Proxy :: Proxy DescribeMaintenanceWindowTasks)

responseDescribeInstanceAssociationsStatus :: DescribeInstanceAssociationsStatusResponse -> TestTree
responseDescribeInstanceAssociationsStatus = res
    "DescribeInstanceAssociationsStatusResponse"
    "fixture/DescribeInstanceAssociationsStatusResponse.proto"
    ssm
    (Proxy :: Proxy DescribeInstanceAssociationsStatus)

responseDeregisterTaskFromMaintenanceWindow :: DeregisterTaskFromMaintenanceWindowResponse -> TestTree
responseDeregisterTaskFromMaintenanceWindow = res
    "DeregisterTaskFromMaintenanceWindowResponse"
    "fixture/DeregisterTaskFromMaintenanceWindowResponse.proto"
    ssm
    (Proxy :: Proxy DeregisterTaskFromMaintenanceWindow)

responseListInventoryEntries :: ListInventoryEntriesResponse -> TestTree
responseListInventoryEntries = res
    "ListInventoryEntriesResponse"
    "fixture/ListInventoryEntriesResponse.proto"
    ssm
    (Proxy :: Proxy ListInventoryEntries)

responseLabelParameterVersion :: LabelParameterVersionResponse -> TestTree
responseLabelParameterVersion = res
    "LabelParameterVersionResponse"
    "fixture/LabelParameterVersionResponse.proto"
    ssm
    (Proxy :: Proxy LabelParameterVersion)

responseUpdateMaintenanceWindowTask :: UpdateMaintenanceWindowTaskResponse -> TestTree
responseUpdateMaintenanceWindowTask = res
    "UpdateMaintenanceWindowTaskResponse"
    "fixture/UpdateMaintenanceWindowTaskResponse.proto"
    ssm
    (Proxy :: Proxy UpdateMaintenanceWindowTask)

responseGetParameterHistory :: GetParameterHistoryResponse -> TestTree
responseGetParameterHistory = res
    "GetParameterHistoryResponse"
    "fixture/GetParameterHistoryResponse.proto"
    ssm
    (Proxy :: Proxy GetParameterHistory)

responseDescribeAssociationExecutions :: DescribeAssociationExecutionsResponse -> TestTree
responseDescribeAssociationExecutions = res
    "DescribeAssociationExecutionsResponse"
    "fixture/DescribeAssociationExecutionsResponse.proto"
    ssm
    (Proxy :: Proxy DescribeAssociationExecutions)

responseGetServiceSetting :: GetServiceSettingResponse -> TestTree
responseGetServiceSetting = res
    "GetServiceSettingResponse"
    "fixture/GetServiceSettingResponse.proto"
    ssm
    (Proxy :: Proxy GetServiceSetting)

responseStartAssociationsOnce :: StartAssociationsOnceResponse -> TestTree
responseStartAssociationsOnce = res
    "StartAssociationsOnceResponse"
    "fixture/StartAssociationsOnceResponse.proto"
    ssm
    (Proxy :: Proxy StartAssociationsOnce)

responseCreateMaintenanceWindow :: CreateMaintenanceWindowResponse -> TestTree
responseCreateMaintenanceWindow = res
    "CreateMaintenanceWindowResponse"
    "fixture/CreateMaintenanceWindowResponse.proto"
    ssm
    (Proxy :: Proxy CreateMaintenanceWindow)

responseStopAutomationExecution :: StopAutomationExecutionResponse -> TestTree
responseStopAutomationExecution = res
    "StopAutomationExecutionResponse"
    "fixture/StopAutomationExecutionResponse.proto"
    ssm
    (Proxy :: Proxy StopAutomationExecution)

responseGetMaintenanceWindowExecution :: GetMaintenanceWindowExecutionResponse -> TestTree
responseGetMaintenanceWindowExecution = res
    "GetMaintenanceWindowExecutionResponse"
    "fixture/GetMaintenanceWindowExecutionResponse.proto"
    ssm
    (Proxy :: Proxy GetMaintenanceWindowExecution)

responseSendAutomationSignal :: SendAutomationSignalResponse -> TestTree
responseSendAutomationSignal = res
    "SendAutomationSignalResponse"
    "fixture/SendAutomationSignalResponse.proto"
    ssm
    (Proxy :: Proxy SendAutomationSignal)

responsePutParameter :: PutParameterResponse -> TestTree
responsePutParameter = res
    "PutParameterResponse"
    "fixture/PutParameterResponse.proto"
    ssm
    (Proxy :: Proxy PutParameter)

responseDescribeMaintenanceWindowExecutionTaskInvocations :: DescribeMaintenanceWindowExecutionTaskInvocationsResponse -> TestTree
responseDescribeMaintenanceWindowExecutionTaskInvocations = res
    "DescribeMaintenanceWindowExecutionTaskInvocationsResponse"
    "fixture/DescribeMaintenanceWindowExecutionTaskInvocationsResponse.proto"
    ssm
    (Proxy :: Proxy DescribeMaintenanceWindowExecutionTaskInvocations)

responseGetMaintenanceWindowExecutionTaskInvocation :: GetMaintenanceWindowExecutionTaskInvocationResponse -> TestTree
responseGetMaintenanceWindowExecutionTaskInvocation = res
    "GetMaintenanceWindowExecutionTaskInvocationResponse"
    "fixture/GetMaintenanceWindowExecutionTaskInvocationResponse.proto"
    ssm
    (Proxy :: Proxy GetMaintenanceWindowExecutionTaskInvocation)

responseDeleteParameter :: DeleteParameterResponse -> TestTree
responseDeleteParameter = res
    "DeleteParameterResponse"
    "fixture/DeleteParameterResponse.proto"
    ssm
    (Proxy :: Proxy DeleteParameter)

responseDescribeInstanceInformation :: DescribeInstanceInformationResponse -> TestTree
responseDescribeInstanceInformation = res
    "DescribeInstanceInformationResponse"
    "fixture/DescribeInstanceInformationResponse.proto"
    ssm
    (Proxy :: Proxy DescribeInstanceInformation)

responseListAssociations :: ListAssociationsResponse -> TestTree
responseListAssociations = res
    "ListAssociationsResponse"
    "fixture/ListAssociationsResponse.proto"
    ssm
    (Proxy :: Proxy ListAssociations)

responseUpdateOpsItem :: UpdateOpsItemResponse -> TestTree
responseUpdateOpsItem = res
    "UpdateOpsItemResponse"
    "fixture/UpdateOpsItemResponse.proto"
    ssm
    (Proxy :: Proxy UpdateOpsItem)

responseDeleteAssociation :: DeleteAssociationResponse -> TestTree
responseDeleteAssociation = res
    "DeleteAssociationResponse"
    "fixture/DeleteAssociationResponse.proto"
    ssm
    (Proxy :: Proxy DeleteAssociation)

responseUpdateAssociation :: UpdateAssociationResponse -> TestTree
responseUpdateAssociation = res
    "UpdateAssociationResponse"
    "fixture/UpdateAssociationResponse.proto"
    ssm
    (Proxy :: Proxy UpdateAssociation)

responseDescribeInventoryDeletions :: DescribeInventoryDeletionsResponse -> TestTree
responseDescribeInventoryDeletions = res
    "DescribeInventoryDeletionsResponse"
    "fixture/DescribeInventoryDeletionsResponse.proto"
    ssm
    (Proxy :: Proxy DescribeInventoryDeletions)

responseDeleteInventory :: DeleteInventoryResponse -> TestTree
responseDeleteInventory = res
    "DeleteInventoryResponse"
    "fixture/DeleteInventoryResponse.proto"
    ssm
    (Proxy :: Proxy DeleteInventory)

responsePutInventory :: PutInventoryResponse -> TestTree
responsePutInventory = res
    "PutInventoryResponse"
    "fixture/PutInventoryResponse.proto"
    ssm
    (Proxy :: Proxy PutInventory)

responseDescribeEffectiveInstanceAssociations :: DescribeEffectiveInstanceAssociationsResponse -> TestTree
responseDescribeEffectiveInstanceAssociations = res
    "DescribeEffectiveInstanceAssociationsResponse"
    "fixture/DescribeEffectiveInstanceAssociationsResponse.proto"
    ssm
    (Proxy :: Proxy DescribeEffectiveInstanceAssociations)

responseDescribeAutomationExecutions :: DescribeAutomationExecutionsResponse -> TestTree
responseDescribeAutomationExecutions = res
    "DescribeAutomationExecutionsResponse"
    "fixture/DescribeAutomationExecutionsResponse.proto"
    ssm
    (Proxy :: Proxy DescribeAutomationExecutions)

responseGetAutomationExecution :: GetAutomationExecutionResponse -> TestTree
responseGetAutomationExecution = res
    "GetAutomationExecutionResponse"
    "fixture/GetAutomationExecutionResponse.proto"
    ssm
    (Proxy :: Proxy GetAutomationExecution)

responseSendCommand :: SendCommandResponse -> TestTree
responseSendCommand = res
    "SendCommandResponse"
    "fixture/SendCommandResponse.proto"
    ssm
    (Proxy :: Proxy SendCommand)

responseDescribePatchBaselines :: DescribePatchBaselinesResponse -> TestTree
responseDescribePatchBaselines = res
    "DescribePatchBaselinesResponse"
    "fixture/DescribePatchBaselinesResponse.proto"
    ssm
    (Proxy :: Proxy DescribePatchBaselines)

responseGetPatchBaseline :: GetPatchBaselineResponse -> TestTree
responseGetPatchBaseline = res
    "GetPatchBaselineResponse"
    "fixture/GetPatchBaselineResponse.proto"
    ssm
    (Proxy :: Proxy GetPatchBaseline)

responseRegisterTargetWithMaintenanceWindow :: RegisterTargetWithMaintenanceWindowResponse -> TestTree
responseRegisterTargetWithMaintenanceWindow = res
    "RegisterTargetWithMaintenanceWindowResponse"
    "fixture/RegisterTargetWithMaintenanceWindowResponse.proto"
    ssm
    (Proxy :: Proxy RegisterTargetWithMaintenanceWindow)

responseStartSession :: StartSessionResponse -> TestTree
responseStartSession = res
    "StartSessionResponse"
    "fixture/StartSessionResponse.proto"
    ssm
    (Proxy :: Proxy StartSession)

responseListCommands :: ListCommandsResponse -> TestTree
responseListCommands = res
    "ListCommandsResponse"
    "fixture/ListCommandsResponse.proto"
    ssm
    (Proxy :: Proxy ListCommands)

responseUpdateDocument :: UpdateDocumentResponse -> TestTree
responseUpdateDocument = res
    "UpdateDocumentResponse"
    "fixture/UpdateDocumentResponse.proto"
    ssm
    (Proxy :: Proxy UpdateDocument)

responseDeleteDocument :: DeleteDocumentResponse -> TestTree
responseDeleteDocument = res
    "DeleteDocumentResponse"
    "fixture/DeleteDocumentResponse.proto"
    ssm
    (Proxy :: Proxy DeleteDocument)

responseDescribeDocumentPermission :: DescribeDocumentPermissionResponse -> TestTree
responseDescribeDocumentPermission = res
    "DescribeDocumentPermissionResponse"
    "fixture/DescribeDocumentPermissionResponse.proto"
    ssm
    (Proxy :: Proxy DescribeDocumentPermission)

responseCreateAssociationBatch :: CreateAssociationBatchResponse -> TestTree
responseCreateAssociationBatch = res
    "CreateAssociationBatchResponse"
    "fixture/CreateAssociationBatchResponse.proto"
    ssm
    (Proxy :: Proxy CreateAssociationBatch)

responseUpdateMaintenanceWindowTarget :: UpdateMaintenanceWindowTargetResponse -> TestTree
responseUpdateMaintenanceWindowTarget = res
    "UpdateMaintenanceWindowTargetResponse"
    "fixture/UpdateMaintenanceWindowTargetResponse.proto"
    ssm
    (Proxy :: Proxy UpdateMaintenanceWindowTarget)

responseCreateResourceDataSync :: CreateResourceDataSyncResponse -> TestTree
responseCreateResourceDataSync = res
    "CreateResourceDataSyncResponse"
    "fixture/CreateResourceDataSyncResponse.proto"
    ssm
    (Proxy :: Proxy CreateResourceDataSync)

responseCreatePatchBaseline :: CreatePatchBaselineResponse -> TestTree
responseCreatePatchBaseline = res
    "CreatePatchBaselineResponse"
    "fixture/CreatePatchBaselineResponse.proto"
    ssm
    (Proxy :: Proxy CreatePatchBaseline)

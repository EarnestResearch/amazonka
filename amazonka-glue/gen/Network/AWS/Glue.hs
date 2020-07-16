{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS Glue__ 
--
-- Defines the public endpoint for the AWS Glue service.
--
module Network.AWS.Glue
    (
    -- * Service Configuration
      glue

    -- * Errors
    -- $errors

    -- ** NoScheduleException
    , _NoScheduleException

    -- ** IdempotentParameterMismatchException
    , _IdempotentParameterMismatchException

    -- ** CrawlerStoppingException
    , _CrawlerStoppingException

    -- ** ResourceNumberLimitExceededException
    , _ResourceNumberLimitExceededException

    -- ** ConcurrentRunsExceededException
    , _ConcurrentRunsExceededException

    -- ** InternalServiceException
    , _InternalServiceException

    -- ** SchedulerNotRunningException
    , _SchedulerNotRunningException

    -- ** SchedulerTransitioningException
    , _SchedulerTransitioningException

    -- ** SchedulerRunningException
    , _SchedulerRunningException

    -- ** AccessDeniedException
    , _AccessDeniedException

    -- ** CrawlerRunningException
    , _CrawlerRunningException

    -- ** ValidationException
    , _ValidationException

    -- ** MLTransformNotReadyException
    , _MLTransformNotReadyException

    -- ** CrawlerNotRunningException
    , _CrawlerNotRunningException

    -- ** VersionMismatchException
    , _VersionMismatchException

    -- ** GlueEncryptionException
    , _GlueEncryptionException

    -- ** InvalidInputException
    , _InvalidInputException

    -- ** ConditionCheckFailureException
    , _ConditionCheckFailureException

    -- ** ConcurrentModificationException
    , _ConcurrentModificationException

    -- ** EntityNotFoundException
    , _EntityNotFoundException

    -- ** OperationTimeoutException
    , _OperationTimeoutException

    -- ** AlreadyExistsException
    , _AlreadyExistsException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** StartImportLabelsTaskRun 
    , module Network.AWS.Glue.StartImportLabelsTaskRun

    -- ** UpdateMLTransform 
    , module Network.AWS.Glue.UpdateMLTransform

    -- ** DeleteMLTransform 
    , module Network.AWS.Glue.DeleteMLTransform

    -- ** StartCrawler 
    , module Network.AWS.Glue.StartCrawler

    -- ** GetCatalogImportStatus 
    , module Network.AWS.Glue.GetCatalogImportStatus

    -- ** ListMLTransforms 
    , module Network.AWS.Glue.ListMLTransforms

    -- ** GetPartition 
    , module Network.AWS.Glue.GetPartition

    -- ** CreateTrigger 
    , module Network.AWS.Glue.CreateTrigger

    -- ** DeleteTable 
    , module Network.AWS.Glue.DeleteTable

    -- ** UpdateTable 
    , module Network.AWS.Glue.UpdateTable

    -- ** GetWorkflowRuns 
    , module Network.AWS.Glue.GetWorkflowRuns

    -- ** CreateWorkflow 
    , module Network.AWS.Glue.CreateWorkflow

    -- ** DeleteConnection 
    , module Network.AWS.Glue.DeleteConnection

    -- ** UpdateConnection 
    , module Network.AWS.Glue.UpdateConnection

    -- ** GetUserDefinedFunctions (Paginated)
    , module Network.AWS.Glue.GetUserDefinedFunctions

    -- ** GetTags 
    , module Network.AWS.Glue.GetTags

    -- ** GetDataCatalogEncryptionSettings 
    , module Network.AWS.Glue.GetDataCatalogEncryptionSettings

    -- ** BatchCreatePartition 
    , module Network.AWS.Glue.BatchCreatePartition

    -- ** GetMapping 
    , module Network.AWS.Glue.GetMapping

    -- ** DeleteWorkflow 
    , module Network.AWS.Glue.DeleteWorkflow

    -- ** UpdateWorkflow 
    , module Network.AWS.Glue.UpdateWorkflow

    -- ** GetTableVersion 
    , module Network.AWS.Glue.GetTableVersion

    -- ** CreateSecurityConfiguration 
    , module Network.AWS.Glue.CreateSecurityConfiguration

    -- ** StartWorkflowRun 
    , module Network.AWS.Glue.StartWorkflowRun

    -- ** GetJobs (Paginated)
    , module Network.AWS.Glue.GetJobs

    -- ** BatchGetWorkflows 
    , module Network.AWS.Glue.BatchGetWorkflows

    -- ** GetClassifiers (Paginated)
    , module Network.AWS.Glue.GetClassifiers

    -- ** CreateConnection 
    , module Network.AWS.Glue.CreateConnection

    -- ** GetWorkflowRunProperties 
    , module Network.AWS.Glue.GetWorkflowRunProperties

    -- ** BatchGetDevEndpoints 
    , module Network.AWS.Glue.BatchGetDevEndpoints

    -- ** DeleteTableVersion 
    , module Network.AWS.Glue.DeleteTableVersion

    -- ** DeleteDevEndpoint 
    , module Network.AWS.Glue.DeleteDevEndpoint

    -- ** UpdateDevEndpoint 
    , module Network.AWS.Glue.UpdateDevEndpoint

    -- ** GetWorkflow 
    , module Network.AWS.Glue.GetWorkflow

    -- ** BatchGetCrawlers 
    , module Network.AWS.Glue.BatchGetCrawlers

    -- ** GetJobBookmark 
    , module Network.AWS.Glue.GetJobBookmark

    -- ** DeleteCrawler 
    , module Network.AWS.Glue.DeleteCrawler

    -- ** UpdateCrawler 
    , module Network.AWS.Glue.UpdateCrawler

    -- ** StartExportLabelsTaskRun 
    , module Network.AWS.Glue.StartExportLabelsTaskRun

    -- ** GetSecurityConfiguration 
    , module Network.AWS.Glue.GetSecurityConfiguration

    -- ** GetConnection 
    , module Network.AWS.Glue.GetConnection

    -- ** BatchGetPartition 
    , module Network.AWS.Glue.BatchGetPartition

    -- ** StopTrigger 
    , module Network.AWS.Glue.StopTrigger

    -- ** UpdateCrawlerSchedule 
    , module Network.AWS.Glue.UpdateCrawlerSchedule

    -- ** StartMLEvaluationTaskRun 
    , module Network.AWS.Glue.StartMLEvaluationTaskRun

    -- ** DeleteUserDefinedFunction 
    , module Network.AWS.Glue.DeleteUserDefinedFunction

    -- ** UpdateUserDefinedFunction 
    , module Network.AWS.Glue.UpdateUserDefinedFunction

    -- ** BatchDeleteTable 
    , module Network.AWS.Glue.BatchDeleteTable

    -- ** CancelMLTaskRun 
    , module Network.AWS.Glue.CancelMLTaskRun

    -- ** GetTables (Paginated)
    , module Network.AWS.Glue.GetTables

    -- ** CreateClassifier 
    , module Network.AWS.Glue.CreateClassifier

    -- ** BatchDeleteConnection 
    , module Network.AWS.Glue.BatchDeleteConnection

    -- ** CreateJob 
    , module Network.AWS.Glue.CreateJob

    -- ** GetJobRuns (Paginated)
    , module Network.AWS.Glue.GetJobRuns

    -- ** CreateUserDefinedFunction 
    , module Network.AWS.Glue.CreateUserDefinedFunction

    -- ** ResetJobBookmark 
    , module Network.AWS.Glue.ResetJobBookmark

    -- ** ListJobs 
    , module Network.AWS.Glue.ListJobs

    -- ** DeleteJob 
    , module Network.AWS.Glue.DeleteJob

    -- ** UpdateJob 
    , module Network.AWS.Glue.UpdateJob

    -- ** GetCrawlers (Paginated)
    , module Network.AWS.Glue.GetCrawlers

    -- ** ListTriggers 
    , module Network.AWS.Glue.ListTriggers

    -- ** GetClassifier 
    , module Network.AWS.Glue.GetClassifier

    -- ** GetJob 
    , module Network.AWS.Glue.GetJob

    -- ** BatchDeleteTableVersion 
    , module Network.AWS.Glue.BatchDeleteTableVersion

    -- ** GetDevEndpoints (Paginated)
    , module Network.AWS.Glue.GetDevEndpoints

    -- ** StartCrawlerSchedule 
    , module Network.AWS.Glue.StartCrawlerSchedule

    -- ** GetUserDefinedFunction 
    , module Network.AWS.Glue.GetUserDefinedFunction

    -- ** GetResourcePolicy 
    , module Network.AWS.Glue.GetResourcePolicy

    -- ** GetWorkflowRun 
    , module Network.AWS.Glue.GetWorkflowRun

    -- ** DeleteDatabase 
    , module Network.AWS.Glue.DeleteDatabase

    -- ** UpdateDatabase 
    , module Network.AWS.Glue.UpdateDatabase

    -- ** StopCrawler 
    , module Network.AWS.Glue.StopCrawler

    -- ** DeleteSecurityConfiguration 
    , module Network.AWS.Glue.DeleteSecurityConfiguration

    -- ** GetPartitions (Paginated)
    , module Network.AWS.Glue.GetPartitions

    -- ** BatchDeletePartition 
    , module Network.AWS.Glue.BatchDeletePartition

    -- ** StartMLLabelingSetGenerationTaskRun 
    , module Network.AWS.Glue.StartMLLabelingSetGenerationTaskRun

    -- ** GetCrawler 
    , module Network.AWS.Glue.GetCrawler

    -- ** ListWorkflows 
    , module Network.AWS.Glue.ListWorkflows

    -- ** BatchStopJobRun 
    , module Network.AWS.Glue.BatchStopJobRun

    -- ** GetDevEndpoint 
    , module Network.AWS.Glue.GetDevEndpoint

    -- ** PutWorkflowRunProperties 
    , module Network.AWS.Glue.PutWorkflowRunProperties

    -- ** CreateTable 
    , module Network.AWS.Glue.CreateTable

    -- ** ListCrawlers 
    , module Network.AWS.Glue.ListCrawlers

    -- ** GetCrawlerMetrics (Paginated)
    , module Network.AWS.Glue.GetCrawlerMetrics

    -- ** GetPlan 
    , module Network.AWS.Glue.GetPlan

    -- ** GetTriggers (Paginated)
    , module Network.AWS.Glue.GetTriggers

    -- ** ListDevEndpoints 
    , module Network.AWS.Glue.ListDevEndpoints

    -- ** StartTrigger 
    , module Network.AWS.Glue.StartTrigger

    -- ** GetDataflowGraph 
    , module Network.AWS.Glue.GetDataflowGraph

    -- ** GetDatabases (Paginated)
    , module Network.AWS.Glue.GetDatabases

    -- ** GetTable 
    , module Network.AWS.Glue.GetTable

    -- ** CreateCrawler 
    , module Network.AWS.Glue.CreateCrawler

    -- ** GetJobRun 
    , module Network.AWS.Glue.GetJobRun

    -- ** CreateDevEndpoint 
    , module Network.AWS.Glue.CreateDevEndpoint

    -- ** GetMLTaskRuns 
    , module Network.AWS.Glue.GetMLTaskRuns

    -- ** TagResource 
    , module Network.AWS.Glue.TagResource

    -- ** PutDataCatalogEncryptionSettings 
    , module Network.AWS.Glue.PutDataCatalogEncryptionSettings

    -- ** GetMLTransforms 
    , module Network.AWS.Glue.GetMLTransforms

    -- ** GetDatabase 
    , module Network.AWS.Glue.GetDatabase

    -- ** GetMLTaskRun 
    , module Network.AWS.Glue.GetMLTaskRun

    -- ** DeletePartition 
    , module Network.AWS.Glue.DeletePartition

    -- ** UpdatePartition 
    , module Network.AWS.Glue.UpdatePartition

    -- ** GetMLTransform 
    , module Network.AWS.Glue.GetMLTransform

    -- ** CreateScript 
    , module Network.AWS.Glue.CreateScript

    -- ** PutResourcePolicy 
    , module Network.AWS.Glue.PutResourcePolicy

    -- ** GetSecurityConfigurations (Paginated)
    , module Network.AWS.Glue.GetSecurityConfigurations

    -- ** DeleteResourcePolicy 
    , module Network.AWS.Glue.DeleteResourcePolicy

    -- ** GetConnections (Paginated)
    , module Network.AWS.Glue.GetConnections

    -- ** UntagResource 
    , module Network.AWS.Glue.UntagResource

    -- ** SearchTables 
    , module Network.AWS.Glue.SearchTables

    -- ** GetTrigger 
    , module Network.AWS.Glue.GetTrigger

    -- ** BatchGetJobs 
    , module Network.AWS.Glue.BatchGetJobs

    -- ** ImportCatalogToGlue 
    , module Network.AWS.Glue.ImportCatalogToGlue

    -- ** DeleteClassifier 
    , module Network.AWS.Glue.DeleteClassifier

    -- ** UpdateClassifier 
    , module Network.AWS.Glue.UpdateClassifier

    -- ** StartJobRun 
    , module Network.AWS.Glue.StartJobRun

    -- ** CreatePartition 
    , module Network.AWS.Glue.CreatePartition

    -- ** BatchGetTriggers 
    , module Network.AWS.Glue.BatchGetTriggers

    -- ** StopCrawlerSchedule 
    , module Network.AWS.Glue.StopCrawlerSchedule

    -- ** CreateDatabase 
    , module Network.AWS.Glue.CreateDatabase

    -- ** GetTableVersions (Paginated)
    , module Network.AWS.Glue.GetTableVersions

    -- ** CreateMLTransform 
    , module Network.AWS.Glue.CreateMLTransform

    -- ** DeleteTrigger 
    , module Network.AWS.Glue.DeleteTrigger

    -- ** UpdateTrigger 
    , module Network.AWS.Glue.UpdateTrigger

    -- * Types

    -- ** CSVHeaderOption
    , CSVHeaderOption (..)

    -- ** CatalogEncryptionMode
    , CatalogEncryptionMode (..)

    -- ** CloudWatchEncryptionMode
    , CloudWatchEncryptionMode (..)

    -- ** Comparator
    , Comparator (..)

    -- ** ConnectionPropertyKey
    , ConnectionPropertyKey (..)

    -- ** ConnectionType
    , ConnectionType (..)

    -- ** CrawlState
    , CrawlState (..)

    -- ** CrawlerState
    , CrawlerState (..)

    -- ** DeleteBehavior
    , DeleteBehavior (..)

    -- ** ExistCondition
    , ExistCondition (..)

    -- ** JobBookmarksEncryptionMode
    , JobBookmarksEncryptionMode (..)

    -- ** JobRunState
    , JobRunState (..)

    -- ** Language
    , Language (..)

    -- ** LastCrawlStatus
    , LastCrawlStatus (..)

    -- ** Logical
    , Logical (..)

    -- ** LogicalOperator
    , LogicalOperator (..)

    -- ** NodeType
    , NodeType (..)

    -- ** Permission
    , Permission (..)

    -- ** PrincipalType
    , PrincipalType (..)

    -- ** ResourceType
    , ResourceType (..)

    -- ** S3EncryptionMode
    , S3EncryptionMode (..)

    -- ** ScheduleState
    , ScheduleState (..)

    -- ** Sort
    , Sort (..)

    -- ** SortDirectionType
    , SortDirectionType (..)

    -- ** TaskRunSortColumnType
    , TaskRunSortColumnType (..)

    -- ** TaskStatusType
    , TaskStatusType (..)

    -- ** TaskType
    , TaskType (..)

    -- ** TransformSortColumnType
    , TransformSortColumnType (..)

    -- ** TransformStatusType
    , TransformStatusType (..)

    -- ** TransformType
    , TransformType (..)

    -- ** TriggerState
    , TriggerState (..)

    -- ** TriggerType
    , TriggerType (..)

    -- ** UpdateBehavior
    , UpdateBehavior (..)

    -- ** WorkerType
    , WorkerType (..)

    -- ** WorkflowRunStatus
    , WorkflowRunStatus (..)

    -- ** Action
    , Action
    , action
    , aNotificationProperty
    , aArguments
    , aJobName
    , aSecurityConfiguration
    , aTimeout
    , aCrawlerName

    -- ** BatchStopJobRunError
    , BatchStopJobRunError
    , batchStopJobRunError
    , bsjreJobName
    , bsjreJobRunId
    , bsjreErrorDetail

    -- ** BatchStopJobRunSuccessfulSubmission
    , BatchStopJobRunSuccessfulSubmission
    , batchStopJobRunSuccessfulSubmission
    , bsjrssJobName
    , bsjrssJobRunId

    -- ** CSVClassifier
    , CSVClassifier
    , csvClassifier
    , ccCreationTime
    , ccQuoteSymbol
    , ccContainsHeader
    , ccLastUpdated
    , ccDisableValueTrimming
    , ccHeader
    , ccVersion
    , ccAllowSingleColumn
    , ccDelimiter
    , ccName

    -- ** CatalogEntry
    , CatalogEntry
    , catalogEntry
    , ceDatabaseName
    , ceTableName

    -- ** CatalogImportStatus
    , CatalogImportStatus
    , catalogImportStatus
    , cisImportedBy
    , cisImportTime
    , cisImportCompleted

    -- ** CatalogTarget
    , CatalogTarget
    , catalogTarget
    , ctDatabaseName
    , ctTables

    -- ** Classifier
    , Classifier
    , classifier
    , cGrokClassifier
    , cXMLClassifier
    , cCSVClassifier
    , cJSONClassifier

    -- ** CloudWatchEncryption
    , CloudWatchEncryption
    , cloudWatchEncryption
    , cweCloudWatchEncryptionMode
    , cweKMSKeyARN

    -- ** CodeGenEdge
    , CodeGenEdge
    , codeGenEdge
    , cgeTargetParameter
    , cgeSource
    , cgeTarget

    -- ** CodeGenNode
    , CodeGenNode
    , codeGenNode
    , cgnLineNumber
    , cgnId
    , cgnNodeType
    , cgnArgs

    -- ** CodeGenNodeArg
    , CodeGenNodeArg
    , codeGenNodeArg
    , cgnaParam
    , cgnaName
    , cgnaValue

    -- ** Column
    , Column
    , column
    , cParameters
    , cType
    , cComment
    , cName

    -- ** Condition
    , Condition
    , condition
    , cCrawlState
    , cState
    , cJobName
    , cLogicalOperator
    , cCrawlerName

    -- ** ConfusionMatrix
    , ConfusionMatrix
    , confusionMatrix
    , cmNumTrueNegatives
    , cmNumFalseNegatives
    , cmNumTruePositives
    , cmNumFalsePositives

    -- ** Connection
    , Connection
    , connection
    , conCreationTime
    , conLastUpdatedBy
    , conConnectionProperties
    , conLastUpdatedTime
    , conMatchCriteria
    , conPhysicalConnectionRequirements
    , conName
    , conDescription
    , conConnectionType

    -- ** ConnectionInput
    , ConnectionInput
    , connectionInput
    , ciMatchCriteria
    , ciPhysicalConnectionRequirements
    , ciDescription
    , ciName
    , ciConnectionType
    , ciConnectionProperties

    -- ** ConnectionPasswordEncryption
    , ConnectionPasswordEncryption
    , connectionPasswordEncryption
    , cpeAWSKMSKeyId
    , cpeReturnConnectionPasswordEncrypted

    -- ** ConnectionsList
    , ConnectionsList
    , connectionsList
    , clConnections

    -- ** Crawl
    , Crawl
    , crawl
    , craCompletedOn
    , craState
    , craStartedOn
    , craLogStream
    , craLogGroup
    , craErrorMessage

    -- ** Crawler
    , Crawler
    , crawler
    , crwlrCreationTime
    , crwlrState
    , crwlrSchemaChangePolicy
    , crwlrLastUpdated
    , crwlrSchedule
    , crwlrLastCrawl
    , crwlrCrawlElapsedTime
    , crwlrClassifiers
    , crwlrRole
    , crwlrName
    , crwlrTargets
    , crwlrVersion
    , crwlrDatabaseName
    , crwlrCrawlerSecurityConfiguration
    , crwlrConfiguration
    , crwlrTablePrefix
    , crwlrDescription

    -- ** CrawlerMetrics
    , CrawlerMetrics
    , crawlerMetrics
    , cmLastRuntimeSeconds
    , cmTablesCreated
    , cmStillEstimating
    , cmMedianRuntimeSeconds
    , cmTimeLeftSeconds
    , cmTablesDeleted
    , cmTablesUpdated
    , cmCrawlerName

    -- ** CrawlerNodeDetails
    , CrawlerNodeDetails
    , crawlerNodeDetails
    , cndCrawls

    -- ** CrawlerTargets
    , CrawlerTargets
    , crawlerTargets
    , ctDynamoDBTargets
    , ctS3Targets
    , ctCatalogTargets
    , ctJdbcTargets

    -- ** CreateCSVClassifierRequest
    , CreateCSVClassifierRequest
    , createCSVClassifierRequest
    , cccrQuoteSymbol
    , cccrContainsHeader
    , cccrDisableValueTrimming
    , cccrHeader
    , cccrAllowSingleColumn
    , cccrDelimiter
    , cccrName

    -- ** CreateGrokClassifierRequest
    , CreateGrokClassifierRequest
    , createGrokClassifierRequest
    , cgcrCustomPatterns
    , cgcrClassification
    , cgcrName
    , cgcrGrokPattern

    -- ** CreateJSONClassifierRequest
    , CreateJSONClassifierRequest
    , createJSONClassifierRequest
    , cjcrName
    , cjcrJSONPath

    -- ** CreateXMLClassifierRequest
    , CreateXMLClassifierRequest
    , createXMLClassifierRequest
    , cxcrRowTag
    , cxcrClassification
    , cxcrName

    -- ** DataCatalogEncryptionSettings
    , DataCatalogEncryptionSettings
    , dataCatalogEncryptionSettings
    , dcesEncryptionAtRest
    , dcesConnectionPasswordEncryption

    -- ** DataLakePrincipal
    , DataLakePrincipal
    , dataLakePrincipal
    , dlpDataLakePrincipalIdentifier

    -- ** Database
    , Database
    , database
    , dLocationURI
    , dParameters
    , dDescription
    , dCreateTime
    , dCreateTableDefaultPermissions
    , dName

    -- ** DatabaseInput
    , DatabaseInput
    , databaseInput
    , diLocationURI
    , diParameters
    , diDescription
    , diCreateTableDefaultPermissions
    , diName

    -- ** DevEndpoint
    , DevEndpoint
    , devEndpoint
    , deStatus
    , deFailureReason
    , deEndpointName
    , deNumberOfWorkers
    , deExtraPythonLibsS3Path
    , deLastUpdateStatus
    , deSecurityGroupIds
    , deLastModifiedTimestamp
    , dePublicKeys
    , deVPCId
    , deArguments
    , dePrivateAddress
    , deWorkerType
    , deSecurityConfiguration
    , dePublicKey
    , deSubnetId
    , deGlueVersion
    , deNumberOfNodes
    , dePublicAddress
    , deAvailabilityZone
    , deZeppelinRemoteSparkInterpreterPort
    , deExtraJARsS3Path
    , deCreatedTimestamp
    , deYarnEndpointAddress
    , deRoleARN

    -- ** DevEndpointCustomLibraries
    , DevEndpointCustomLibraries
    , devEndpointCustomLibraries
    , declExtraPythonLibsS3Path
    , declExtraJARsS3Path

    -- ** DynamoDBTarget
    , DynamoDBTarget
    , dynamoDBTarget
    , ddtPath

    -- ** Edge
    , Edge
    , edge
    , eSourceId
    , eDestinationId

    -- ** EncryptionAtRest
    , EncryptionAtRest
    , encryptionAtRest
    , earSseAWSKMSKeyId
    , earCatalogEncryptionMode

    -- ** EncryptionConfiguration
    , EncryptionConfiguration
    , encryptionConfiguration
    , ecS3Encryption
    , ecJobBookmarksEncryption
    , ecCloudWatchEncryption

    -- ** ErrorDetail
    , ErrorDetail
    , errorDetail
    , edErrorCode
    , edErrorMessage

    -- ** EvaluationMetrics
    , EvaluationMetrics
    , evaluationMetrics
    , emFindMatchesMetrics
    , emTransformType

    -- ** ExecutionProperty
    , ExecutionProperty
    , executionProperty
    , epMaxConcurrentRuns

    -- ** ExportLabelsTaskRunProperties
    , ExportLabelsTaskRunProperties
    , exportLabelsTaskRunProperties
    , eltrpOutputS3Path

    -- ** FindMatchesMetrics
    , FindMatchesMetrics
    , findMatchesMetrics
    , fmmF1
    , fmmAreaUnderPRCurve
    , fmmRecall
    , fmmPrecision
    , fmmConfusionMatrix

    -- ** FindMatchesParameters
    , FindMatchesParameters
    , findMatchesParameters
    , fmpEnforceProvidedLabels
    , fmpAccuracyCostTradeoff
    , fmpPrecisionRecallTradeoff
    , fmpPrimaryKeyColumnName

    -- ** FindMatchesTaskRunProperties
    , FindMatchesTaskRunProperties
    , findMatchesTaskRunProperties
    , fmtrpJobId
    , fmtrpJobName
    , fmtrpJobRunId

    -- ** GetConnectionsFilter
    , GetConnectionsFilter
    , getConnectionsFilter
    , gcfMatchCriteria
    , gcfConnectionType

    -- ** GlueTable
    , GlueTable
    , glueTable
    , gtCatalogId
    , gtConnectionName
    , gtDatabaseName
    , gtTableName

    -- ** GrokClassifier
    , GrokClassifier
    , grokClassifier
    , gcCreationTime
    , gcLastUpdated
    , gcVersion
    , gcCustomPatterns
    , gcName
    , gcClassification
    , gcGrokPattern

    -- ** ImportLabelsTaskRunProperties
    , ImportLabelsTaskRunProperties
    , importLabelsTaskRunProperties
    , iltrpReplace
    , iltrpInputS3Path

    -- ** JSONClassifier
    , JSONClassifier
    , jsonClassifier
    , jcCreationTime
    , jcLastUpdated
    , jcVersion
    , jcName
    , jcJSONPath

    -- ** JdbcTarget
    , JdbcTarget
    , jdbcTarget
    , jtPath
    , jtConnectionName
    , jtExclusions

    -- ** Job
    , Job
    , job
    , jNumberOfWorkers
    , jCommand
    , jNotificationProperty
    , jLastModifiedOn
    , jConnections
    , jWorkerType
    , jSecurityConfiguration
    , jGlueVersion
    , jNonOverridableArguments
    , jRole
    , jName
    , jLogURI
    , jMaxRetries
    , jExecutionProperty
    , jAllocatedCapacity
    , jMaxCapacity
    , jTimeout
    , jDefaultArguments
    , jDescription
    , jCreatedOn

    -- ** JobBookmarkEntry
    , JobBookmarkEntry
    , jobBookmarkEntry
    , jbeJobName
    , jbeRun
    , jbeRunId
    , jbeVersion
    , jbePreviousRunId
    , jbeAttempt
    , jbeJobBookmark

    -- ** JobBookmarksEncryption
    , JobBookmarksEncryption
    , jobBookmarksEncryption
    , jbeJobBookmarksEncryptionMode
    , jbeKMSKeyARN

    -- ** JobCommand
    , JobCommand
    , jobCommand
    , jobScriptLocation
    , jobPythonVersion
    , jobName

    -- ** JobNodeDetails
    , JobNodeDetails
    , jobNodeDetails
    , jndJobRuns

    -- ** JobRun
    , JobRun
    , jobRun
    , jrCompletedOn
    , jrNumberOfWorkers
    , jrTriggerName
    , jrNotificationProperty
    , jrLastModifiedOn
    , jrArguments
    , jrJobName
    , jrStartedOn
    , jrWorkerType
    , jrSecurityConfiguration
    , jrGlueVersion
    , jrJobRunState
    , jrLogGroupName
    , jrExecutionTime
    , jrPredecessorRuns
    , jrPreviousRunId
    , jrId
    , jrAttempt
    , jrAllocatedCapacity
    , jrMaxCapacity
    , jrTimeout
    , jrErrorMessage

    -- ** JobUpdate
    , JobUpdate
    , jobUpdate
    , juNumberOfWorkers
    , juCommand
    , juNotificationProperty
    , juConnections
    , juWorkerType
    , juSecurityConfiguration
    , juGlueVersion
    , juNonOverridableArguments
    , juRole
    , juLogURI
    , juMaxRetries
    , juExecutionProperty
    , juAllocatedCapacity
    , juMaxCapacity
    , juTimeout
    , juDefaultArguments
    , juDescription

    -- ** LabelingSetGenerationTaskRunProperties
    , LabelingSetGenerationTaskRunProperties
    , labelingSetGenerationTaskRunProperties
    , lsgtrpOutputS3Path

    -- ** LastCrawlInfo
    , LastCrawlInfo
    , lastCrawlInfo
    , lciStatus
    , lciStartTime
    , lciLogStream
    , lciLogGroup
    , lciMessagePrefix
    , lciErrorMessage

    -- ** Location
    , Location
    , location
    , lDynamoDB
    , lJdbc
    , lS3

    -- ** MLTransform
    , MLTransform
    , mLTransform
    , mltStatus
    , mltNumberOfWorkers
    , mltLastModifiedOn
    , mltLabelCount
    , mltWorkerType
    , mltInputRecordTables
    , mltGlueVersion
    , mltEvaluationMetrics
    , mltSchema
    , mltRole
    , mltName
    , mltParameters
    , mltMaxRetries
    , mltMaxCapacity
    , mltTimeout
    , mltDescription
    , mltCreatedOn
    , mltTransformId

    -- ** MappingEntry
    , MappingEntry
    , mappingEntry
    , meTargetTable
    , meSourceType
    , meSourceTable
    , meTargetType
    , meTargetPath
    , meSourcePath

    -- ** Node
    , Node
    , node
    , nTriggerDetails
    , nUniqueId
    , nCrawlerDetails
    , nName
    , nJobDetails
    , nType

    -- ** NotificationProperty
    , NotificationProperty
    , notificationProperty
    , npNotifyDelayAfter

    -- ** Order
    , Order
    , order
    , oColumn
    , oSortOrder

    -- ** Partition
    , Partition
    , partition
    , pCreationTime
    , pValues
    , pLastAnalyzedTime
    , pStorageDescriptor
    , pDatabaseName
    , pParameters
    , pLastAccessTime
    , pTableName

    -- ** PartitionError
    , PartitionError
    , partitionError
    , pePartitionValues
    , peErrorDetail

    -- ** PartitionInput
    , PartitionInput
    , partitionInput
    , piValues
    , piLastAnalyzedTime
    , piStorageDescriptor
    , piParameters
    , piLastAccessTime

    -- ** PartitionValueList
    , PartitionValueList
    , partitionValueList
    , pvlValues

    -- ** PhysicalConnectionRequirements
    , PhysicalConnectionRequirements
    , physicalConnectionRequirements
    , pcrSecurityGroupIdList
    , pcrSubnetId
    , pcrAvailabilityZone

    -- ** Predecessor
    , Predecessor
    , predecessor
    , pJobName
    , pRunId

    -- ** Predicate
    , Predicate
    , predicate
    , pLogical
    , pConditions

    -- ** PrincipalPermissions
    , PrincipalPermissions
    , principalPermissions
    , ppPrincipal
    , ppPermissions

    -- ** PropertyPredicate
    , PropertyPredicate
    , propertyPredicate
    , ppValue
    , ppKey
    , ppComparator

    -- ** ResourceURI
    , ResourceURI
    , resourceURI
    , ruResourceType
    , ruURI

    -- ** S3Encryption
    , S3Encryption
    , s3Encryption
    , seS3EncryptionMode
    , seKMSKeyARN

    -- ** S3Target
    , S3Target
    , s3Target
    , stPath
    , stExclusions

    -- ** Schedule
    , Schedule
    , schedule
    , sState
    , sScheduleExpression

    -- ** SchemaChangePolicy
    , SchemaChangePolicy
    , schemaChangePolicy
    , scpDeleteBehavior
    , scpUpdateBehavior

    -- ** SchemaColumn
    , SchemaColumn
    , schemaColumn
    , sName
    , sDataType

    -- ** SecurityConfiguration
    , SecurityConfiguration
    , securityConfiguration
    , secName
    , secEncryptionConfiguration
    , secCreatedTimeStamp

    -- ** Segment
    , Segment
    , segment
    , sSegmentNumber
    , sTotalSegments

    -- ** SerDeInfo
    , SerDeInfo
    , serDeInfo
    , sdiSerializationLibrary
    , sdiName
    , sdiParameters

    -- ** SkewedInfo
    , SkewedInfo
    , skewedInfo
    , siSkewedColumnValueLocationMaps
    , siSkewedColumnValues
    , siSkewedColumnNames

    -- ** SortCriterion
    , SortCriterion
    , sortCriterion
    , scSort
    , scFieldName

    -- ** StorageDescriptor
    , StorageDescriptor
    , storageDescriptor
    , sdSortColumns
    , sdCompressed
    , sdLocation
    , sdBucketColumns
    , sdSerdeInfo
    , sdOutputFormat
    , sdNumberOfBuckets
    , sdStoredAsSubDirectories
    , sdParameters
    , sdInputFormat
    , sdSkewedInfo
    , sdColumns

    -- ** Table
    , Table
    , table
    , tRetention
    , tIsRegisteredWithLakeFormation
    , tCreatedBy
    , tTableType
    , tOwner
    , tViewOriginalText
    , tUpdateTime
    , tViewExpandedText
    , tLastAnalyzedTime
    , tStorageDescriptor
    , tDatabaseName
    , tParameters
    , tLastAccessTime
    , tDescription
    , tPartitionKeys
    , tCreateTime
    , tName

    -- ** TableError
    , TableError
    , tableError
    , teTableName
    , teErrorDetail

    -- ** TableInput
    , TableInput
    , tableInput
    , tiRetention
    , tiTableType
    , tiOwner
    , tiViewOriginalText
    , tiViewExpandedText
    , tiLastAnalyzedTime
    , tiStorageDescriptor
    , tiParameters
    , tiLastAccessTime
    , tiDescription
    , tiPartitionKeys
    , tiName

    -- ** TableVersion
    , TableVersion
    , tableVersion
    , tvVersionId
    , tvTable

    -- ** TableVersionError
    , TableVersionError
    , tableVersionError
    , tveVersionId
    , tveTableName
    , tveErrorDetail

    -- ** TaskRun
    , TaskRun
    , taskRun
    , trCompletedOn
    , trStatus
    , trLastModifiedOn
    , trErrorString
    , trStartedOn
    , trLogGroupName
    , trExecutionTime
    , trProperties
    , trTransformId
    , trTaskRunId

    -- ** TaskRunFilterCriteria
    , TaskRunFilterCriteria
    , taskRunFilterCriteria
    , trfcStatus
    , trfcStartedAfter
    , trfcStartedBefore
    , trfcTaskRunType

    -- ** TaskRunProperties
    , TaskRunProperties
    , taskRunProperties
    , trpTaskType
    , trpExportLabelsTaskRunProperties
    , trpLabelingSetGenerationTaskRunProperties
    , trpFindMatchesTaskRunProperties
    , trpImportLabelsTaskRunProperties

    -- ** TaskRunSortCriteria
    , TaskRunSortCriteria
    , taskRunSortCriteria
    , trscColumn
    , trscSortDirection

    -- ** TransformFilterCriteria
    , TransformFilterCriteria
    , transformFilterCriteria
    , tfcCreatedAfter
    , tfcStatus
    , tfcLastModifiedAfter
    , tfcLastModifiedBefore
    , tfcGlueVersion
    , tfcSchema
    , tfcTransformType
    , tfcName
    , tfcCreatedBefore

    -- ** TransformParameters
    , TransformParameters
    , transformParameters
    , tpFindMatchesParameters
    , tpTransformType

    -- ** TransformSortCriteria
    , TransformSortCriteria
    , transformSortCriteria
    , tscColumn
    , tscSortDirection

    -- ** Trigger
    , Trigger
    , trigger
    , triWorkflowName
    , triState
    , triActions
    , triSchedule
    , triPredicate
    , triName
    , triId
    , triType
    , triDescription

    -- ** TriggerNodeDetails
    , TriggerNodeDetails
    , triggerNodeDetails
    , tndTrigger

    -- ** TriggerUpdate
    , TriggerUpdate
    , triggerUpdate
    , tuActions
    , tuSchedule
    , tuPredicate
    , tuName
    , tuDescription

    -- ** UpdateCSVClassifierRequest
    , UpdateCSVClassifierRequest
    , updateCSVClassifierRequest
    , uccrQuoteSymbol
    , uccrContainsHeader
    , uccrDisableValueTrimming
    , uccrHeader
    , uccrAllowSingleColumn
    , uccrDelimiter
    , uccrName

    -- ** UpdateGrokClassifierRequest
    , UpdateGrokClassifierRequest
    , updateGrokClassifierRequest
    , ugcrClassification
    , ugcrCustomPatterns
    , ugcrGrokPattern
    , ugcrName

    -- ** UpdateJSONClassifierRequest
    , UpdateJSONClassifierRequest
    , updateJSONClassifierRequest
    , ujcrJSONPath
    , ujcrName

    -- ** UpdateXMLClassifierRequest
    , UpdateXMLClassifierRequest
    , updateXMLClassifierRequest
    , uxcrClassification
    , uxcrRowTag
    , uxcrName

    -- ** UserDefinedFunction
    , UserDefinedFunction
    , userDefinedFunction
    , udfOwnerName
    , udfResourceURIs
    , udfFunctionName
    , udfOwnerType
    , udfCreateTime
    , udfClassName

    -- ** UserDefinedFunctionInput
    , UserDefinedFunctionInput
    , userDefinedFunctionInput
    , udfiOwnerName
    , udfiResourceURIs
    , udfiFunctionName
    , udfiOwnerType
    , udfiClassName

    -- ** Workflow
    , Workflow
    , workflow
    , wGraph
    , wLastModifiedOn
    , wDefaultRunProperties
    , wName
    , wLastRun
    , wDescription
    , wCreatedOn

    -- ** WorkflowGraph
    , WorkflowGraph
    , workflowGraph
    , wgEdges
    , wgNodes

    -- ** WorkflowRun
    , WorkflowRun
    , workflowRun
    , wrCompletedOn
    , wrStatus
    , wrGraph
    , wrStartedOn
    , wrWorkflowRunId
    , wrName
    , wrStatistics
    , wrWorkflowRunProperties

    -- ** WorkflowRunStatistics
    , WorkflowRunStatistics
    , workflowRunStatistics
    , wrsRunningActions
    , wrsStoppedActions
    , wrsTotalActions
    , wrsFailedActions
    , wrsTimeoutActions
    , wrsSucceededActions

    -- ** XMLClassifier
    , XMLClassifier
    , xmlClassifier
    , xcCreationTime
    , xcLastUpdated
    , xcVersion
    , xcRowTag
    , xcName
    , xcClassification
    ) where

import Network.AWS.Glue.BatchCreatePartition
import Network.AWS.Glue.BatchDeleteConnection
import Network.AWS.Glue.BatchDeletePartition
import Network.AWS.Glue.BatchDeleteTable
import Network.AWS.Glue.BatchDeleteTableVersion
import Network.AWS.Glue.BatchGetCrawlers
import Network.AWS.Glue.BatchGetDevEndpoints
import Network.AWS.Glue.BatchGetJobs
import Network.AWS.Glue.BatchGetPartition
import Network.AWS.Glue.BatchGetTriggers
import Network.AWS.Glue.BatchGetWorkflows
import Network.AWS.Glue.BatchStopJobRun
import Network.AWS.Glue.CancelMLTaskRun
import Network.AWS.Glue.CreateClassifier
import Network.AWS.Glue.CreateConnection
import Network.AWS.Glue.CreateCrawler
import Network.AWS.Glue.CreateDatabase
import Network.AWS.Glue.CreateDevEndpoint
import Network.AWS.Glue.CreateJob
import Network.AWS.Glue.CreateMLTransform
import Network.AWS.Glue.CreatePartition
import Network.AWS.Glue.CreateScript
import Network.AWS.Glue.CreateSecurityConfiguration
import Network.AWS.Glue.CreateTable
import Network.AWS.Glue.CreateTrigger
import Network.AWS.Glue.CreateUserDefinedFunction
import Network.AWS.Glue.CreateWorkflow
import Network.AWS.Glue.DeleteClassifier
import Network.AWS.Glue.DeleteConnection
import Network.AWS.Glue.DeleteCrawler
import Network.AWS.Glue.DeleteDatabase
import Network.AWS.Glue.DeleteDevEndpoint
import Network.AWS.Glue.DeleteJob
import Network.AWS.Glue.DeleteMLTransform
import Network.AWS.Glue.DeletePartition
import Network.AWS.Glue.DeleteResourcePolicy
import Network.AWS.Glue.DeleteSecurityConfiguration
import Network.AWS.Glue.DeleteTable
import Network.AWS.Glue.DeleteTableVersion
import Network.AWS.Glue.DeleteTrigger
import Network.AWS.Glue.DeleteUserDefinedFunction
import Network.AWS.Glue.DeleteWorkflow
import Network.AWS.Glue.GetCatalogImportStatus
import Network.AWS.Glue.GetClassifier
import Network.AWS.Glue.GetClassifiers
import Network.AWS.Glue.GetConnection
import Network.AWS.Glue.GetConnections
import Network.AWS.Glue.GetCrawler
import Network.AWS.Glue.GetCrawlerMetrics
import Network.AWS.Glue.GetCrawlers
import Network.AWS.Glue.GetDataCatalogEncryptionSettings
import Network.AWS.Glue.GetDatabase
import Network.AWS.Glue.GetDatabases
import Network.AWS.Glue.GetDataflowGraph
import Network.AWS.Glue.GetDevEndpoint
import Network.AWS.Glue.GetDevEndpoints
import Network.AWS.Glue.GetJob
import Network.AWS.Glue.GetJobBookmark
import Network.AWS.Glue.GetJobRun
import Network.AWS.Glue.GetJobRuns
import Network.AWS.Glue.GetJobs
import Network.AWS.Glue.GetMLTaskRun
import Network.AWS.Glue.GetMLTaskRuns
import Network.AWS.Glue.GetMLTransform
import Network.AWS.Glue.GetMLTransforms
import Network.AWS.Glue.GetMapping
import Network.AWS.Glue.GetPartition
import Network.AWS.Glue.GetPartitions
import Network.AWS.Glue.GetPlan
import Network.AWS.Glue.GetResourcePolicy
import Network.AWS.Glue.GetSecurityConfiguration
import Network.AWS.Glue.GetSecurityConfigurations
import Network.AWS.Glue.GetTable
import Network.AWS.Glue.GetTableVersion
import Network.AWS.Glue.GetTableVersions
import Network.AWS.Glue.GetTables
import Network.AWS.Glue.GetTags
import Network.AWS.Glue.GetTrigger
import Network.AWS.Glue.GetTriggers
import Network.AWS.Glue.GetUserDefinedFunction
import Network.AWS.Glue.GetUserDefinedFunctions
import Network.AWS.Glue.GetWorkflow
import Network.AWS.Glue.GetWorkflowRun
import Network.AWS.Glue.GetWorkflowRunProperties
import Network.AWS.Glue.GetWorkflowRuns
import Network.AWS.Glue.ImportCatalogToGlue
import Network.AWS.Glue.ListCrawlers
import Network.AWS.Glue.ListDevEndpoints
import Network.AWS.Glue.ListJobs
import Network.AWS.Glue.ListMLTransforms
import Network.AWS.Glue.ListTriggers
import Network.AWS.Glue.ListWorkflows
import Network.AWS.Glue.PutDataCatalogEncryptionSettings
import Network.AWS.Glue.PutResourcePolicy
import Network.AWS.Glue.PutWorkflowRunProperties
import Network.AWS.Glue.ResetJobBookmark
import Network.AWS.Glue.SearchTables
import Network.AWS.Glue.StartCrawler
import Network.AWS.Glue.StartCrawlerSchedule
import Network.AWS.Glue.StartExportLabelsTaskRun
import Network.AWS.Glue.StartImportLabelsTaskRun
import Network.AWS.Glue.StartJobRun
import Network.AWS.Glue.StartMLEvaluationTaskRun
import Network.AWS.Glue.StartMLLabelingSetGenerationTaskRun
import Network.AWS.Glue.StartTrigger
import Network.AWS.Glue.StartWorkflowRun
import Network.AWS.Glue.StopCrawler
import Network.AWS.Glue.StopCrawlerSchedule
import Network.AWS.Glue.StopTrigger
import Network.AWS.Glue.TagResource
import Network.AWS.Glue.Types
import Network.AWS.Glue.UntagResource
import Network.AWS.Glue.UpdateClassifier
import Network.AWS.Glue.UpdateConnection
import Network.AWS.Glue.UpdateCrawler
import Network.AWS.Glue.UpdateCrawlerSchedule
import Network.AWS.Glue.UpdateDatabase
import Network.AWS.Glue.UpdateDevEndpoint
import Network.AWS.Glue.UpdateJob
import Network.AWS.Glue.UpdateMLTransform
import Network.AWS.Glue.UpdatePartition
import Network.AWS.Glue.UpdateTable
import Network.AWS.Glue.UpdateTrigger
import Network.AWS.Glue.UpdateUserDefinedFunction
import Network.AWS.Glue.UpdateWorkflow
import Network.AWS.Glue.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Glue'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}

{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS CodeCommit__ 
--
-- This is the /AWS CodeCommit API Reference/ . This reference provides descriptions of the operations and data types for AWS CodeCommit API along with usage examples.
--
-- You can use the AWS CodeCommit API to work with the following objects:
--
-- Repositories, by calling the following:
--
--     * 'BatchGetRepositories' , which returns information about one or more repositories associated with your AWS account.
--
--     * 'CreateRepository' , which creates an AWS CodeCommit repository.
--
--     * 'DeleteRepository' , which deletes an AWS CodeCommit repository.
--
--     * 'GetRepository' , which returns information about a specified repository.
--
--     * 'ListRepositories' , which lists all AWS CodeCommit repositories associated with your AWS account.
--
--     * 'UpdateRepositoryDescription' , which sets or updates the description of the repository.
--
--     * 'UpdateRepositoryName' , which changes the name of the repository. If you change the name of a repository, no other users of that repository can access it until you send them the new HTTPS or SSH URL to use.
--
--
--
-- Branches, by calling the following:
--
--     * 'CreateBranch' , which creates a branch in a specified repository.
--
--     * 'DeleteBranch' , which deletes the specified branch in a repository unless it is the default branch.
--
--     * 'GetBranch' , which returns information about a specified branch.
--
--     * 'ListBranches' , which lists all branches for a specified repository.
--
--     * 'UpdateDefaultBranch' , which changes the default branch for a repository.
--
--
--
-- Files, by calling the following:
--
--     * 'DeleteFile' , which deletes the content of a specified file from a specified branch.
--
--     * 'GetBlob' , which returns the base-64 encoded content of an individual Git blob object in a repository.
--
--     * 'GetFile' , which returns the base-64 encoded content of a specified file.
--
--     * 'GetFolder' , which returns the contents of a specified folder or directory.
--
--     * 'PutFile' , which adds or modifies a single file in a specified repository and branch.
--
--
--
-- Commits, by calling the following:
--
--     * 'BatchGetCommits' , which returns information about one or more commits in a repository.
--
--     * 'CreateCommit' , which creates a commit for changes to a repository.
--
--     * 'GetCommit' , which returns information about a commit, including commit messages and author and committer information.
--
--     * 'GetDifferences' , which returns information about the differences in a valid commit specifier (such as a branch, tag, HEAD, commit ID, or other fully qualified reference).
--
--
--
-- Merges, by calling the following:
--
--     * 'BatchDescribeMergeConflicts' , which returns information about conflicts in a merge between commits in a repository.
--
--     * 'CreateUnreferencedMergeCommit' , which creates an unreferenced commit between two branches or commits for the purpose of comparing them and identifying any potential conflicts.
--
--     * 'DescribeMergeConflicts' , which returns information about merge conflicts between the base, source, and destination versions of a file in a potential merge.
--
--     * 'GetMergeCommit' , which returns information about the merge between a source and destination commit. 
--
--     * 'GetMergeConflicts' , which returns information about merge conflicts between the source and destination branch in a pull request.
--
--     * 'GetMergeOptions' , which returns information about the available merge options between two branches or commit specifiers.
--
--     * 'MergeBranchesByFastForward' , which merges two branches using the fast-forward merge option.
--
--     * 'MergeBranchesBySquash' , which merges two branches using the squash merge option.
--
--     * 'MergeBranchesByThreeWay' , which merges two branches using the three-way merge option.
--
--
--
-- Pull requests, by calling the following:
--
--     * 'CreatePullRequest' , which creates a pull request in a specified repository.
--
--     * 'CreatePullRequestApprovalRule' , which creates an approval rule for a specified pull request.
--
--     * 'DeletePullRequestApprovalRule' , which deletes an approval rule for a specified pull request.
--
--     * 'DescribePullRequestEvents' , which returns information about one or more pull request events.
--
--     * 'EvaluatePullRequestApprovalRules' , which evaluates whether a pull request has met all the conditions specified in its associated approval rules.
--
--     * 'GetCommentsForPullRequest' , which returns information about comments on a specified pull request.
--
--     * 'GetPullRequest' , which returns information about a specified pull request.
--
--     * 'GetPullRequestApprovalStates' , which returns information about the approval states for a specified pull request.
--
--     * 'GetPullRequestOverrideState' , which returns information about whether approval rules have been set aside (overriden) for a pull request, and if so, the Amazon Resource Name (ARN) of the user or identity that overrode the rules and their requirements for the pull request.
--
--     * 'ListPullRequests' , which lists all pull requests for a repository.
--
--     * 'MergePullRequestByFastForward' , which merges the source destination branch of a pull request into the specified destination branch for that pull request using the fast-forward merge option.
--
--     * 'MergePullRequestBySquash' , which merges the source destination branch of a pull request into the specified destination branch for that pull request using the squash merge option.
--
--     * 'MergePullRequestByThreeWay' . which merges the source destination branch of a pull request into the specified destination branch for that pull request using the three-way merge option.
--
--     * 'OverridePullRequestApprovalRules' , which sets aside all approval rule requirements for a pull request.
--
--     * 'PostCommentForPullRequest' , which posts a comment to a pull request at the specified line, file, or request.
--
--     * 'UpdatePullRequestApprovalRuleContent' , which updates the structure of an approval rule for a pull request.
--
--     * 'UpdatePullRequestApprovalState' , which updates the state of an approval on a pull request.
--
--     * 'UpdatePullRequestDescription' , which updates the description of a pull request.
--
--     * 'UpdatePullRequestStatus' , which updates the status of a pull request.
--
--     * 'UpdatePullRequestTitle' , which updates the title of a pull request.
--
--
--
-- Approval rule templates, by calling the following:
--
--     * 'AssociateApprovalRuleTemplateWithRepository' , which associates a template with a specified repository. After the template is associated with a repository, AWS CodeCommit creates approval rules that match the template conditions on every pull request created in the specified repository.
--
--     * 'BatchAssociateApprovalRuleTemplateWithRepositories' , which associates a template with one or more specified repositories. After the template is associated with a repository, AWS CodeCommit creates approval rules that match the template conditions on every pull request created in the specified repositories.
--
--     * 'BatchDisassociateApprovalRuleTemplateFromRepositories' , which removes the association between a template and specified repositories so that approval rules based on the template are not automatically created when pull requests are created in those repositories.
--
--     * 'CreateApprovalRuleTemplate' , which creates a template for approval rules that can then be associated with one or more repositories in your AWS account.
--
--     * 'DeleteApprovalRuleTemplate' , which deletes the specified template. It does not remove approval rules on pull requests already created with the template.
--
--     * 'DisassociateApprovalRuleTemplateFromRepository' , which removes the association between a template and a repository so that approval rules based on the template are not automatically created when pull requests are created in the specified repository.
--
--     * 'GetApprovalRuleTemplate' , which returns information about an approval rule template.
--
--     * 'ListApprovalRuleTemplates' , which lists all approval rule templates in the AWS Region in your AWS account.
--
--     * 'ListAssociatedApprovalRuleTemplatesForRepository' , which lists all approval rule templates that are associated with a specified repository.
--
--     * 'ListRepositoriesForApprovalRuleTemplate' , which lists all repositories associated with the specified approval rule template.
--
--     * 'UpdateApprovalRuleTemplateDescription' , which updates the description of an approval rule template.
--
--     * 'UpdateApprovalRuleTemplateName' , which updates the name of an approval rule template.
--
--     * 'UpdateApprovalRuleTemplateContent' , which updates the content of an approval rule template.
--
--
--
-- Comments in a repository, by calling the following:
--
--     * 'DeleteCommentContent' , which deletes the content of a comment on a commit in a repository.
--
--     * 'GetComment' , which returns information about a comment on a commit.
--
--     * 'GetCommentsForComparedCommit' , which returns information about comments on the comparison between two commit specifiers in a repository.
--
--     * 'PostCommentForComparedCommit' , which creates a comment on the comparison between two commit specifiers in a repository.
--
--     * 'PostCommentReply' , which creates a reply to a comment.
--
--     * 'UpdateComment' , which updates the content of a comment on a commit in a repository.
--
--
--
-- Tags used to tag resources in AWS CodeCommit (not Git tags), by calling the following:
--
--     * 'ListTagsForResource' , which gets information about AWS tags for a specified Amazon Resource Name (ARN) in AWS CodeCommit.
--
--     * 'TagResource' , which adds or updates tags for a resource in AWS CodeCommit.
--
--     * 'UntagResource' , which removes tags for a resource in AWS CodeCommit.
--
--
--
-- Triggers, by calling the following:
--
--     * 'GetRepositoryTriggers' , which returns information about triggers configured for a repository.
--
--     * 'PutRepositoryTriggers' , which replaces all triggers for a repository and can be used to create or delete triggers.
--
--     * 'TestRepositoryTriggers' , which tests the functionality of a repository trigger by sending data to the trigger target.
--
--
--
-- For information about how to use AWS CodeCommit, see the <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit User Guide> .
--
module Network.AWS.CodeCommit
    (
    -- * Service Configuration
      codeCommit

    -- * Errors
    -- $errors

    -- ** NumberOfRulesExceededException
    , _NumberOfRulesExceededException

    -- ** FileModeRequiredException
    , _FileModeRequiredException

    -- ** PathRequiredException
    , _PathRequiredException

    -- ** FolderContentSizeLimitExceededException
    , _FolderContentSizeLimitExceededException

    -- ** RepositoryLimitExceededException
    , _RepositoryLimitExceededException

    -- ** InvalidTagsMapException
    , _InvalidTagsMapException

    -- ** DefaultBranchCannotBeDeletedException
    , _DefaultBranchCannotBeDeletedException

    -- ** InvalidDeletionParameterException
    , _InvalidDeletionParameterException

    -- ** CommitIdsListRequiredException
    , _CommitIdsListRequiredException

    -- ** TagsMapRequiredException
    , _TagsMapRequiredException

    -- ** InvalidRepositoryNameException
    , _InvalidRepositoryNameException

    -- ** BranchNameIsTagNameException
    , _BranchNameIsTagNameException

    -- ** InvalidMaxResultsException
    , _InvalidMaxResultsException

    -- ** CommentIdRequiredException
    , _CommentIdRequiredException

    -- ** InvalidApprovalRuleTemplateNameException
    , _InvalidApprovalRuleTemplateNameException

    -- ** PullRequestCannotBeApprovedByAuthorException
    , _PullRequestCannotBeApprovedByAuthorException

    -- ** InvalidOrderException
    , _InvalidOrderException

    -- ** MultipleRepositoriesInPullRequestException
    , _MultipleRepositoriesInPullRequestException

    -- ** ApprovalRuleNameRequiredException
    , _ApprovalRuleNameRequiredException

    -- ** BranchDoesNotExistException
    , _BranchDoesNotExistException

    -- ** NoChangeException
    , _NoChangeException

    -- ** BlobIdRequiredException
    , _BlobIdRequiredException

    -- ** FilePathConflictsWithSubmodulePathException
    , _FilePathConflictsWithSubmodulePathException

    -- ** InvalidCommentIdException
    , _InvalidCommentIdException

    -- ** InvalidDescriptionException
    , _InvalidDescriptionException

    -- ** InvalidRevisionIdException
    , _InvalidRevisionIdException

    -- ** IdempotencyParameterMismatchException
    , _IdempotencyParameterMismatchException

    -- ** RepositoryTriggerBranchNameListRequiredException
    , _RepositoryTriggerBranchNameListRequiredException

    -- ** InvalidClientRequestTokenException
    , _InvalidClientRequestTokenException

    -- ** InvalidRelativeFileVersionEnumException
    , _InvalidRelativeFileVersionEnumException

    -- ** InvalidConflictResolutionStrategyException
    , _InvalidConflictResolutionStrategyException

    -- ** BranchNameRequiredException
    , _BranchNameRequiredException

    -- ** TagPolicyException
    , _TagPolicyException

    -- ** RepositoryTriggersListRequiredException
    , _RepositoryTriggersListRequiredException

    -- ** EncryptionKeyUnavailableException
    , _EncryptionKeyUnavailableException

    -- ** InvalidRepositoryTriggerDestinationARNException
    , _InvalidRepositoryTriggerDestinationARNException

    -- ** RepositoryTriggerDestinationARNRequiredException
    , _RepositoryTriggerDestinationARNRequiredException

    -- ** ApprovalRuleDoesNotExistException
    , _ApprovalRuleDoesNotExistException

    -- ** MultipleConflictResolutionEntriesException
    , _MultipleConflictResolutionEntriesException

    -- ** RepositoryNameExistsException
    , _RepositoryNameExistsException

    -- ** PutFileEntryConflictException
    , _PutFileEntryConflictException

    -- ** MaximumRepositoryNamesExceededException
    , _MaximumRepositoryNamesExceededException

    -- ** InvalidCommitIdException
    , _InvalidCommitIdException

    -- ** FolderDoesNotExistException
    , _FolderDoesNotExistException

    -- ** BlobIdDoesNotExistException
    , _BlobIdDoesNotExistException

    -- ** InvalidRuleContentSha256Exception
    , _InvalidRuleContentSha256Exception

    -- ** InvalidRepositoryDescriptionException
    , _InvalidRepositoryDescriptionException

    -- ** SameFileContentException
    , _SameFileContentException

    -- ** TipOfSourceReferenceIsDifferentException
    , _TipOfSourceReferenceIsDifferentException

    -- ** InvalidEmailException
    , _InvalidEmailException

    -- ** FileContentAndSourceFileSpecifiedException
    , _FileContentAndSourceFileSpecifiedException

    -- ** ApprovalRuleTemplateInUseException
    , _ApprovalRuleTemplateInUseException

    -- ** OverrideAlreadySetException
    , _OverrideAlreadySetException

    -- ** PullRequestAlreadyClosedException
    , _PullRequestAlreadyClosedException

    -- ** CommentDoesNotExistException
    , _CommentDoesNotExistException

    -- ** InvalidRepositoryTriggerBranchNameException
    , _InvalidRepositoryTriggerBranchNameException

    -- ** InvalidRepositoryTriggerCustomDataException
    , _InvalidRepositoryTriggerCustomDataException

    -- ** MaximumOpenPullRequestsExceededException
    , _MaximumOpenPullRequestsExceededException

    -- ** ApprovalRuleTemplateNameAlreadyExistsException
    , _ApprovalRuleTemplateNameAlreadyExistsException

    -- ** EncryptionKeyDisabledException
    , _EncryptionKeyDisabledException

    -- ** InvalidDestinationCommitSpecifierException
    , _InvalidDestinationCommitSpecifierException

    -- ** InvalidCommitException
    , _InvalidCommitException

    -- ** InvalidTargetException
    , _InvalidTargetException

    -- ** ReferenceTypeNotSupportedException
    , _ReferenceTypeNotSupportedException

    -- ** DirectoryNameConflictsWithFileNameException
    , _DirectoryNameConflictsWithFileNameException

    -- ** CommentNotCreatedByCallerException
    , _CommentNotCreatedByCallerException

    -- ** InvalidSortByException
    , _InvalidSortByException

    -- ** TipsDivergenceExceededException
    , _TipsDivergenceExceededException

    -- ** InvalidContinuationTokenException
    , _InvalidContinuationTokenException

    -- ** EncryptionKeyAccessDeniedException
    , _EncryptionKeyAccessDeniedException

    -- ** InvalidRepositoryTriggerRegionException
    , _InvalidRepositoryTriggerRegionException

    -- ** CommentDeletedException
    , _CommentDeletedException

    -- ** EncryptionIntegrityChecksFailedException
    , _EncryptionIntegrityChecksFailedException

    -- ** ParentCommitDoesNotExistException
    , _ParentCommitDoesNotExistException

    -- ** InvalidApprovalRuleTemplateDescriptionException
    , _InvalidApprovalRuleTemplateDescriptionException

    -- ** TooManyTagsException
    , _TooManyTagsException

    -- ** FileContentRequiredException
    , _FileContentRequiredException

    -- ** SourceAndDestinationAreSameException
    , _SourceAndDestinationAreSameException

    -- ** RepositoryTriggerEventsListRequiredException
    , _RepositoryTriggerEventsListRequiredException

    -- ** AuthorDoesNotExistException
    , _AuthorDoesNotExistException

    -- ** SourceFileOrContentRequiredException
    , _SourceFileOrContentRequiredException

    -- ** InvalidReplacementContentException
    , _InvalidReplacementContentException

    -- ** SamePathRequestException
    , _SamePathRequestException

    -- ** CommentContentSizeLimitExceededException
    , _CommentContentSizeLimitExceededException

    -- ** CommitIdsLimitExceededException
    , _CommitIdsLimitExceededException

    -- ** MaximumBranchesExceededException
    , _MaximumBranchesExceededException

    -- ** OverrideStatusRequiredException
    , _OverrideStatusRequiredException

    -- ** InvalidPullRequestEventTypeException
    , _InvalidPullRequestEventTypeException

    -- ** MaximumFileContentToLoadExceededException
    , _MaximumFileContentToLoadExceededException

    -- ** InvalidApprovalStateException
    , _InvalidApprovalStateException

    -- ** ApprovalStateRequiredException
    , _ApprovalStateRequiredException

    -- ** ConcurrentReferenceUpdateException
    , _ConcurrentReferenceUpdateException

    -- ** InvalidParentCommitIdException
    , _InvalidParentCommitIdException

    -- ** RepositoryNotAssociatedWithPullRequestException
    , _RepositoryNotAssociatedWithPullRequestException

    -- ** InvalidApprovalRuleTemplateContentException
    , _InvalidApprovalRuleTemplateContentException

    -- ** MaximumConflictResolutionEntriesExceededException
    , _MaximumConflictResolutionEntriesExceededException

    -- ** InvalidAuthorARNException
    , _InvalidAuthorARNException

    -- ** ApprovalRuleTemplateContentRequiredException
    , _ApprovalRuleTemplateContentRequiredException

    -- ** InvalidTargetBranchException
    , _InvalidTargetBranchException

    -- ** InvalidRepositoryTriggerNameException
    , _InvalidRepositoryTriggerNameException

    -- ** InvalidRepositoryTriggerEventsException
    , _InvalidRepositoryTriggerEventsException

    -- ** InvalidPathException
    , _InvalidPathException

    -- ** NumberOfRuleTemplatesExceededException
    , _NumberOfRuleTemplatesExceededException

    -- ** RepositoryNameRequiredException
    , _RepositoryNameRequiredException

    -- ** RepositoryTriggerNameRequiredException
    , _RepositoryTriggerNameRequiredException

    -- ** InvalidFileModeException
    , _InvalidFileModeException

    -- ** FileContentSizeLimitExceededException
    , _FileContentSizeLimitExceededException

    -- ** RevisionNotCurrentException
    , _RevisionNotCurrentException

    -- ** InvalidReplacementTypeException
    , _InvalidReplacementTypeException

    -- ** InvalidActorARNException
    , _InvalidActorARNException

    -- ** PullRequestDoesNotExistException
    , _PullRequestDoesNotExistException

    -- ** ApprovalRuleTemplateNameRequiredException
    , _ApprovalRuleTemplateNameRequiredException

    -- ** CommitIdDoesNotExistException
    , _CommitIdDoesNotExistException

    -- ** CannotDeleteApprovalRuleFromTemplateException
    , _CannotDeleteApprovalRuleFromTemplateException

    -- ** RepositoryNamesRequiredException
    , _RepositoryNamesRequiredException

    -- ** InvalidBlobIdException
    , _InvalidBlobIdException

    -- ** FileTooLargeException
    , _FileTooLargeException

    -- ** InvalidApprovalRuleNameException
    , _InvalidApprovalRuleNameException

    -- ** RevisionIdRequiredException
    , _RevisionIdRequiredException

    -- ** MaximumFileEntriesExceededException
    , _MaximumFileEntriesExceededException

    -- ** ReplacementTypeRequiredException
    , _ReplacementTypeRequiredException

    -- ** MergeOptionRequiredException
    , _MergeOptionRequiredException

    -- ** CommitDoesNotExistException
    , _CommitDoesNotExistException

    -- ** ClientRequestTokenRequiredException
    , _ClientRequestTokenRequiredException

    -- ** InvalidBranchNameException
    , _InvalidBranchNameException

    -- ** InvalidMergeOptionException
    , _InvalidMergeOptionException

    -- ** InvalidFileLocationException
    , _InvalidFileLocationException

    -- ** ApprovalRuleTemplateDoesNotExistException
    , _ApprovalRuleTemplateDoesNotExistException

    -- ** MaximumRepositoryTriggersExceededException
    , _MaximumRepositoryTriggersExceededException

    -- ** BeforeCommitIdAndAfterCommitIdAreSameException
    , _BeforeCommitIdAndAfterCommitIdAreSameException

    -- ** CommitMessageLengthExceededException
    , _CommitMessageLengthExceededException

    -- ** MaximumNumberOfApprovalsExceededException
    , _MaximumNumberOfApprovalsExceededException

    -- ** MaximumRuleTemplatesAssociatedWithRepositoryException
    , _MaximumRuleTemplatesAssociatedWithRepositoryException

    -- ** ReferenceNameRequiredException
    , _ReferenceNameRequiredException

    -- ** InvalidTagKeysListException
    , _InvalidTagKeysListException

    -- ** FileDoesNotExistException
    , _FileDoesNotExistException

    -- ** InvalidReferenceNameException
    , _InvalidReferenceNameException

    -- ** TagKeysListRequiredException
    , _TagKeysListRequiredException

    -- ** CommitIdRequiredException
    , _CommitIdRequiredException

    -- ** ApprovalRuleNameAlreadyExistsException
    , _ApprovalRuleNameAlreadyExistsException

    -- ** ReferenceDoesNotExistException
    , _ReferenceDoesNotExistException

    -- ** TargetRequiredException
    , _TargetRequiredException

    -- ** NameLengthExceededException
    , _NameLengthExceededException

    -- ** FileNameConflictsWithDirectoryNameException
    , _FileNameConflictsWithDirectoryNameException

    -- ** InvalidPullRequestStatusUpdateException
    , _InvalidPullRequestStatusUpdateException

    -- ** CommitRequiredException
    , _CommitRequiredException

    -- ** ActorDoesNotExistException
    , _ActorDoesNotExistException

    -- ** InvalidConflictDetailLevelException
    , _InvalidConflictDetailLevelException

    -- ** PullRequestIdRequiredException
    , _PullRequestIdRequiredException

    -- ** InvalidPullRequestIdException
    , _InvalidPullRequestIdException

    -- ** EncryptionKeyNotFoundException
    , _EncryptionKeyNotFoundException

    -- ** BranchNameExistsException
    , _BranchNameExistsException

    -- ** InvalidSystemTagUsageException
    , _InvalidSystemTagUsageException

    -- ** InvalidTargetsException
    , _InvalidTargetsException

    -- ** FileEntryRequiredException
    , _FileEntryRequiredException

    -- ** TargetsRequiredException
    , _TargetsRequiredException

    -- ** ManualMergeRequiredException
    , _ManualMergeRequiredException

    -- ** CommentContentRequiredException
    , _CommentContentRequiredException

    -- ** ResourceARNRequiredException
    , _ResourceARNRequiredException

    -- ** RestrictedSourceFileException
    , _RestrictedSourceFileException

    -- ** PathDoesNotExistException
    , _PathDoesNotExistException

    -- ** InvalidResourceARNException
    , _InvalidResourceARNException

    -- ** CannotModifyApprovalRuleFromTemplateException
    , _CannotModifyApprovalRuleFromTemplateException

    -- ** ReplacementContentRequiredException
    , _ReplacementContentRequiredException

    -- ** InvalidMaxConflictFilesException
    , _InvalidMaxConflictFilesException

    -- ** InvalidFilePositionException
    , _InvalidFilePositionException

    -- ** ParentCommitIdOutdatedException
    , _ParentCommitIdOutdatedException

    -- ** InvalidMaxMergeHunksException
    , _InvalidMaxMergeHunksException

    -- ** PullRequestApprovalRulesNotSatisfiedException
    , _PullRequestApprovalRulesNotSatisfiedException

    -- ** InvalidApprovalRuleContentException
    , _InvalidApprovalRuleContentException

    -- ** InvalidSourceCommitSpecifierException
    , _InvalidSourceCommitSpecifierException

    -- ** PullRequestStatusRequiredException
    , _PullRequestStatusRequiredException

    -- ** InvalidTitleException
    , _InvalidTitleException

    -- ** InvalidConflictResolutionException
    , _InvalidConflictResolutionException

    -- ** ParentCommitIdRequiredException
    , _ParentCommitIdRequiredException

    -- ** InvalidPullRequestStatusException
    , _InvalidPullRequestStatusException

    -- ** ApprovalRuleContentRequiredException
    , _ApprovalRuleContentRequiredException

    -- ** RepositoryDoesNotExistException
    , _RepositoryDoesNotExistException

    -- ** InvalidOverrideStatusException
    , _InvalidOverrideStatusException

    -- ** TitleRequiredException
    , _TitleRequiredException

    -- ** MaximumItemsToCompareExceededException
    , _MaximumItemsToCompareExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** MergePullRequestByFastForward 
    , module Network.AWS.CodeCommit.MergePullRequestByFastForward

    -- ** UpdateRepositoryName 
    , module Network.AWS.CodeCommit.UpdateRepositoryName

    -- ** PostCommentForPullRequest 
    , module Network.AWS.CodeCommit.PostCommentForPullRequest

    -- ** MergeBranchesBySquash 
    , module Network.AWS.CodeCommit.MergeBranchesBySquash

    -- ** GetCommit 
    , module Network.AWS.CodeCommit.GetCommit

    -- ** BatchAssociateApprovalRuleTemplateWithRepositories 
    , module Network.AWS.CodeCommit.BatchAssociateApprovalRuleTemplateWithRepositories

    -- ** GetApprovalRuleTemplate 
    , module Network.AWS.CodeCommit.GetApprovalRuleTemplate

    -- ** DisassociateApprovalRuleTemplateFromRepository 
    , module Network.AWS.CodeCommit.DisassociateApprovalRuleTemplateFromRepository

    -- ** GetBranch 
    , module Network.AWS.CodeCommit.GetBranch

    -- ** GetDifferences (Paginated)
    , module Network.AWS.CodeCommit.GetDifferences

    -- ** ListTagsForResource 
    , module Network.AWS.CodeCommit.ListTagsForResource

    -- ** GetPullRequest 
    , module Network.AWS.CodeCommit.GetPullRequest

    -- ** OverridePullRequestApprovalRules 
    , module Network.AWS.CodeCommit.OverridePullRequestApprovalRules

    -- ** ListPullRequests (Paginated)
    , module Network.AWS.CodeCommit.ListPullRequests

    -- ** CreateCommit 
    , module Network.AWS.CodeCommit.CreateCommit

    -- ** UpdatePullRequestApprovalState 
    , module Network.AWS.CodeCommit.UpdatePullRequestApprovalState

    -- ** EvaluatePullRequestApprovalRules 
    , module Network.AWS.CodeCommit.EvaluatePullRequestApprovalRules

    -- ** GetComment 
    , module Network.AWS.CodeCommit.GetComment

    -- ** CreateApprovalRuleTemplate 
    , module Network.AWS.CodeCommit.CreateApprovalRuleTemplate

    -- ** DeleteBranch 
    , module Network.AWS.CodeCommit.DeleteBranch

    -- ** UpdateRepositoryDescription 
    , module Network.AWS.CodeCommit.UpdateRepositoryDescription

    -- ** CreateBranch 
    , module Network.AWS.CodeCommit.CreateBranch

    -- ** GetFolder 
    , module Network.AWS.CodeCommit.GetFolder

    -- ** CreatePullRequest 
    , module Network.AWS.CodeCommit.CreatePullRequest

    -- ** DeleteApprovalRuleTemplate 
    , module Network.AWS.CodeCommit.DeleteApprovalRuleTemplate

    -- ** ListBranches (Paginated)
    , module Network.AWS.CodeCommit.ListBranches

    -- ** BatchGetCommits 
    , module Network.AWS.CodeCommit.BatchGetCommits

    -- ** UpdatePullRequestDescription 
    , module Network.AWS.CodeCommit.UpdatePullRequestDescription

    -- ** ListRepositories (Paginated)
    , module Network.AWS.CodeCommit.ListRepositories

    -- ** CreateRepository 
    , module Network.AWS.CodeCommit.CreateRepository

    -- ** UpdateDefaultBranch 
    , module Network.AWS.CodeCommit.UpdateDefaultBranch

    -- ** GetMergeOptions 
    , module Network.AWS.CodeCommit.GetMergeOptions

    -- ** CreatePullRequestApprovalRule 
    , module Network.AWS.CodeCommit.CreatePullRequestApprovalRule

    -- ** PostCommentReply 
    , module Network.AWS.CodeCommit.PostCommentReply

    -- ** UpdateApprovalRuleTemplateContent 
    , module Network.AWS.CodeCommit.UpdateApprovalRuleTemplateContent

    -- ** CreateUnreferencedMergeCommit 
    , module Network.AWS.CodeCommit.CreateUnreferencedMergeCommit

    -- ** ListRepositoriesForApprovalRuleTemplate 
    , module Network.AWS.CodeCommit.ListRepositoriesForApprovalRuleTemplate

    -- ** GetRepository 
    , module Network.AWS.CodeCommit.GetRepository

    -- ** BatchDescribeMergeConflicts 
    , module Network.AWS.CodeCommit.BatchDescribeMergeConflicts

    -- ** DeletePullRequestApprovalRule 
    , module Network.AWS.CodeCommit.DeletePullRequestApprovalRule

    -- ** GetRepositoryTriggers 
    , module Network.AWS.CodeCommit.GetRepositoryTriggers

    -- ** UpdateApprovalRuleTemplateName 
    , module Network.AWS.CodeCommit.UpdateApprovalRuleTemplateName

    -- ** PutFile 
    , module Network.AWS.CodeCommit.PutFile

    -- ** DeleteFile 
    , module Network.AWS.CodeCommit.DeleteFile

    -- ** GetCommentsForComparedCommit (Paginated)
    , module Network.AWS.CodeCommit.GetCommentsForComparedCommit

    -- ** GetMergeCommit 
    , module Network.AWS.CodeCommit.GetMergeCommit

    -- ** TestRepositoryTriggers 
    , module Network.AWS.CodeCommit.TestRepositoryTriggers

    -- ** MergePullRequestBySquash 
    , module Network.AWS.CodeCommit.MergePullRequestBySquash

    -- ** UpdateComment 
    , module Network.AWS.CodeCommit.UpdateComment

    -- ** PostCommentForComparedCommit 
    , module Network.AWS.CodeCommit.PostCommentForComparedCommit

    -- ** MergeBranchesByFastForward 
    , module Network.AWS.CodeCommit.MergeBranchesByFastForward

    -- ** UpdatePullRequestTitle 
    , module Network.AWS.CodeCommit.UpdatePullRequestTitle

    -- ** BatchDisassociateApprovalRuleTemplateFromRepositories 
    , module Network.AWS.CodeCommit.BatchDisassociateApprovalRuleTemplateFromRepositories

    -- ** UpdatePullRequestApprovalRuleContent 
    , module Network.AWS.CodeCommit.UpdatePullRequestApprovalRuleContent

    -- ** GetBlob 
    , module Network.AWS.CodeCommit.GetBlob

    -- ** AssociateApprovalRuleTemplateWithRepository 
    , module Network.AWS.CodeCommit.AssociateApprovalRuleTemplateWithRepository

    -- ** PutRepositoryTriggers 
    , module Network.AWS.CodeCommit.PutRepositoryTriggers

    -- ** ListApprovalRuleTemplates 
    , module Network.AWS.CodeCommit.ListApprovalRuleTemplates

    -- ** DescribeMergeConflicts 
    , module Network.AWS.CodeCommit.DescribeMergeConflicts

    -- ** TagResource 
    , module Network.AWS.CodeCommit.TagResource

    -- ** MergeBranchesByThreeWay 
    , module Network.AWS.CodeCommit.MergeBranchesByThreeWay

    -- ** GetFile 
    , module Network.AWS.CodeCommit.GetFile

    -- ** UntagResource 
    , module Network.AWS.CodeCommit.UntagResource

    -- ** GetMergeConflicts 
    , module Network.AWS.CodeCommit.GetMergeConflicts

    -- ** DeleteRepository 
    , module Network.AWS.CodeCommit.DeleteRepository

    -- ** DeleteCommentContent 
    , module Network.AWS.CodeCommit.DeleteCommentContent

    -- ** MergePullRequestByThreeWay 
    , module Network.AWS.CodeCommit.MergePullRequestByThreeWay

    -- ** DescribePullRequestEvents (Paginated)
    , module Network.AWS.CodeCommit.DescribePullRequestEvents

    -- ** BatchGetRepositories 
    , module Network.AWS.CodeCommit.BatchGetRepositories

    -- ** UpdateApprovalRuleTemplateDescription 
    , module Network.AWS.CodeCommit.UpdateApprovalRuleTemplateDescription

    -- ** GetPullRequestOverrideState 
    , module Network.AWS.CodeCommit.GetPullRequestOverrideState

    -- ** GetPullRequestApprovalStates 
    , module Network.AWS.CodeCommit.GetPullRequestApprovalStates

    -- ** GetCommentsForPullRequest (Paginated)
    , module Network.AWS.CodeCommit.GetCommentsForPullRequest

    -- ** UpdatePullRequestStatus 
    , module Network.AWS.CodeCommit.UpdatePullRequestStatus

    -- ** ListAssociatedApprovalRuleTemplatesForRepository 
    , module Network.AWS.CodeCommit.ListAssociatedApprovalRuleTemplatesForRepository

    -- * Types

    -- ** ApprovalState
    , ApprovalState (..)

    -- ** ChangeTypeEnum
    , ChangeTypeEnum (..)

    -- ** ConflictDetailLevelTypeEnum
    , ConflictDetailLevelTypeEnum (..)

    -- ** ConflictResolutionStrategyTypeEnum
    , ConflictResolutionStrategyTypeEnum (..)

    -- ** FileModeTypeEnum
    , FileModeTypeEnum (..)

    -- ** MergeOptionTypeEnum
    , MergeOptionTypeEnum (..)

    -- ** ObjectTypeEnum
    , ObjectTypeEnum (..)

    -- ** OrderEnum
    , OrderEnum (..)

    -- ** OverrideStatus
    , OverrideStatus (..)

    -- ** PullRequestEventType
    , PullRequestEventType (..)

    -- ** PullRequestStatusEnum
    , PullRequestStatusEnum (..)

    -- ** RelativeFileVersionEnum
    , RelativeFileVersionEnum (..)

    -- ** ReplacementTypeEnum
    , ReplacementTypeEnum (..)

    -- ** RepositoryTriggerEventEnum
    , RepositoryTriggerEventEnum (..)

    -- ** SortByEnum
    , SortByEnum (..)

    -- ** Approval
    , Approval
    , approval
    , aApprovalState
    , aUserARN

    -- ** ApprovalRule
    , ApprovalRule
    , approvalRule
    , arRuleContentSha256
    , arLastModifiedDate
    , arApprovalRuleName
    , arApprovalRuleId
    , arLastModifiedUser
    , arOriginApprovalRuleTemplate
    , arCreationDate
    , arApprovalRuleContent

    -- ** ApprovalRuleEventMetadata
    , ApprovalRuleEventMetadata
    , approvalRuleEventMetadata
    , aremApprovalRuleName
    , aremApprovalRuleId
    , aremApprovalRuleContent

    -- ** ApprovalRuleOverriddenEventMetadata
    , ApprovalRuleOverriddenEventMetadata
    , approvalRuleOverriddenEventMetadata
    , aroemOverrideStatus
    , aroemRevisionId

    -- ** ApprovalRuleTemplate
    , ApprovalRuleTemplate
    , approvalRuleTemplate
    , artRuleContentSha256
    , artApprovalRuleTemplateId
    , artLastModifiedDate
    , artApprovalRuleTemplateDescription
    , artApprovalRuleTemplateContent
    , artLastModifiedUser
    , artCreationDate
    , artApprovalRuleTemplateName

    -- ** ApprovalStateChangedEventMetadata
    , ApprovalStateChangedEventMetadata
    , approvalStateChangedEventMetadata
    , ascemApprovalStatus
    , ascemRevisionId

    -- ** BatchAssociateApprovalRuleTemplateWithRepositoriesError
    , BatchAssociateApprovalRuleTemplateWithRepositoriesError
    , batchAssociateApprovalRuleTemplateWithRepositoriesError
    , baartwreErrorCode
    , baartwreRepositoryName
    , baartwreErrorMessage

    -- ** BatchDescribeMergeConflictsError
    , BatchDescribeMergeConflictsError
    , batchDescribeMergeConflictsError
    , bdmceFilePath
    , bdmceExceptionName
    , bdmceMessage

    -- ** BatchDisassociateApprovalRuleTemplateFromRepositoriesError
    , BatchDisassociateApprovalRuleTemplateFromRepositoriesError
    , batchDisassociateApprovalRuleTemplateFromRepositoriesError
    , bdartfreErrorCode
    , bdartfreRepositoryName
    , bdartfreErrorMessage

    -- ** BatchGetCommitsError
    , BatchGetCommitsError
    , batchGetCommitsError
    , bgceCommitId
    , bgceErrorCode
    , bgceErrorMessage

    -- ** BlobMetadata
    , BlobMetadata
    , blobMetadata
    , bmPath
    , bmMode
    , bmBlobId

    -- ** BranchInfo
    , BranchInfo
    , branchInfo
    , biCommitId
    , biBranchName

    -- ** Comment
    , Comment
    , comment
    , cLastModifiedDate
    , cAuthorARN
    , cContent
    , cCreationDate
    , cDeleted
    , cClientRequestToken
    , cCommentId
    , cInReplyTo

    -- ** CommentsForComparedCommit
    , CommentsForComparedCommit
    , commentsForComparedCommit
    , cfccBeforeBlobId
    , cfccLocation
    , cfccAfterCommitId
    , cfccAfterBlobId
    , cfccBeforeCommitId
    , cfccRepositoryName
    , cfccComments

    -- ** CommentsForPullRequest
    , CommentsForPullRequest
    , commentsForPullRequest
    , cfprBeforeBlobId
    , cfprLocation
    , cfprAfterCommitId
    , cfprPullRequestId
    , cfprAfterBlobId
    , cfprBeforeCommitId
    , cfprRepositoryName
    , cfprComments

    -- ** Commit
    , Commit
    , commit
    , cCommitId
    , cCommitter
    , cTreeId
    , cAdditionalData
    , cParents
    , cAuthor
    , cMessage

    -- ** Conflict
    , Conflict
    , conflict
    , cMergeHunks
    , cConflictMetadata

    -- ** ConflictMetadata
    , ConflictMetadata
    , conflictMetadata
    , cmNumberOfConflicts
    , cmContentConflict
    , cmFileSizes
    , cmFilePath
    , cmIsBinaryFile
    , cmFileModeConflict
    , cmObjectTypeConflict
    , cmMergeOperations
    , cmObjectTypes
    , cmFileModes

    -- ** ConflictResolution
    , ConflictResolution
    , conflictResolution
    , crSetFileModes
    , crDeleteFiles
    , crReplaceContents

    -- ** DeleteFileEntry
    , DeleteFileEntry
    , deleteFileEntry
    , dfeFilePath

    -- ** Difference
    , Difference
    , difference
    , dAfterBlob
    , dBeforeBlob
    , dChangeType

    -- ** Evaluation
    , Evaluation
    , evaluation
    , eApprovalRulesSatisfied
    , eApprovalRulesNotSatisfied
    , eApproved
    , eOverridden

    -- ** File
    , File
    , file
    , fAbsolutePath
    , fFileMode
    , fBlobId
    , fRelativePath

    -- ** FileMetadata
    , FileMetadata
    , fileMetadata
    , fmAbsolutePath
    , fmFileMode
    , fmBlobId

    -- ** FileModes
    , FileModes
    , fileModes
    , fmDestination
    , fmBase
    , fmSource

    -- ** FileSizes
    , FileSizes
    , fileSizes
    , fsDestination
    , fsBase
    , fsSource

    -- ** Folder
    , Folder
    , folder
    , folAbsolutePath
    , folTreeId
    , folRelativePath

    -- ** IsBinaryFile
    , IsBinaryFile
    , isBinaryFile
    , ibfDestination
    , ibfBase
    , ibfSource

    -- ** Location
    , Location
    , location
    , lRelativeFileVersion
    , lFilePath
    , lFilePosition

    -- ** MergeHunk
    , MergeHunk
    , mergeHunk
    , mhDestination
    , mhBase
    , mhIsConflict
    , mhSource

    -- ** MergeHunkDetail
    , MergeHunkDetail
    , mergeHunkDetail
    , mhdStartLine
    , mhdEndLine
    , mhdHunkContent

    -- ** MergeMetadata
    , MergeMetadata
    , mergeMetadata
    , mmMergedBy
    , mmMergeOption
    , mmIsMerged
    , mmMergeCommitId

    -- ** MergeOperations
    , MergeOperations
    , mergeOperations
    , moDestination
    , moSource

    -- ** ObjectTypes
    , ObjectTypes
    , objectTypes
    , otDestination
    , otBase
    , otSource

    -- ** OriginApprovalRuleTemplate
    , OriginApprovalRuleTemplate
    , originApprovalRuleTemplate
    , oartApprovalRuleTemplateId
    , oartApprovalRuleTemplateName

    -- ** PullRequest
    , PullRequest
    , pullRequest
    , prApprovalRules
    , prAuthorARN
    , prPullRequestId
    , prCreationDate
    , prPullRequestStatus
    , prTitle
    , prClientRequestToken
    , prLastActivityDate
    , prRevisionId
    , prPullRequestTargets
    , prDescription

    -- ** PullRequestCreatedEventMetadata
    , PullRequestCreatedEventMetadata
    , pullRequestCreatedEventMetadata
    , prcemDestinationCommitId
    , prcemMergeBase
    , prcemRepositoryName
    , prcemSourceCommitId

    -- ** PullRequestEvent
    , PullRequestEvent
    , pullRequestEvent
    , prePullRequestMergedStateChangedEventMetadata
    , prePullRequestCreatedEventMetadata
    , preApprovalRuleEventMetadata
    , prePullRequestEventType
    , prePullRequestStatusChangedEventMetadata
    , preActorARN
    , prePullRequestId
    , preEventDate
    , preApprovalStateChangedEventMetadata
    , prePullRequestSourceReferenceUpdatedEventMetadata
    , preApprovalRuleOverriddenEventMetadata

    -- ** PullRequestMergedStateChangedEventMetadata
    , PullRequestMergedStateChangedEventMetadata
    , pullRequestMergedStateChangedEventMetadata
    , prmscemDestinationReference
    , prmscemMergeMetadata
    , prmscemRepositoryName

    -- ** PullRequestSourceReferenceUpdatedEventMetadata
    , PullRequestSourceReferenceUpdatedEventMetadata
    , pullRequestSourceReferenceUpdatedEventMetadata
    , prsruemAfterCommitId
    , prsruemBeforeCommitId
    , prsruemMergeBase
    , prsruemRepositoryName

    -- ** PullRequestStatusChangedEventMetadata
    , PullRequestStatusChangedEventMetadata
    , pullRequestStatusChangedEventMetadata
    , prscemPullRequestStatus

    -- ** PullRequestTarget
    , PullRequestTarget
    , pullRequestTarget
    , prtSourceCommit
    , prtDestinationReference
    , prtMergeMetadata
    , prtMergeBase
    , prtDestinationCommit
    , prtRepositoryName
    , prtSourceReference

    -- ** PutFileEntry
    , PutFileEntry
    , putFileEntry
    , pfeFileContent
    , pfeFileMode
    , pfeSourceFile
    , pfeFilePath

    -- ** ReplaceContentEntry
    , ReplaceContentEntry
    , replaceContentEntry
    , rceFileMode
    , rceContent
    , rceFilePath
    , rceReplacementType

    -- ** RepositoryMetadata
    , RepositoryMetadata
    , repositoryMetadata
    , rmRepositoryDescription
    , rmLastModifiedDate
    , rmARN
    , rmCloneURLHTTP
    , rmAccountId
    , rmDefaultBranch
    , rmRepositoryId
    , rmRepositoryName
    , rmCreationDate
    , rmCloneURLSSH

    -- ** RepositoryNameIdPair
    , RepositoryNameIdPair
    , repositoryNameIdPair
    , rnipRepositoryId
    , rnipRepositoryName

    -- ** RepositoryTrigger
    , RepositoryTrigger
    , repositoryTrigger
    , rtBranches
    , rtCustomData
    , rtName
    , rtDestinationARN
    , rtEvents

    -- ** RepositoryTriggerExecutionFailure
    , RepositoryTriggerExecutionFailure
    , repositoryTriggerExecutionFailure
    , rtefFailureMessage
    , rtefTrigger

    -- ** SetFileModeEntry
    , SetFileModeEntry
    , setFileModeEntry
    , sfmeFilePath
    , sfmeFileMode

    -- ** SourceFileSpecifier
    , SourceFileSpecifier
    , sourceFileSpecifier
    , sfsIsMove
    , sfsFilePath

    -- ** SubModule
    , SubModule
    , subModule
    , smCommitId
    , smAbsolutePath
    , smRelativePath

    -- ** SymbolicLink
    , SymbolicLink
    , symbolicLink
    , slAbsolutePath
    , slFileMode
    , slBlobId
    , slRelativePath

    -- ** Target
    , Target
    , target
    , tDestinationReference
    , tRepositoryName
    , tSourceReference

    -- ** UserInfo
    , UserInfo
    , userInfo
    , uiEmail
    , uiDate
    , uiName
    ) where

import Network.AWS.CodeCommit.AssociateApprovalRuleTemplateWithRepository
import Network.AWS.CodeCommit.BatchAssociateApprovalRuleTemplateWithRepositories
import Network.AWS.CodeCommit.BatchDescribeMergeConflicts
import Network.AWS.CodeCommit.BatchDisassociateApprovalRuleTemplateFromRepositories
import Network.AWS.CodeCommit.BatchGetCommits
import Network.AWS.CodeCommit.BatchGetRepositories
import Network.AWS.CodeCommit.CreateApprovalRuleTemplate
import Network.AWS.CodeCommit.CreateBranch
import Network.AWS.CodeCommit.CreateCommit
import Network.AWS.CodeCommit.CreatePullRequest
import Network.AWS.CodeCommit.CreatePullRequestApprovalRule
import Network.AWS.CodeCommit.CreateRepository
import Network.AWS.CodeCommit.CreateUnreferencedMergeCommit
import Network.AWS.CodeCommit.DeleteApprovalRuleTemplate
import Network.AWS.CodeCommit.DeleteBranch
import Network.AWS.CodeCommit.DeleteCommentContent
import Network.AWS.CodeCommit.DeleteFile
import Network.AWS.CodeCommit.DeletePullRequestApprovalRule
import Network.AWS.CodeCommit.DeleteRepository
import Network.AWS.CodeCommit.DescribeMergeConflicts
import Network.AWS.CodeCommit.DescribePullRequestEvents
import Network.AWS.CodeCommit.DisassociateApprovalRuleTemplateFromRepository
import Network.AWS.CodeCommit.EvaluatePullRequestApprovalRules
import Network.AWS.CodeCommit.GetApprovalRuleTemplate
import Network.AWS.CodeCommit.GetBlob
import Network.AWS.CodeCommit.GetBranch
import Network.AWS.CodeCommit.GetComment
import Network.AWS.CodeCommit.GetCommentsForComparedCommit
import Network.AWS.CodeCommit.GetCommentsForPullRequest
import Network.AWS.CodeCommit.GetCommit
import Network.AWS.CodeCommit.GetDifferences
import Network.AWS.CodeCommit.GetFile
import Network.AWS.CodeCommit.GetFolder
import Network.AWS.CodeCommit.GetMergeCommit
import Network.AWS.CodeCommit.GetMergeConflicts
import Network.AWS.CodeCommit.GetMergeOptions
import Network.AWS.CodeCommit.GetPullRequest
import Network.AWS.CodeCommit.GetPullRequestApprovalStates
import Network.AWS.CodeCommit.GetPullRequestOverrideState
import Network.AWS.CodeCommit.GetRepository
import Network.AWS.CodeCommit.GetRepositoryTriggers
import Network.AWS.CodeCommit.ListApprovalRuleTemplates
import Network.AWS.CodeCommit.ListAssociatedApprovalRuleTemplatesForRepository
import Network.AWS.CodeCommit.ListBranches
import Network.AWS.CodeCommit.ListPullRequests
import Network.AWS.CodeCommit.ListRepositories
import Network.AWS.CodeCommit.ListRepositoriesForApprovalRuleTemplate
import Network.AWS.CodeCommit.ListTagsForResource
import Network.AWS.CodeCommit.MergeBranchesByFastForward
import Network.AWS.CodeCommit.MergeBranchesBySquash
import Network.AWS.CodeCommit.MergeBranchesByThreeWay
import Network.AWS.CodeCommit.MergePullRequestByFastForward
import Network.AWS.CodeCommit.MergePullRequestBySquash
import Network.AWS.CodeCommit.MergePullRequestByThreeWay
import Network.AWS.CodeCommit.OverridePullRequestApprovalRules
import Network.AWS.CodeCommit.PostCommentForComparedCommit
import Network.AWS.CodeCommit.PostCommentForPullRequest
import Network.AWS.CodeCommit.PostCommentReply
import Network.AWS.CodeCommit.PutFile
import Network.AWS.CodeCommit.PutRepositoryTriggers
import Network.AWS.CodeCommit.TagResource
import Network.AWS.CodeCommit.TestRepositoryTriggers
import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.UntagResource
import Network.AWS.CodeCommit.UpdateApprovalRuleTemplateContent
import Network.AWS.CodeCommit.UpdateApprovalRuleTemplateDescription
import Network.AWS.CodeCommit.UpdateApprovalRuleTemplateName
import Network.AWS.CodeCommit.UpdateComment
import Network.AWS.CodeCommit.UpdateDefaultBranch
import Network.AWS.CodeCommit.UpdatePullRequestApprovalRuleContent
import Network.AWS.CodeCommit.UpdatePullRequestApprovalState
import Network.AWS.CodeCommit.UpdatePullRequestDescription
import Network.AWS.CodeCommit.UpdatePullRequestStatus
import Network.AWS.CodeCommit.UpdatePullRequestTitle
import Network.AWS.CodeCommit.UpdateRepositoryDescription
import Network.AWS.CodeCommit.UpdateRepositoryName
import Network.AWS.CodeCommit.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'CodeCommit'.
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

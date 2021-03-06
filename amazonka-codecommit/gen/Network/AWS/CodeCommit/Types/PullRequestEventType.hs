{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.PullRequestEventType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.PullRequestEventType (
  PullRequestEventType (
    ..
    , PullRequestApprovalRuleCreated
    , PullRequestApprovalRuleDeleted
    , PullRequestApprovalRuleOverridden
    , PullRequestApprovalRuleUpdated
    , PullRequestApprovalStateChanged
    , PullRequestCreated
    , PullRequestMergeStateChanged
    , PullRequestSourceReferenceUpdated
    , PullRequestStatusChanged
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data PullRequestEventType = PullRequestEventType' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern PullRequestApprovalRuleCreated :: PullRequestEventType
pattern PullRequestApprovalRuleCreated = PullRequestEventType' "PULL_REQUEST_APPROVAL_RULE_CREATED"

pattern PullRequestApprovalRuleDeleted :: PullRequestEventType
pattern PullRequestApprovalRuleDeleted = PullRequestEventType' "PULL_REQUEST_APPROVAL_RULE_DELETED"

pattern PullRequestApprovalRuleOverridden :: PullRequestEventType
pattern PullRequestApprovalRuleOverridden = PullRequestEventType' "PULL_REQUEST_APPROVAL_RULE_OVERRIDDEN"

pattern PullRequestApprovalRuleUpdated :: PullRequestEventType
pattern PullRequestApprovalRuleUpdated = PullRequestEventType' "PULL_REQUEST_APPROVAL_RULE_UPDATED"

pattern PullRequestApprovalStateChanged :: PullRequestEventType
pattern PullRequestApprovalStateChanged = PullRequestEventType' "PULL_REQUEST_APPROVAL_STATE_CHANGED"

pattern PullRequestCreated :: PullRequestEventType
pattern PullRequestCreated = PullRequestEventType' "PULL_REQUEST_CREATED"

pattern PullRequestMergeStateChanged :: PullRequestEventType
pattern PullRequestMergeStateChanged = PullRequestEventType' "PULL_REQUEST_MERGE_STATE_CHANGED"

pattern PullRequestSourceReferenceUpdated :: PullRequestEventType
pattern PullRequestSourceReferenceUpdated = PullRequestEventType' "PULL_REQUEST_SOURCE_REFERENCE_UPDATED"

pattern PullRequestStatusChanged :: PullRequestEventType
pattern PullRequestStatusChanged = PullRequestEventType' "PULL_REQUEST_STATUS_CHANGED"

{-# COMPLETE
  PullRequestApprovalRuleCreated,
  PullRequestApprovalRuleDeleted,
  PullRequestApprovalRuleOverridden,
  PullRequestApprovalRuleUpdated,
  PullRequestApprovalStateChanged,
  PullRequestCreated,
  PullRequestMergeStateChanged,
  PullRequestSourceReferenceUpdated,
  PullRequestStatusChanged,
  PullRequestEventType' #-}

instance FromText PullRequestEventType where
    parser = (PullRequestEventType' . mk) <$> takeText

instance ToText PullRequestEventType where
    toText (PullRequestEventType' ci) = original ci

-- | Represents an enum of /known/ $PullRequestEventType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum PullRequestEventType where
    toEnum i = case i of
        0 -> PullRequestApprovalRuleCreated
        1 -> PullRequestApprovalRuleDeleted
        2 -> PullRequestApprovalRuleOverridden
        3 -> PullRequestApprovalRuleUpdated
        4 -> PullRequestApprovalStateChanged
        5 -> PullRequestCreated
        6 -> PullRequestMergeStateChanged
        7 -> PullRequestSourceReferenceUpdated
        8 -> PullRequestStatusChanged
        _ -> (error . showText) $ "Unknown index for PullRequestEventType: " <> toText i
    fromEnum x = case x of
        PullRequestApprovalRuleCreated -> 0
        PullRequestApprovalRuleDeleted -> 1
        PullRequestApprovalRuleOverridden -> 2
        PullRequestApprovalRuleUpdated -> 3
        PullRequestApprovalStateChanged -> 4
        PullRequestCreated -> 5
        PullRequestMergeStateChanged -> 6
        PullRequestSourceReferenceUpdated -> 7
        PullRequestStatusChanged -> 8
        PullRequestEventType' name -> (error . showText) $ "Unknown PullRequestEventType: " <> original name

-- | Represents the bounds of /known/ $PullRequestEventType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded PullRequestEventType where
    minBound = PullRequestApprovalRuleCreated
    maxBound = PullRequestStatusChanged

instance Hashable     PullRequestEventType
instance NFData       PullRequestEventType
instance ToByteString PullRequestEventType
instance ToQuery      PullRequestEventType
instance ToHeader     PullRequestEventType

instance ToJSON PullRequestEventType where
    toJSON = toJSONText

instance FromJSON PullRequestEventType where
    parseJSON = parseJSONText "PullRequestEventType"

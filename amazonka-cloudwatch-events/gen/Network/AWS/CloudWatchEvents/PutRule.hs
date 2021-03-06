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
-- Module      : Network.AWS.CloudWatchEvents.PutRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the specified rule. Rules are enabled by default, or based on value of the state. You can disable a rule using 'DisableRule' .
--
--
-- A single rule watches for events from a single event bus. Events generated by AWS services go to your account's default event bus. Events generated by SaaS partner services or applications go to the matching partner event bus. If you have custom applications or services, you can specify whether their events go to your default event bus or a custom event bus that you have created. For more information, see 'CreateEventBus' .
--
-- If you are updating an existing rule, the rule is replaced with what you specify in this @PutRule@ command. If you omit arguments in @PutRule@ , the old values for those arguments are not kept. Instead, they are replaced with null values.
--
-- When you create or update a rule, incoming events might not immediately start matching to new or updated rules. Allow a short period of time for changes to take effect.
--
-- A rule must contain at least an EventPattern or ScheduleExpression. Rules with EventPatterns are triggered when a matching event is observed. Rules with ScheduleExpressions self-trigger based on the given schedule. A rule can have both an EventPattern and a ScheduleExpression, in which case the rule triggers on matching events as well as on a schedule.
--
-- When you initially create a rule, you can optionally assign one or more tags to the rule. Tags can help you organize and categorize your resources. You can also use them to scope user permissions, by granting a user permission to access or change only rules with certain tag values. To use the @PutRule@ operation and assign tags, you must have both the @events:PutRule@ and @events:TagResource@ permissions.
--
-- If you are updating an existing rule, any tags you specify in the @PutRule@ operation are ignored. To update the tags of an existing rule, use 'TagResource' and 'UntagResource' .
--
-- Most services in AWS treat : or / as the same character in Amazon Resource Names (ARNs). However, EventBridge uses an exact match in event patterns and rules. Be sure to use the correct ARN characters when creating event patterns so that they match the ARN syntax in the event you want to match.
--
-- In EventBridge, it is possible to create rules that lead to infinite loops, where a rule is fired repeatedly. For example, a rule might detect that ACLs have changed on an S3 bucket, and trigger software to change them to the desired state. If the rule is not written carefully, the subsequent change to the ACLs fires the rule again, creating an infinite loop.
--
-- To prevent this, write the rules so that the triggered actions do not re-fire the same rule. For example, your rule could fire only if ACLs are found to be in a bad state, instead of after any change. 
--
-- An infinite loop can quickly cause higher than expected charges. We recommend that you use budgeting, which alerts you when charges exceed your specified limit. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/budgets-managing-costs.html Managing Your Costs with Budgets> .
--
module Network.AWS.CloudWatchEvents.PutRule
    (
    -- * Creating a Request
      putRule
    , PutRule
    -- * Request Lenses
    , prEventPattern
    , prState
    , prEventBusName
    , prScheduleExpression
    , prDescription
    , prTags
    , prRoleARN
    , prName

    -- * Destructuring the Response
    , putRuleResponse
    , PutRuleResponse
    -- * Response Lenses
    , prrsRuleARN
    , prrsResponseStatus
    ) where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.CloudWatchEvents.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putRule' smart constructor.
data PutRule = PutRule'{_prEventPattern ::
                        !(Maybe Text),
                        _prState :: !(Maybe RuleState),
                        _prEventBusName :: !(Maybe Text),
                        _prScheduleExpression :: !(Maybe Text),
                        _prDescription :: !(Maybe Text),
                        _prTags :: !(Maybe [Tag]),
                        _prRoleARN :: !(Maybe Text), _prName :: !Text}
                 deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prEventPattern' - The event pattern. For more information, see <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns> in the /Amazon EventBridge User Guide/ .
--
-- * 'prState' - Indicates whether the rule is enabled or disabled.
--
-- * 'prEventBusName' - The event bus to associate with this rule. If you omit this, the default event bus is used.
--
-- * 'prScheduleExpression' - The scheduling expression. For example, "cron(0 20 * * ? *)" or "rate(5 minutes)".
--
-- * 'prDescription' - A description of the rule.
--
-- * 'prTags' - The list of key-value pairs to associate with the rule.
--
-- * 'prRoleARN' - The Amazon Resource Name (ARN) of the IAM role associated with the rule.
--
-- * 'prName' - The name of the rule that you are creating or updating.
putRule
    :: Text -- ^ 'prName'
    -> PutRule
putRule pName_
  = PutRule'{_prEventPattern = Nothing,
             _prState = Nothing, _prEventBusName = Nothing,
             _prScheduleExpression = Nothing,
             _prDescription = Nothing, _prTags = Nothing,
             _prRoleARN = Nothing, _prName = pName_}

-- | The event pattern. For more information, see <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns> in the /Amazon EventBridge User Guide/ .
prEventPattern :: Lens' PutRule (Maybe Text)
prEventPattern = lens _prEventPattern (\ s a -> s{_prEventPattern = a})

-- | Indicates whether the rule is enabled or disabled.
prState :: Lens' PutRule (Maybe RuleState)
prState = lens _prState (\ s a -> s{_prState = a})

-- | The event bus to associate with this rule. If you omit this, the default event bus is used.
prEventBusName :: Lens' PutRule (Maybe Text)
prEventBusName = lens _prEventBusName (\ s a -> s{_prEventBusName = a})

-- | The scheduling expression. For example, "cron(0 20 * * ? *)" or "rate(5 minutes)".
prScheduleExpression :: Lens' PutRule (Maybe Text)
prScheduleExpression = lens _prScheduleExpression (\ s a -> s{_prScheduleExpression = a})

-- | A description of the rule.
prDescription :: Lens' PutRule (Maybe Text)
prDescription = lens _prDescription (\ s a -> s{_prDescription = a})

-- | The list of key-value pairs to associate with the rule.
prTags :: Lens' PutRule [Tag]
prTags = lens _prTags (\ s a -> s{_prTags = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the IAM role associated with the rule.
prRoleARN :: Lens' PutRule (Maybe Text)
prRoleARN = lens _prRoleARN (\ s a -> s{_prRoleARN = a})

-- | The name of the rule that you are creating or updating.
prName :: Lens' PutRule Text
prName = lens _prName (\ s a -> s{_prName = a})

instance AWSRequest PutRule where
        type Rs PutRule = PutRuleResponse
        request = postJSON cloudWatchEvents
        response
          = receiveJSON
              (\ s h x ->
                 PutRuleResponse' <$>
                   (x .?> "RuleArn") <*> (pure (fromEnum s)))

instance Hashable PutRule where

instance NFData PutRule where

instance ToHeaders PutRule where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSEvents.PutRule" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutRule where
        toJSON PutRule'{..}
          = object
              (catMaybes
                 [("EventPattern" .=) <$> _prEventPattern,
                  ("State" .=) <$> _prState,
                  ("EventBusName" .=) <$> _prEventBusName,
                  ("ScheduleExpression" .=) <$> _prScheduleExpression,
                  ("Description" .=) <$> _prDescription,
                  ("Tags" .=) <$> _prTags,
                  ("RoleArn" .=) <$> _prRoleARN,
                  Just ("Name" .= _prName)])

instance ToPath PutRule where
        toPath = const "/"

instance ToQuery PutRule where
        toQuery = const mempty

-- | /See:/ 'putRuleResponse' smart constructor.
data PutRuleResponse = PutRuleResponse'{_prrsRuleARN
                                        :: !(Maybe Text),
                                        _prrsResponseStatus :: !Int}
                         deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prrsRuleARN' - The Amazon Resource Name (ARN) of the rule.
--
-- * 'prrsResponseStatus' - -- | The response status code.
putRuleResponse
    :: Int -- ^ 'prrsResponseStatus'
    -> PutRuleResponse
putRuleResponse pResponseStatus_
  = PutRuleResponse'{_prrsRuleARN = Nothing,
                     _prrsResponseStatus = pResponseStatus_}

-- | The Amazon Resource Name (ARN) of the rule.
prrsRuleARN :: Lens' PutRuleResponse (Maybe Text)
prrsRuleARN = lens _prrsRuleARN (\ s a -> s{_prrsRuleARN = a})

-- | -- | The response status code.
prrsResponseStatus :: Lens' PutRuleResponse Int
prrsResponseStatus = lens _prrsResponseStatus (\ s a -> s{_prrsResponseStatus = a})

instance NFData PutRuleResponse where

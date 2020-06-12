{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.StackInstance where

import Network.AWS.CloudFormation.Types.Parameter
import Network.AWS.CloudFormation.Types.StackInstanceStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An AWS CloudFormation stack, in a specific account and region, that's part of a stack set operation. A stack instance is a reference to an attempted or actual stack in a given account within a given region. A stack instance can exist without a stack—for example, if the stack couldn't be created for some reason. A stack instance is associated with only one stack set. Each stack instance contains the ID of its associated stack set, as well as the ID of the actual stack and the stack status.
--
--
--
-- /See:/ 'stackInstance' smart constructor.
data StackInstance = StackInstance'{_siStatus ::
                                    !(Maybe StackInstanceStatus),
                                    _siAccount :: !(Maybe Text),
                                    _siRegion :: !(Maybe Text),
                                    _siStatusReason :: !(Maybe Text),
                                    _siStackId :: !(Maybe Text),
                                    _siParameterOverrides ::
                                    !(Maybe [Parameter]),
                                    _siStackSetId :: !(Maybe Text)}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StackInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siStatus' - The status of the stack instance, in terms of its synchronization with its associated stack set.     * @INOPERABLE@ : A @DeleteStackInstances@ operation has failed and left the stack in an unstable state. Stacks in this state are excluded from further @UpdateStackSet@ operations. You might need to perform a @DeleteStackInstances@ operation, with @RetainStacks@ set to @true@ , to delete the stack instance, and then delete the stack manually.     * @OUTDATED@ : The stack isn't currently up to date with the stack set because:     * The associated stack failed during a @CreateStackSet@ or @UpdateStackSet@ operation.      * The stack was part of a @CreateStackSet@ or @UpdateStackSet@ operation that failed or was stopped before the stack was created or updated.      * @CURRENT@ : The stack is currently up to date with the stack set.
--
-- * 'siAccount' - The name of the AWS account that the stack instance is associated with.
--
-- * 'siRegion' - The name of the AWS region that the stack instance is associated with.
--
-- * 'siStatusReason' - The explanation for the specific status code that is assigned to this stack instance.
--
-- * 'siStackId' - The ID of the stack instance.
--
-- * 'siParameterOverrides' - A list of parameters from the stack set template whose values have been overridden in this stack instance.
--
-- * 'siStackSetId' - The name or unique ID of the stack set that the stack instance is associated with.
stackInstance
    :: StackInstance
stackInstance
  = StackInstance'{_siStatus = Nothing,
                   _siAccount = Nothing, _siRegion = Nothing,
                   _siStatusReason = Nothing, _siStackId = Nothing,
                   _siParameterOverrides = Nothing,
                   _siStackSetId = Nothing}

-- | The status of the stack instance, in terms of its synchronization with its associated stack set.     * @INOPERABLE@ : A @DeleteStackInstances@ operation has failed and left the stack in an unstable state. Stacks in this state are excluded from further @UpdateStackSet@ operations. You might need to perform a @DeleteStackInstances@ operation, with @RetainStacks@ set to @true@ , to delete the stack instance, and then delete the stack manually.     * @OUTDATED@ : The stack isn't currently up to date with the stack set because:     * The associated stack failed during a @CreateStackSet@ or @UpdateStackSet@ operation.      * The stack was part of a @CreateStackSet@ or @UpdateStackSet@ operation that failed or was stopped before the stack was created or updated.      * @CURRENT@ : The stack is currently up to date with the stack set.
siStatus :: Lens' StackInstance (Maybe StackInstanceStatus)
siStatus = lens _siStatus (\ s a -> s{_siStatus = a})

-- | The name of the AWS account that the stack instance is associated with.
siAccount :: Lens' StackInstance (Maybe Text)
siAccount = lens _siAccount (\ s a -> s{_siAccount = a})

-- | The name of the AWS region that the stack instance is associated with.
siRegion :: Lens' StackInstance (Maybe Text)
siRegion = lens _siRegion (\ s a -> s{_siRegion = a})

-- | The explanation for the specific status code that is assigned to this stack instance.
siStatusReason :: Lens' StackInstance (Maybe Text)
siStatusReason = lens _siStatusReason (\ s a -> s{_siStatusReason = a})

-- | The ID of the stack instance.
siStackId :: Lens' StackInstance (Maybe Text)
siStackId = lens _siStackId (\ s a -> s{_siStackId = a})

-- | A list of parameters from the stack set template whose values have been overridden in this stack instance.
siParameterOverrides :: Lens' StackInstance [Parameter]
siParameterOverrides = lens _siParameterOverrides (\ s a -> s{_siParameterOverrides = a}) . _Default . _Coerce

-- | The name or unique ID of the stack set that the stack instance is associated with.
siStackSetId :: Lens' StackInstance (Maybe Text)
siStackSetId = lens _siStackSetId (\ s a -> s{_siStackSetId = a})

instance FromXML StackInstance where
        parseXML x
          = StackInstance' <$>
              (x .@? "Status") <*> (x .@? "Account") <*>
                (x .@? "Region")
                <*> (x .@? "StatusReason")
                <*> (x .@? "StackId")
                <*>
                (x .@? "ParameterOverrides" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "StackSetId")

instance Hashable StackInstance where

instance NFData StackInstance where

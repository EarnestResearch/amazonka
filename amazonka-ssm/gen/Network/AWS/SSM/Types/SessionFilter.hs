{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.SessionFilter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.SessionFilter where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.SessionFilterKey

-- | Describes a filter for Session Manager information.
--
--
--
-- /See:/ 'sessionFilter' smart constructor.
data SessionFilter = SessionFilter'{_sfKey ::
                                    !SessionFilterKey,
                                    _sfValue :: !Text}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SessionFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfKey' - The name of the filter.
--
-- * 'sfValue' - The filter value. Valid values for each filter key are as follows:     * InvokedAfter: Specify a timestamp to limit your results. For example, specify 2018-08-29T00:00:00Z to see sessions that started August 29, 2018, and later.     * InvokedBefore: Specify a timestamp to limit your results. For example, specify 2018-08-29T00:00:00Z to see sessions that started before August 29, 2018.     * Target: Specify an instance to which session connections have been made.     * Owner: Specify an AWS user account to see a list of sessions started by that user.     * Status: Specify a valid session status to see a list of all sessions with that status. Status values you can specify include:     * Connected     * Connecting     * Disconnected     * Terminated     * Terminating     * Failed
sessionFilter
    :: SessionFilterKey -- ^ 'sfKey'
    -> Text -- ^ 'sfValue'
    -> SessionFilter
sessionFilter pKey_ pValue_
  = SessionFilter'{_sfKey = pKey_, _sfValue = pValue_}

-- | The name of the filter.
sfKey :: Lens' SessionFilter SessionFilterKey
sfKey = lens _sfKey (\ s a -> s{_sfKey = a})

-- | The filter value. Valid values for each filter key are as follows:     * InvokedAfter: Specify a timestamp to limit your results. For example, specify 2018-08-29T00:00:00Z to see sessions that started August 29, 2018, and later.     * InvokedBefore: Specify a timestamp to limit your results. For example, specify 2018-08-29T00:00:00Z to see sessions that started before August 29, 2018.     * Target: Specify an instance to which session connections have been made.     * Owner: Specify an AWS user account to see a list of sessions started by that user.     * Status: Specify a valid session status to see a list of all sessions with that status. Status values you can specify include:     * Connected     * Connecting     * Disconnected     * Terminated     * Terminating     * Failed
sfValue :: Lens' SessionFilter Text
sfValue = lens _sfValue (\ s a -> s{_sfValue = a})

instance Hashable SessionFilter where

instance NFData SessionFilter where

instance ToJSON SessionFilter where
        toJSON SessionFilter'{..}
          = object
              (catMaybes
                 [Just ("key" .= _sfKey), Just ("value" .= _sfValue)])

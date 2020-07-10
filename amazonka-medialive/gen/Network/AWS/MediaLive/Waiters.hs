{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Waiters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Waiters where

import Network.AWS.Lens
import Network.AWS.MediaLive.DescribeChannel
import Network.AWS.MediaLive.DescribeChannel
import Network.AWS.MediaLive.DescribeChannel
import Network.AWS.MediaLive.DescribeChannel
import Network.AWS.MediaLive.DescribeMultiplex
import Network.AWS.MediaLive.DescribeMultiplex
import Network.AWS.MediaLive.DescribeMultiplex
import Network.AWS.MediaLive.DescribeMultiplex
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Waiter

-- | Polls 'Network.AWS.MediaLive.DescribeChannel' every 5 seconds until a successful state is reached. An error is returned after 120 failed checks.
channelRunning :: Wait DescribeChannel
channelRunning
  = Wait{_waitName = "ChannelRunning",
         _waitAttempts = 120, _waitDelay = 5,
         _waitAcceptors =
           [matchAll "RUNNING" AcceptSuccess
              (dcrsState . _Just . to toTextCI),
            matchAll "STARTING" AcceptRetry
              (dcrsState . _Just . to toTextCI),
            matchStatus 500 AcceptRetry]}

-- | Polls 'Network.AWS.MediaLive.DescribeMultiplex' every 5 seconds until a successful state is reached. An error is returned after 120 failed checks.
multiplexRunning :: Wait DescribeMultiplex
multiplexRunning
  = Wait{_waitName = "MultiplexRunning",
         _waitAttempts = 120, _waitDelay = 5,
         _waitAcceptors =
           [matchAll "RUNNING" AcceptSuccess
              (dmrsState . _Just . to toTextCI),
            matchAll "STARTING" AcceptRetry
              (dmrsState . _Just . to toTextCI),
            matchStatus 500 AcceptRetry]}

-- | Polls 'Network.AWS.MediaLive.DescribeMultiplex' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
multiplexDeleted :: Wait DescribeMultiplex
multiplexDeleted
  = Wait{_waitName = "MultiplexDeleted",
         _waitAttempts = 20, _waitDelay = 5,
         _waitAcceptors =
           [matchAll "DELETED" AcceptSuccess
              (dmrsState . _Just . to toTextCI),
            matchAll "DELETING" AcceptRetry
              (dmrsState . _Just . to toTextCI),
            matchStatus 500 AcceptRetry]}

-- | Polls 'Network.AWS.MediaLive.DescribeChannel' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
channelStopped :: Wait DescribeChannel
channelStopped
  = Wait{_waitName = "ChannelStopped",
         _waitAttempts = 60, _waitDelay = 5,
         _waitAcceptors =
           [matchAll "IDLE" AcceptSuccess
              (dcrsState . _Just . to toTextCI),
            matchAll "STOPPING" AcceptRetry
              (dcrsState . _Just . to toTextCI),
            matchStatus 500 AcceptRetry]}

-- | Polls 'Network.AWS.MediaLive.DescribeMultiplex' every 3 seconds until a successful state is reached. An error is returned after 5 failed checks.
multiplexCreated :: Wait DescribeMultiplex
multiplexCreated
  = Wait{_waitName = "MultiplexCreated",
         _waitAttempts = 5, _waitDelay = 3,
         _waitAcceptors =
           [matchAll "IDLE" AcceptSuccess
              (dmrsState . _Just . to toTextCI),
            matchAll "CREATING" AcceptRetry
              (dmrsState . _Just . to toTextCI),
            matchStatus 500 AcceptRetry,
            matchAll "CREATE_FAILED" AcceptFailure
              (dmrsState . _Just . to toTextCI)]}

-- | Polls 'Network.AWS.MediaLive.DescribeChannel' every 3 seconds until a successful state is reached. An error is returned after 5 failed checks.
channelCreated :: Wait DescribeChannel
channelCreated
  = Wait{_waitName = "ChannelCreated",
         _waitAttempts = 5, _waitDelay = 3,
         _waitAcceptors =
           [matchAll "IDLE" AcceptSuccess
              (dcrsState . _Just . to toTextCI),
            matchAll "CREATING" AcceptRetry
              (dcrsState . _Just . to toTextCI),
            matchStatus 500 AcceptRetry,
            matchAll "CREATE_FAILED" AcceptFailure
              (dcrsState . _Just . to toTextCI)]}

-- | Polls 'Network.AWS.MediaLive.DescribeChannel' every 5 seconds until a successful state is reached. An error is returned after 84 failed checks.
channelDeleted :: Wait DescribeChannel
channelDeleted
  = Wait{_waitName = "ChannelDeleted",
         _waitAttempts = 84, _waitDelay = 5,
         _waitAcceptors =
           [matchAll "DELETED" AcceptSuccess
              (dcrsState . _Just . to toTextCI),
            matchAll "DELETING" AcceptRetry
              (dcrsState . _Just . to toTextCI),
            matchStatus 500 AcceptRetry]}

-- | Polls 'Network.AWS.MediaLive.DescribeMultiplex' every 5 seconds until a successful state is reached. An error is returned after 28 failed checks.
multiplexStopped :: Wait DescribeMultiplex
multiplexStopped
  = Wait{_waitName = "MultiplexStopped",
         _waitAttempts = 28, _waitDelay = 5,
         _waitAcceptors =
           [matchAll "IDLE" AcceptSuccess
              (dmrsState . _Just . to toTextCI),
            matchAll "STOPPING" AcceptRetry
              (dmrsState . _Just . to toTextCI),
            matchStatus 500 AcceptRetry]}

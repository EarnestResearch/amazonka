{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}

-- |
-- Module      : Network.AWS.Env
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Environment and AWS specific configuration for the
-- 'Network.AWS.AWS' and 'Control.Monad.Trans.AWS.AWST' monads.
module Network.AWS.Env
    (
    -- * Creating the Environment
      newEnv
    , newEnvWith

    , Env      (..)
    , HasEnv   (..)

    -- * Overriding Default Configuration
    , override
    , configure

    -- * Scoped Actions
    , reconfigure
    , within
    , once
    , timeout

    -- * Retry HTTP Exceptions
    , retryConnectionFailure
    ) where

import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.IORef
import Data.Maybe (fromMaybe)

import Network.AWS.Auth
import Network.AWS.Internal.Env
import Network.AWS.Types
import Network.HTTP.Conduit


-- | Creates a new environment with a new 'Manager' without debug logging
-- and uses 'getAuth' to expand/discover the supplied 'Credentials'.
-- Lenses from 'HasEnv' can be used to further configure the resulting 'Env'.
--
-- /Since:/ @1.5.0@ - The region is now retrieved from the @AWS_REGION@ environment
-- variable (identical to official SDKs), or defaults to @us-east-1@.
-- You can override the 'Env' region by using 'envRegion', or the current operation's
-- region by using 'within'.
--
-- /Since:/ @1.3.6@ - The default logic for retrying 'HttpException's now uses
-- 'retryConnectionFailure' to retry specific connection failure conditions up to 3 times.
-- Previously only service specific errors were automatically retried.
-- This can be reverted to the old behaviour by resetting the 'Env' using
-- 'envRetryCheck' lens to @(\\_ _ -> False)@.
--
-- Throws 'AuthError' when environment variables or IAM profiles cannot be read.
--
-- /See:/ 'newEnvWith'.
newEnv :: (Applicative m, MonadIO m, MonadCatch m)
       => Credentials -- ^ Credential discovery mechanism.
       -> m Env
newEnv c =
    liftIO (newManager tlsManagerSettings)
        >>= newEnvWith c Nothing

-- | /See:/ 'newEnv'
--
-- The 'Maybe' 'Bool' parameter is used by the EC2 instance check. By passing a
-- value of 'Nothing', the check will be performed. 'Just' 'True' would cause
-- the check to be skipped and the host treated as an EC2 instance.
--
-- Throws 'AuthError' when environment variables or IAM profiles cannot be read.
newEnvWith :: (Applicative m, MonadIO m, MonadCatch m)
           => Credentials -- ^ Credential discovery mechanism.
           -> Maybe Bool  -- ^ Preload the EC2 instance check.
           -> Manager
           -> m Env
newEnvWith c p m = do
    (a, fromMaybe NorthVirginia -> r) <- getAuth m c
    Env r (\_ _ -> pure ()) (retryConnectionFailure 3) mempty m
        <$> liftIO (newIORef p)
        <*> pure a


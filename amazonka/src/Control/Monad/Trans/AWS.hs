-- |
-- Module      : Control.Monad.Trans.Internal.AWS
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- The 'AWST' transformer provides the environment required to perform AWS
-- operations. The transformer is intended to be used directly
-- or embedded as a layer within a transformer stack.
--
-- "Network.AWS" contains an 'IO' specialised version of 'AWST' with a typeclass
-- to assist in automatically lifting operations.
module Control.Monad.Trans.AWS
    (
    -- * Running AWS Actions
      AWST
    , AWST'
    , runAWST
    , runResourceT
    , AWSConstraint

    -- * Authentication and Environment
    , newEnv
    , Env
    , HasEnv       (..)
    , askEnv

    -- ** Credential Discovery
    , Credentials  (..)
    -- $discovery

    -- ** Supported Regions
    , Region       (..)

    -- * Sending Requests
    -- $sending

    , send

    -- ** Pagination
    -- $pagination

    , paginate

    -- ** Waiters
    -- $waiters

    , await

    -- ** Service Configuration
    -- $service

    -- *** Overriding Defaults
    , configure
    , override

    -- *** Scoped Actions
    , reconfigure
    , within
    , once
    , timeout

    -- ** Streaming
    -- $streaming

    -- *** Request Bodies
    , ToHashedBody (..)
    , hashedFile
    , hashedBody

    -- *** Chunked Request Bodies
    , ToBody       (..)
    , ChunkSize    (..)
    , defaultChunkSize
    , chunkedFile
    , unsafeChunkedBody

    -- *** Response Bodies
    , sinkBody

    -- *** File Size and MD5/SHA256
    , getFileSize
    , sinkMD5
    , sinkSHA256

    -- * Presigning Requests
    -- $presigning

    , presignURL
    , presign

    -- * EC2 Instance Metadata
    -- $metadata

    , isEC2
    , dynamic
    , metadata
    , userdata

    , EC2.Dynamic  (..)
    , EC2.Metadata (..)

    -- * Running Asynchronous Actions
    -- $async

    -- * Handling Errors
    -- $errors

    , AsError      (..)
    , AsAuthError  (..)

    , trying
    , catching

    -- ** Building Error Prisms
    , Error._MatchServiceError
    , Error.hasService
    , Error.hasStatus
    , Error.hasCode

    -- * Logging
    -- $logging

    , Logger
    , LogLevel     (..)

    -- ** Constructing a Logger
    , newLogger

    -- ** Endpoints
    , Endpoint
    , setEndpoint

    -- * Re-exported Types
    , module Network.AWS.Types
    , module Network.AWS.Waiter
    , module Network.AWS.Pager
    , RqBody
    , HashedBody
    , ChunkedBody
    , RsBody
    ) where

import           Control.Monad.Trans.Internal.AWS
import qualified Network.AWS.EC2.Metadata         as EC2
import qualified Network.AWS.Error                as Error

import Network.AWS.Auth
import Network.AWS.Env
import Network.AWS.Pager
import Network.AWS.Types
import Network.AWS.Waiter

{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.AWS.Auth
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Explicitly specify your Amazon AWS security credentials, or retrieve them
-- from the underlying OS.
--
-- The format of environment variables and the credentials file follows the official
-- <http://blogs.aws.amazon.com/security/post/Tx3D6U6WSFGOK2H/A-New-and-Standardized-Way-to-Manage-Credentials-in-the-AWS-SDKs AWS SDK guidelines>.
module Network.AWS.Auth
    (
    -- * Authentication
    -- ** Retrieving Authentication
      getAuth
    , Credentials  (..)
    , Auth         (..)

    -- ** Defaults
    -- *** Environment
    , envAccessKey
    , envSecretKey
    , envSessionToken

    -- *** Credentials File
    , credAccessKey
    , credSecretKey
    , credSessionToken
    , credProfile
    , credFile

    -- ** Credentials
    -- $credentials

    , fromKeys
    , fromSession
    , fromTemporarySession
    , fromEnv
    , fromEnvKeys
    , fromFile
    , fromFilePath
    , fromProfile
    , fromProfileName
    , fromContainer

    -- ** Keys
    , AccessKey    (..)
    , SecretKey    (..)
    , SessionToken (..)

    -- ** Handling Errors
    , AsAuthError  (..)
    , AuthError    (..)
    ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Internal.AWS

import Data.Time.Clock.System

import           Network.AWS.Data.Log
import qualified Network.AWS.EC2.Metadata                  as EC2
import           Network.AWS.Internal.Auth
import           Network.AWS.Internal.Env
import           Network.AWS.Lens                          (catching, catching_,
                                                            throwingM, (<&>),
                                                            (^.))
import           Network.AWS.Prelude
import           Network.AWS.STS.AssumeRoleWithWebIdentity
import           Network.HTTP.Conduit

import System.Environment (lookupEnv)


import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text             as Text
import qualified Data.Text.IO          as TIO


-- | Role arn to assume through web identity (see FromWebIdentityToken)
envAwsRoleArn :: Text -- ^ AWS_ROLE_ARN
envAwsRoleArn = "AWS_ROLE_ARN"

envWebIdentityTokenFile :: Text -- ^ AWS_WEB_IDENTITY_TOKEN_FILE
envWebIdentityTokenFile = "AWS_WEB_IDENTITY_TOKEN_FILE"


-- | Determines how AuthN/AuthZ information is retrieved.
data Credentials
    = FromKeys AccessKey SecretKey
      -- ^ Explicit access and secret keys. See 'fromKeys'.

    | FromSession AccessKey SecretKey SessionToken
      -- ^ Explicit access key, secret key and a session token. See 'fromSession'.

    | FromEnv Text Text (Maybe Text) (Maybe Text)
      -- ^ Lookup specific environment variables for access key, secret key,
      -- an optional session token, and an optional region, respectively.

    | FromProfile Text
      -- ^ An IAM Profile name to lookup from the local EC2 instance-data.
      -- Environment variables to lookup for the access key, secret key and
      -- optional session token.

    | FromFile Text FilePath
      -- ^ A credentials profile name (the INI section) and the path to the AWS
      -- <http://blogs.aws.amazon.com/security/post/Tx3D6U6WSFGOK2H/A-New-and-Standardized-Way-to-Manage-Credentials-in-the-AWS-SDKs credentials> file.

    | FromContainer
      -- ^ Obtain credentials by attempting to contact the ECS container agent
      -- at <http://169.254.170.2> using the path in 'envContainerCredentialsURI'.
      -- See <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html IAM Roles for Tasks>
      -- in the AWS documentation for more information.

    | FromWebIdentityToken
      -- ^ Obtain credentials using a web identity token, normally supplied by
      -- an OIDC pvodier.
      -- See <https://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRoleWithWebIdentity.html>
      -- and <https://aws.amazon.com/blogs/opensource/introducing-fine-grained-iam-roles-service-accounts/>
      -- for more information

    | Discover
      -- ^ Attempt credentials discovery via the following steps:
      --
      -- * Read the 'envAccessKey', 'envSecretKey', and 'envRegion' from the environment if they are set.
      --
      -- * Read the credentials file if 'credFile' exists.
      --
      -- * Obtain credentials from the ECS container agent if
      -- 'envContainerCredentialsURI' is set.
      --
      -- * Retrieve the first available IAM profile and read
      -- the 'Region' from the instance identity document, if running on EC2.
      --
      -- An attempt is made to resolve <http://instance-data> rather than directly
      -- retrieving <http://169.254.169.254> for IAM profile information.
      -- This assists in ensuring the DNS lookup terminates promptly if not
      -- running on EC2.
      deriving (Eq)

instance ToLog Credentials where
    build = \case
        FromKeys    a _ ->
            "FromKeys " <> build a <> " ****"
        FromSession a _ _ ->
            "FromSession " <> build a <> " **** ****"
        FromEnv     a s t r ->
            "FromEnv " <> build a <> " " <> build s <> " " <> m t <> " " <> m r
        FromProfile n ->
            "FromProfile " <> build n
        FromFile    n f ->
            "FromFile " <> build n <> " " <> build f
        FromContainer ->
            "FromContainer"
        FromWebIdentityToken ->
            "FromWebIdentityToken"
        Discover ->
            "Discover"
      where
        m (Just x) = "(Just " <> build x <> ")"
        m Nothing  = "Nothing"

instance Show Credentials where
    show = BS8.unpack . toBS . build


-- | Retrieve authentication information via the specified 'Credentials' mechanism.
--
-- Throws 'AuthError' when environment variables or IAM profiles cannot be read,
-- and credentials files are invalid or cannot be found.
getAuth :: (Applicative m, MonadIO m, MonadCatch m)
        => Manager
        -> Credentials
        -> m (Auth, Maybe Region)
getAuth m = \case
    FromKeys    a s     -> return (fromKeys a s, Nothing)
    FromSession a s t   -> return (fromSession a s t, Nothing)
    FromEnv     a s t r -> fromEnvKeys a s t r
    FromProfile n       -> fromProfileName m n
    FromFile    n f     -> fromFilePath n f
    FromContainer       -> fromContainer m
    FromWebIdentityToken -> fromWebIdentityToken m
    Discover            ->
        -- Don't try and catch InvalidFileError, or InvalidIAMProfile,
        -- let both errors propagate.
        catching_ _MissingEnvError fromEnv $
            -- proceed, missing env keys
            catching_ _MissingEnvError (fromWebIdentityToken m) $
              -- proceed, missing AWS_ROLE_ARN
            catching _MissingFileError fromFile $ \f ->
                -- proceed, missing credentials file
                catching_ _MissingEnvError (fromContainer m) $ do
                  -- proceed, missing env key
                  p <- EC2.isEC2 m
                  unless p $
                      -- not an EC2 instance, rethrow the previous error.
                      throwingM _MissingFileError f
                   -- proceed, check EC2 metadata for IAM information.
                  fromProfile m


-- | Use OIDC token and sts:AssumeRoleWithWebIdentity call to fetch credentials
fromWebIdentityToken :: (MonadIO m, MonadCatch m) => Manager -> m (Auth, Maybe Region)
fromWebIdentityToken mgr = do
  -- sts:AssumeRoleWtihIdentity doesn't require credentials, credentials
  -- come from web token
    env <- emptyCredentialsEnv
    roleToAssume <- lookupEnvReq envAwsRoleArn <&> Text.pack
    tokenIdentityFile <- lookupEnvReq envWebIdentityTokenFile
    auth <- liftIO $ fetchAuthInBackground (renew env roleToAssume tokenIdentityFile)
    reg  <- getRegion
    return (auth, reg)
     where
        renew :: Env -> Text -> FilePath -> IO AuthEnv
        renew env roleToAssume tokenIdentityFile = do
            token <- TIO.readFile tokenIdentityFile
            roleSessionName <- (\x -> "amazonka-" <> (Text.pack . show . systemSeconds) x) <$> getSystemTime
            let assumeRoleReq = assumeRoleWithWebIdentity roleToAssume roleSessionName token
            assumeRoleResp <- runResourceT $ runAWST env $ send assumeRoleReq
            maybe (throwM . InvalidIAMError $ "No credentials returned") pure $ assumeRoleResp ^. arwwirsCredentials

        getRegion :: (MonadIO m, MonadCatch m) => m (Maybe Region)
        getRegion = do
            maybeRegionFromEnv <- lookupEnvOpt envVarRegion
            isEc2 <- EC2.isEC2 mgr
            case maybeRegionFromEnv of
              Just region' -> pure $ either (const Nothing) Just $ fromText (Text.pack region')
              Nothing -> if isEc2 then
                Just <$> lookupRegionFromInstanceDocument
                else pure Nothing

        lookupRegionFromInstanceDocument :: (MonadIO m, MonadCatch m) => m Region
        lookupRegionFromInstanceDocument = try (EC2.identity mgr) >>=
           handleErr (fmap EC2._region) invalidIdentityErr

        handleErr _ _ (Left  e) = throwM (RetrievalError e)
        handleErr f g (Right x) = either (throwM . g) return (f x)

        invalidIdentityErr = InvalidIAMError
            . mappend "Error parsing Instance Identity Document "
            . Text.pack

lookupEnvOpt :: MonadIO m => Text -> m (Maybe FilePath)
lookupEnvOpt key = liftIO (lookupEnv (Text.unpack key))

lookupEnvReq :: (MonadIO m, MonadThrow m) => Text -> m FilePath
lookupEnvReq key = do
        m <- lookupEnvOpt key
        maybe (throwM . MissingEnvError $ "Unable to read ENV variable: " <> key)
              return
              m

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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminInitiateAuth
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates the authentication flow, as an administrator.
--
--
-- Calling this action requires developer credentials.
--
module Network.AWS.CognitoIdentityProvider.AdminInitiateAuth
    (
    -- * Creating a Request
      adminInitiateAuth
    , AdminInitiateAuth
    -- * Request Lenses
    , aiaClientMetadata
    , aiaContextData
    , aiaAnalyticsMetadata
    , aiaAuthParameters
    , aiaUserPoolId
    , aiaClientId
    , aiaAuthFlow

    -- * Destructuring the Response
    , adminInitiateAuthResponse
    , AdminInitiateAuthResponse
    -- * Response Lenses
    , aiarsChallengeName
    , aiarsChallengeParameters
    , aiarsAuthenticationResult
    , aiarsSession
    , aiarsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Initiates the authorization request, as an administrator.
--
--
--
-- /See:/ 'adminInitiateAuth' smart constructor.
data AdminInitiateAuth = AdminInitiateAuth'{_aiaClientMetadata
                                            :: !(Maybe (Map Text Text)),
                                            _aiaContextData ::
                                            !(Maybe ContextDataType),
                                            _aiaAnalyticsMetadata ::
                                            !(Maybe AnalyticsMetadataType),
                                            _aiaAuthParameters ::
                                            !(Maybe
                                                (Map Text (Sensitive Text))),
                                            _aiaUserPoolId :: !Text,
                                            _aiaClientId :: !(Sensitive Text),
                                            _aiaAuthFlow :: !AuthFlowType}
                           deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'AdminInitiateAuth' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiaClientMetadata' - A map of custom key-value pairs that you can provide as input for certain custom workflows that this action triggers. You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the AdminInitiateAuth API action, Amazon Cognito invokes the AWS Lambda functions that are specified for various triggers. The ClientMetadata value is passed as input to the functions for only the following triggers:     * Pre signup     * Pre authentication     * User migration When Amazon Cognito invokes the functions for these triggers, it passes a JSON payload, which the function receives as input. This payload contains a @validationData@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your AdminInitiateAuth request. In your function code in AWS Lambda, you can process the @validationData@ value to enhance your workflow for your specific needs. When you use the AdminInitiateAuth API action, Amazon Cognito also invokes the functions for the following triggers, but it does not provide the ClientMetadata value as input:     * Post authentication     * Custom message     * Pre token generation     * Create auth challenge     * Define auth challenge     * Verify auth challenge For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- * 'aiaContextData' - Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- * 'aiaAnalyticsMetadata' - The analytics metadata for collecting Amazon Pinpoint metrics for @AdminInitiateAuth@ calls.
--
-- * 'aiaAuthParameters' - The authentication parameters. These are inputs corresponding to the @AuthFlow@ that you are invoking. The required values depend on the value of @AuthFlow@ :     * For @USER_SRP_AUTH@ : @USERNAME@ (required), @SRP_A@ (required), @SECRET_HASH@ (required if the app client is configured with a client secret), @DEVICE_KEY@      * For @REFRESH_TOKEN_AUTH/REFRESH_TOKEN@ : @REFRESH_TOKEN@ (required), @SECRET_HASH@ (required if the app client is configured with a client secret), @DEVICE_KEY@      * For @ADMIN_NO_SRP_AUTH@ : @USERNAME@ (required), @SECRET_HASH@ (if app client is configured with client secret), @PASSWORD@ (required), @DEVICE_KEY@      * For @CUSTOM_AUTH@ : @USERNAME@ (required), @SECRET_HASH@ (if app client is configured with client secret), @DEVICE_KEY@ 
--
-- * 'aiaUserPoolId' - The ID of the Amazon Cognito user pool.
--
-- * 'aiaClientId' - The app client ID.
--
-- * 'aiaAuthFlow' - The authentication flow for this call to execute. The API action will depend on this value. For example:     * @REFRESH_TOKEN_AUTH@ will take in a valid refresh token and return new tokens.     * @USER_SRP_AUTH@ will take in @USERNAME@ and @SRP_A@ and return the SRP variables to be used for next challenge execution.     * @USER_PASSWORD_AUTH@ will take in @USERNAME@ and @PASSWORD@ and return the next challenge or tokens. Valid values include:     * @USER_SRP_AUTH@ : Authentication flow for the Secure Remote Password (SRP) protocol.     * @REFRESH_TOKEN_AUTH@ /@REFRESH_TOKEN@ : Authentication flow for refreshing the access token and ID token by supplying a valid refresh token.     * @CUSTOM_AUTH@ : Custom authentication flow.     * @ADMIN_NO_SRP_AUTH@ : Non-SRP authentication flow; you can pass in the USERNAME and PASSWORD directly if the flow is enabled for calling the app client.     * @USER_PASSWORD_AUTH@ : Non-SRP authentication flow; USERNAME and PASSWORD are passed directly. If a user migration Lambda trigger is set, this flow will invoke the user migration Lambda if the USERNAME is not found in the user pool.      * @ADMIN_USER_PASSWORD_AUTH@ : Admin-based user password authentication. This replaces the @ADMIN_NO_SRP_AUTH@ authentication flow. In this flow, Cognito receives the password in the request instead of using the SRP process to verify passwords.
adminInitiateAuth
    :: Text -- ^ 'aiaUserPoolId'
    -> Text -- ^ 'aiaClientId'
    -> AuthFlowType -- ^ 'aiaAuthFlow'
    -> AdminInitiateAuth
adminInitiateAuth pUserPoolId_ pClientId_ pAuthFlow_
  = AdminInitiateAuth'{_aiaClientMetadata = Nothing,
                       _aiaContextData = Nothing,
                       _aiaAnalyticsMetadata = Nothing,
                       _aiaAuthParameters = Nothing,
                       _aiaUserPoolId = pUserPoolId_,
                       _aiaClientId = _Sensitive # pClientId_,
                       _aiaAuthFlow = pAuthFlow_}

-- | A map of custom key-value pairs that you can provide as input for certain custom workflows that this action triggers. You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the AdminInitiateAuth API action, Amazon Cognito invokes the AWS Lambda functions that are specified for various triggers. The ClientMetadata value is passed as input to the functions for only the following triggers:     * Pre signup     * Pre authentication     * User migration When Amazon Cognito invokes the functions for these triggers, it passes a JSON payload, which the function receives as input. This payload contains a @validationData@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your AdminInitiateAuth request. In your function code in AWS Lambda, you can process the @validationData@ value to enhance your workflow for your specific needs. When you use the AdminInitiateAuth API action, Amazon Cognito also invokes the functions for the following triggers, but it does not provide the ClientMetadata value as input:     * Post authentication     * Custom message     * Pre token generation     * Create auth challenge     * Define auth challenge     * Verify auth challenge For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
aiaClientMetadata :: Lens' AdminInitiateAuth (HashMap Text Text)
aiaClientMetadata = lens _aiaClientMetadata (\ s a -> s{_aiaClientMetadata = a}) . _Default . _Map

-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
aiaContextData :: Lens' AdminInitiateAuth (Maybe ContextDataType)
aiaContextData = lens _aiaContextData (\ s a -> s{_aiaContextData = a})

-- | The analytics metadata for collecting Amazon Pinpoint metrics for @AdminInitiateAuth@ calls.
aiaAnalyticsMetadata :: Lens' AdminInitiateAuth (Maybe AnalyticsMetadataType)
aiaAnalyticsMetadata = lens _aiaAnalyticsMetadata (\ s a -> s{_aiaAnalyticsMetadata = a})

-- | The authentication parameters. These are inputs corresponding to the @AuthFlow@ that you are invoking. The required values depend on the value of @AuthFlow@ :     * For @USER_SRP_AUTH@ : @USERNAME@ (required), @SRP_A@ (required), @SECRET_HASH@ (required if the app client is configured with a client secret), @DEVICE_KEY@      * For @REFRESH_TOKEN_AUTH/REFRESH_TOKEN@ : @REFRESH_TOKEN@ (required), @SECRET_HASH@ (required if the app client is configured with a client secret), @DEVICE_KEY@      * For @ADMIN_NO_SRP_AUTH@ : @USERNAME@ (required), @SECRET_HASH@ (if app client is configured with client secret), @PASSWORD@ (required), @DEVICE_KEY@      * For @CUSTOM_AUTH@ : @USERNAME@ (required), @SECRET_HASH@ (if app client is configured with client secret), @DEVICE_KEY@ 
aiaAuthParameters :: Lens' AdminInitiateAuth (HashMap Text Text)
aiaAuthParameters = lens _aiaAuthParameters (\ s a -> s{_aiaAuthParameters = a}) . _Default . _Map

-- | The ID of the Amazon Cognito user pool.
aiaUserPoolId :: Lens' AdminInitiateAuth Text
aiaUserPoolId = lens _aiaUserPoolId (\ s a -> s{_aiaUserPoolId = a})

-- | The app client ID.
aiaClientId :: Lens' AdminInitiateAuth Text
aiaClientId = lens _aiaClientId (\ s a -> s{_aiaClientId = a}) . _Sensitive

-- | The authentication flow for this call to execute. The API action will depend on this value. For example:     * @REFRESH_TOKEN_AUTH@ will take in a valid refresh token and return new tokens.     * @USER_SRP_AUTH@ will take in @USERNAME@ and @SRP_A@ and return the SRP variables to be used for next challenge execution.     * @USER_PASSWORD_AUTH@ will take in @USERNAME@ and @PASSWORD@ and return the next challenge or tokens. Valid values include:     * @USER_SRP_AUTH@ : Authentication flow for the Secure Remote Password (SRP) protocol.     * @REFRESH_TOKEN_AUTH@ /@REFRESH_TOKEN@ : Authentication flow for refreshing the access token and ID token by supplying a valid refresh token.     * @CUSTOM_AUTH@ : Custom authentication flow.     * @ADMIN_NO_SRP_AUTH@ : Non-SRP authentication flow; you can pass in the USERNAME and PASSWORD directly if the flow is enabled for calling the app client.     * @USER_PASSWORD_AUTH@ : Non-SRP authentication flow; USERNAME and PASSWORD are passed directly. If a user migration Lambda trigger is set, this flow will invoke the user migration Lambda if the USERNAME is not found in the user pool.      * @ADMIN_USER_PASSWORD_AUTH@ : Admin-based user password authentication. This replaces the @ADMIN_NO_SRP_AUTH@ authentication flow. In this flow, Cognito receives the password in the request instead of using the SRP process to verify passwords.
aiaAuthFlow :: Lens' AdminInitiateAuth AuthFlowType
aiaAuthFlow = lens _aiaAuthFlow (\ s a -> s{_aiaAuthFlow = a})

instance AWSRequest AdminInitiateAuth where
        type Rs AdminInitiateAuth = AdminInitiateAuthResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 AdminInitiateAuthResponse' <$>
                   (x .?> "ChallengeName") <*>
                     (x .?> "ChallengeParameters" .!@ mempty)
                     <*> (x .?> "AuthenticationResult")
                     <*> (x .?> "Session")
                     <*> (pure (fromEnum s)))

instance Hashable AdminInitiateAuth where

instance NFData AdminInitiateAuth where

instance ToHeaders AdminInitiateAuth where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.AdminInitiateAuth"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AdminInitiateAuth where
        toJSON AdminInitiateAuth'{..}
          = object
              (catMaybes
                 [("ClientMetadata" .=) <$> _aiaClientMetadata,
                  ("ContextData" .=) <$> _aiaContextData,
                  ("AnalyticsMetadata" .=) <$> _aiaAnalyticsMetadata,
                  ("AuthParameters" .=) <$> _aiaAuthParameters,
                  Just ("UserPoolId" .= _aiaUserPoolId),
                  Just ("ClientId" .= _aiaClientId),
                  Just ("AuthFlow" .= _aiaAuthFlow)])

instance ToPath AdminInitiateAuth where
        toPath = const "/"

instance ToQuery AdminInitiateAuth where
        toQuery = const mempty

-- | Initiates the authentication response, as an administrator.
--
--
--
-- /See:/ 'adminInitiateAuthResponse' smart constructor.
data AdminInitiateAuthResponse = AdminInitiateAuthResponse'{_aiarsChallengeName
                                                            ::
                                                            !(Maybe
                                                                ChallengeNameType),
                                                            _aiarsChallengeParameters
                                                            ::
                                                            !(Maybe
                                                                (Map Text
                                                                   Text)),
                                                            _aiarsAuthenticationResult
                                                            ::
                                                            !(Maybe
                                                                AuthenticationResultType),
                                                            _aiarsSession ::
                                                            !(Maybe Text),
                                                            _aiarsResponseStatus
                                                            :: !Int}
                                   deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'AdminInitiateAuthResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiarsChallengeName' - The name of the challenge which you are responding to with this call. This is returned to you in the @AdminInitiateAuth@ response if you need to pass another challenge.     * @MFA_SETUP@ : If MFA is required, users who do not have at least one of the MFA methods set up are presented with an @MFA_SETUP@ challenge. The user must set up at least one MFA type to continue to authenticate.     * @SELECT_MFA_TYPE@ : Selects the MFA type. Valid MFA options are @SMS_MFA@ for text SMS MFA, and @SOFTWARE_TOKEN_MFA@ for TOTP software token MFA.     * @SMS_MFA@ : Next challenge is to supply an @SMS_MFA_CODE@ , delivered via SMS.     * @PASSWORD_VERIFIER@ : Next challenge is to supply @PASSWORD_CLAIM_SIGNATURE@ , @PASSWORD_CLAIM_SECRET_BLOCK@ , and @TIMESTAMP@ after the client-side SRP calculations.     * @CUSTOM_CHALLENGE@ : This is returned if your custom authentication flow determines that the user should pass another challenge before tokens are issued.     * @DEVICE_SRP_AUTH@ : If device tracking was enabled on your user pool and the previous challenges were passed, this challenge is returned so that Amazon Cognito can start tracking this device.     * @DEVICE_PASSWORD_VERIFIER@ : Similar to @PASSWORD_VERIFIER@ , but for devices only.     * @ADMIN_NO_SRP_AUTH@ : This is returned if you need to authenticate with @USERNAME@ and @PASSWORD@ directly. An app client must be enabled to use this flow.     * @NEW_PASSWORD_REQUIRED@ : For users which are required to change their passwords after successful first login. This challenge should be passed with @NEW_PASSWORD@ and any other required attributes.
--
-- * 'aiarsChallengeParameters' - The challenge parameters. These are returned to you in the @AdminInitiateAuth@ response if you need to pass another challenge. The responses in this parameter should be used to compute inputs to the next call (@AdminRespondToAuthChallenge@ ). All challenges require @USERNAME@ and @SECRET_HASH@ (if applicable). The value of the @USER_ID_FOR_SRP@ attribute will be the user's actual username, not an alias (such as email address or phone number), even if you specified an alias in your call to @AdminInitiateAuth@ . This is because, in the @AdminRespondToAuthChallenge@ API @ChallengeResponses@ , the @USERNAME@ attribute cannot be an alias.
--
-- * 'aiarsAuthenticationResult' - The result of the authentication response. This is only returned if the caller does not need to pass another challenge. If the caller does need to pass another challenge before it gets tokens, @ChallengeName@ , @ChallengeParameters@ , and @Session@ are returned.
--
-- * 'aiarsSession' - The session which should be passed both ways in challenge-response calls to the service. If @AdminInitiateAuth@ or @AdminRespondToAuthChallenge@ API call determines that the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @AdminRespondToAuthChallenge@ API call.
--
-- * 'aiarsResponseStatus' - -- | The response status code.
adminInitiateAuthResponse
    :: Int -- ^ 'aiarsResponseStatus'
    -> AdminInitiateAuthResponse
adminInitiateAuthResponse pResponseStatus_
  = AdminInitiateAuthResponse'{_aiarsChallengeName =
                                 Nothing,
                               _aiarsChallengeParameters = Nothing,
                               _aiarsAuthenticationResult = Nothing,
                               _aiarsSession = Nothing,
                               _aiarsResponseStatus = pResponseStatus_}

-- | The name of the challenge which you are responding to with this call. This is returned to you in the @AdminInitiateAuth@ response if you need to pass another challenge.     * @MFA_SETUP@ : If MFA is required, users who do not have at least one of the MFA methods set up are presented with an @MFA_SETUP@ challenge. The user must set up at least one MFA type to continue to authenticate.     * @SELECT_MFA_TYPE@ : Selects the MFA type. Valid MFA options are @SMS_MFA@ for text SMS MFA, and @SOFTWARE_TOKEN_MFA@ for TOTP software token MFA.     * @SMS_MFA@ : Next challenge is to supply an @SMS_MFA_CODE@ , delivered via SMS.     * @PASSWORD_VERIFIER@ : Next challenge is to supply @PASSWORD_CLAIM_SIGNATURE@ , @PASSWORD_CLAIM_SECRET_BLOCK@ , and @TIMESTAMP@ after the client-side SRP calculations.     * @CUSTOM_CHALLENGE@ : This is returned if your custom authentication flow determines that the user should pass another challenge before tokens are issued.     * @DEVICE_SRP_AUTH@ : If device tracking was enabled on your user pool and the previous challenges were passed, this challenge is returned so that Amazon Cognito can start tracking this device.     * @DEVICE_PASSWORD_VERIFIER@ : Similar to @PASSWORD_VERIFIER@ , but for devices only.     * @ADMIN_NO_SRP_AUTH@ : This is returned if you need to authenticate with @USERNAME@ and @PASSWORD@ directly. An app client must be enabled to use this flow.     * @NEW_PASSWORD_REQUIRED@ : For users which are required to change their passwords after successful first login. This challenge should be passed with @NEW_PASSWORD@ and any other required attributes.
aiarsChallengeName :: Lens' AdminInitiateAuthResponse (Maybe ChallengeNameType)
aiarsChallengeName = lens _aiarsChallengeName (\ s a -> s{_aiarsChallengeName = a})

-- | The challenge parameters. These are returned to you in the @AdminInitiateAuth@ response if you need to pass another challenge. The responses in this parameter should be used to compute inputs to the next call (@AdminRespondToAuthChallenge@ ). All challenges require @USERNAME@ and @SECRET_HASH@ (if applicable). The value of the @USER_ID_FOR_SRP@ attribute will be the user's actual username, not an alias (such as email address or phone number), even if you specified an alias in your call to @AdminInitiateAuth@ . This is because, in the @AdminRespondToAuthChallenge@ API @ChallengeResponses@ , the @USERNAME@ attribute cannot be an alias.
aiarsChallengeParameters :: Lens' AdminInitiateAuthResponse (HashMap Text Text)
aiarsChallengeParameters = lens _aiarsChallengeParameters (\ s a -> s{_aiarsChallengeParameters = a}) . _Default . _Map

-- | The result of the authentication response. This is only returned if the caller does not need to pass another challenge. If the caller does need to pass another challenge before it gets tokens, @ChallengeName@ , @ChallengeParameters@ , and @Session@ are returned.
aiarsAuthenticationResult :: Lens' AdminInitiateAuthResponse (Maybe AuthenticationResultType)
aiarsAuthenticationResult = lens _aiarsAuthenticationResult (\ s a -> s{_aiarsAuthenticationResult = a})

-- | The session which should be passed both ways in challenge-response calls to the service. If @AdminInitiateAuth@ or @AdminRespondToAuthChallenge@ API call determines that the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @AdminRespondToAuthChallenge@ API call.
aiarsSession :: Lens' AdminInitiateAuthResponse (Maybe Text)
aiarsSession = lens _aiarsSession (\ s a -> s{_aiarsSession = a})

-- | -- | The response status code.
aiarsResponseStatus :: Lens' AdminInitiateAuthResponse Int
aiarsResponseStatus = lens _aiarsResponseStatus (\ s a -> s{_aiarsResponseStatus = a})

instance NFData AdminInitiateAuthResponse where

{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Lex provides both build and runtime endpoints. Each endpoint provides a set of operations (API). Your conversational bot uses the runtime API to understand user utterances (user input text or voice). For example, suppose a user says "I want pizza", your bot sends this input to Amazon Lex using the runtime API. Amazon Lex recognizes that the user request is for the OrderPizza intent (one of the intents defined in the bot). Then Amazon Lex engages in user conversation on behalf of the bot to elicit required information (slot values, such as pizza size and crust type), and then performs fulfillment activity (that you configured when you created the bot). You use the build-time API to create and manage your Amazon Lex bot. For a list of build-time operations, see the build-time API, . 
--
--
module Network.AWS.LexRuntime
    (
    -- * Service Configuration
      lexRuntime

    -- * Errors
    -- $errors

    -- ** DependencyFailedException
    , _DependencyFailedException

    -- ** InternalFailureException
    , _InternalFailureException

    -- ** BadRequestException
    , _BadRequestException

    -- ** BadGatewayException
    , _BadGatewayException

    -- ** NotAcceptableException
    , _NotAcceptableException

    -- ** LoopDetectedException
    , _LoopDetectedException

    -- ** RequestTimeoutException
    , _RequestTimeoutException

    -- ** NotFoundException
    , _NotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- ** ConflictException
    , _ConflictException

    -- ** UnsupportedMediaTypeException
    , _UnsupportedMediaTypeException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** PutSession 
    , module Network.AWS.LexRuntime.PutSession

    -- ** DeleteSession 
    , module Network.AWS.LexRuntime.DeleteSession

    -- ** PostText 
    , module Network.AWS.LexRuntime.PostText

    -- ** PostContent 
    , module Network.AWS.LexRuntime.PostContent

    -- ** GetSession 
    , module Network.AWS.LexRuntime.GetSession

    -- * Types

    -- ** ConfirmationStatus
    , ConfirmationStatus (..)

    -- ** ContentType
    , ContentType (..)

    -- ** DialogActionType
    , DialogActionType (..)

    -- ** DialogState
    , DialogState (..)

    -- ** FulfillmentState
    , FulfillmentState (..)

    -- ** MessageFormatType
    , MessageFormatType (..)

    -- ** Button
    , Button
    , button
    , bText
    , bValue

    -- ** DialogAction
    , DialogAction
    , dialogAction
    , daSlots
    , daIntentName
    , daFulfillmentState
    , daMessageFormat
    , daMessage
    , daSlotToElicit
    , daType

    -- ** GenericAttachment
    , GenericAttachment
    , genericAttachment
    , gaButtons
    , gaSubTitle
    , gaImageURL
    , gaAttachmentLinkURL
    , gaTitle

    -- ** IntentSummary
    , IntentSummary
    , intentSummary
    , isCheckpointLabel
    , isSlots
    , isIntentName
    , isFulfillmentState
    , isConfirmationStatus
    , isSlotToElicit
    , isDialogActionType

    -- ** ResponseCard
    , ResponseCard
    , responseCard
    , rcGenericAttachments
    , rcVersion
    , rcContentType

    -- ** SentimentResponse
    , SentimentResponse
    , sentimentResponse
    , sSentimentScore
    , sSentimentLabel
    ) where

import Network.AWS.LexRuntime.DeleteSession
import Network.AWS.LexRuntime.GetSession
import Network.AWS.LexRuntime.PostContent
import Network.AWS.LexRuntime.PostText
import Network.AWS.LexRuntime.PutSession
import Network.AWS.LexRuntime.Types
import Network.AWS.LexRuntime.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'LexRuntime'.
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

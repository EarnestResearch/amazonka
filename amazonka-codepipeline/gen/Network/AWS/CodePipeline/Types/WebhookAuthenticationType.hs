{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.WebhookAuthenticationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.WebhookAuthenticationType (
  WebhookAuthenticationType (
    ..
    , GithubHmac
    , IP
    , Unauthenticated
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data WebhookAuthenticationType = WebhookAuthenticationType' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern GithubHmac :: WebhookAuthenticationType
pattern GithubHmac = WebhookAuthenticationType' "GITHUB_HMAC"

pattern IP :: WebhookAuthenticationType
pattern IP = WebhookAuthenticationType' "IP"

pattern Unauthenticated :: WebhookAuthenticationType
pattern Unauthenticated = WebhookAuthenticationType' "UNAUTHENTICATED"

{-# COMPLETE
  GithubHmac,
  IP,
  Unauthenticated,
  WebhookAuthenticationType' #-}

instance FromText WebhookAuthenticationType where
    parser = (WebhookAuthenticationType' . mk) <$> takeText

instance ToText WebhookAuthenticationType where
    toText (WebhookAuthenticationType' ci) = original ci

-- | Represents an enum of /known/ $WebhookAuthenticationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum WebhookAuthenticationType where
    toEnum i = case i of
        0 -> GithubHmac
        1 -> IP
        2 -> Unauthenticated
        _ -> (error . showText) $ "Unknown index for WebhookAuthenticationType: " <> toText i
    fromEnum x = case x of
        GithubHmac -> 0
        IP -> 1
        Unauthenticated -> 2
        WebhookAuthenticationType' name -> (error . showText) $ "Unknown WebhookAuthenticationType: " <> original name

-- | Represents the bounds of /known/ $WebhookAuthenticationType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded WebhookAuthenticationType where
    minBound = GithubHmac
    maxBound = Unauthenticated

instance Hashable     WebhookAuthenticationType
instance NFData       WebhookAuthenticationType
instance ToByteString WebhookAuthenticationType
instance ToQuery      WebhookAuthenticationType
instance ToHeader     WebhookAuthenticationType

instance ToJSON WebhookAuthenticationType where
    toJSON = toJSONText

instance FromJSON WebhookAuthenticationType where
    parseJSON = parseJSONText "WebhookAuthenticationType"

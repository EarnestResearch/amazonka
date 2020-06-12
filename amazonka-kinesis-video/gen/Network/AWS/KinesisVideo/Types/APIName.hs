{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.APIName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideo.Types.APIName where

import Network.AWS.Prelude
  
data APIName = GetMedia
             | GetMediaForFragmentList
             | ListFragments
             | PutMedia
                 deriving (Eq, Ord, Read, Show, Enum, Bounded, Data,
                           Typeable, Generic)

instance FromText APIName where
    parser = takeLowerText >>= \case
        "get_media" -> pure GetMedia
        "get_media_for_fragment_list" -> pure GetMediaForFragmentList
        "list_fragments" -> pure ListFragments
        "put_media" -> pure PutMedia
        e -> fromTextError $ "Failure parsing APIName from value: '" <> e
           <> "'. Accepted values: get_media, get_media_for_fragment_list, list_fragments, put_media"

instance ToText APIName where
    toText = \case
        GetMedia -> "GET_MEDIA"
        GetMediaForFragmentList -> "GET_MEDIA_FOR_FRAGMENT_LIST"
        ListFragments -> "LIST_FRAGMENTS"
        PutMedia -> "PUT_MEDIA"

instance Hashable     APIName
instance NFData       APIName
instance ToByteString APIName
instance ToQuery      APIName
instance ToHeader     APIName

instance ToJSON APIName where
    toJSON = toJSONText

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ChannelSummary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.ChannelSummary where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.ChannelEgressEndpoint
import Network.AWS.MediaLive.Types.ChannelState
import Network.AWS.MediaLive.Types.InputAttachment
import Network.AWS.MediaLive.Types.InputSpecification
import Network.AWS.MediaLive.Types.OutputDestination
import Network.AWS.Prelude

-- | Placeholder documentation for ChannelSummary
--
-- /See:/ 'channelSummary' smart constructor.
data ChannelSummary = ChannelSummary'{_chaState ::
                                      !(Maybe ChannelState),
                                      _chaARN :: !(Maybe Text),
                                      _chaPipelinesRunningCount :: !(Maybe Int),
                                      _chaInputSpecification ::
                                      !(Maybe InputSpecification),
                                      _chaInputAttachments ::
                                      !(Maybe [InputAttachment]),
                                      _chaDestinations ::
                                      !(Maybe [OutputDestination]),
                                      _chaName :: !(Maybe Text),
                                      _chaId :: !(Maybe Text),
                                      _chaEgressEndpoints ::
                                      !(Maybe [ChannelEgressEndpoint]),
                                      _chaRoleARN :: !(Maybe Text)}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ChannelSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chaState' - Undocumented member.
--
-- * 'chaARN' - The unique arn of the channel.
--
-- * 'chaPipelinesRunningCount' - The number of currently healthy pipelines.
--
-- * 'chaInputSpecification' - Undocumented member.
--
-- * 'chaInputAttachments' - List of input attachments for channel.
--
-- * 'chaDestinations' - A list of destinations of the channel. For UDP outputs, there is one destination per output. For other types (HLS, for example), there is one destination per packager.
--
-- * 'chaName' - The name of the channel. (user-mutable)
--
-- * 'chaId' - The unique id of the channel.
--
-- * 'chaEgressEndpoints' - The endpoints where outgoing connections initiate from
--
-- * 'chaRoleARN' - The Amazon Resource Name (ARN) of the role assumed when running the Channel.
channelSummary
    :: ChannelSummary
channelSummary
  = ChannelSummary'{_chaState = Nothing,
                    _chaARN = Nothing,
                    _chaPipelinesRunningCount = Nothing,
                    _chaInputSpecification = Nothing,
                    _chaInputAttachments = Nothing,
                    _chaDestinations = Nothing, _chaName = Nothing,
                    _chaId = Nothing, _chaEgressEndpoints = Nothing,
                    _chaRoleARN = Nothing}

-- | Undocumented member.
chaState :: Lens' ChannelSummary (Maybe ChannelState)
chaState = lens _chaState (\ s a -> s{_chaState = a})

-- | The unique arn of the channel.
chaARN :: Lens' ChannelSummary (Maybe Text)
chaARN = lens _chaARN (\ s a -> s{_chaARN = a})

-- | The number of currently healthy pipelines.
chaPipelinesRunningCount :: Lens' ChannelSummary (Maybe Int)
chaPipelinesRunningCount = lens _chaPipelinesRunningCount (\ s a -> s{_chaPipelinesRunningCount = a})

-- | Undocumented member.
chaInputSpecification :: Lens' ChannelSummary (Maybe InputSpecification)
chaInputSpecification = lens _chaInputSpecification (\ s a -> s{_chaInputSpecification = a})

-- | List of input attachments for channel.
chaInputAttachments :: Lens' ChannelSummary [InputAttachment]
chaInputAttachments = lens _chaInputAttachments (\ s a -> s{_chaInputAttachments = a}) . _Default . _Coerce

-- | A list of destinations of the channel. For UDP outputs, there is one destination per output. For other types (HLS, for example), there is one destination per packager.
chaDestinations :: Lens' ChannelSummary [OutputDestination]
chaDestinations = lens _chaDestinations (\ s a -> s{_chaDestinations = a}) . _Default . _Coerce

-- | The name of the channel. (user-mutable)
chaName :: Lens' ChannelSummary (Maybe Text)
chaName = lens _chaName (\ s a -> s{_chaName = a})

-- | The unique id of the channel.
chaId :: Lens' ChannelSummary (Maybe Text)
chaId = lens _chaId (\ s a -> s{_chaId = a})

-- | The endpoints where outgoing connections initiate from
chaEgressEndpoints :: Lens' ChannelSummary [ChannelEgressEndpoint]
chaEgressEndpoints = lens _chaEgressEndpoints (\ s a -> s{_chaEgressEndpoints = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the role assumed when running the Channel.
chaRoleARN :: Lens' ChannelSummary (Maybe Text)
chaRoleARN = lens _chaRoleARN (\ s a -> s{_chaRoleARN = a})

instance FromJSON ChannelSummary where
        parseJSON
          = withObject "ChannelSummary"
              (\ x ->
                 ChannelSummary' <$>
                   (x .:? "state") <*> (x .:? "arn") <*>
                     (x .:? "pipelinesRunningCount")
                     <*> (x .:? "inputSpecification")
                     <*> (x .:? "inputAttachments" .!= mempty)
                     <*> (x .:? "destinations" .!= mempty)
                     <*> (x .:? "name")
                     <*> (x .:? "id")
                     <*> (x .:? "egressEndpoints" .!= mempty)
                     <*> (x .:? "roleArn"))

instance Hashable ChannelSummary where

instance NFData ChannelSummary where

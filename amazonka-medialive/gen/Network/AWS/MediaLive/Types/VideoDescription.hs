{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.VideoDescription
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.VideoDescription where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.VideoCodecSettings
import Network.AWS.MediaLive.Types.VideoDescriptionRespondToAfd
import Network.AWS.MediaLive.Types.VideoDescriptionScalingBehavior
import Network.AWS.Prelude

-- | Video settings for this stream.
--
-- /See:/ 'videoDescription' smart constructor.
data VideoDescription = VideoDescription'{_vdHeight
                                          :: !(Maybe Int),
                                          _vdSharpness :: !(Maybe Nat),
                                          _vdWidth :: !(Maybe Int),
                                          _vdScalingBehavior ::
                                          !(Maybe
                                              VideoDescriptionScalingBehavior),
                                          _vdRespondToAfd ::
                                          !(Maybe VideoDescriptionRespondToAfd),
                                          _vdCodecSettings ::
                                          !(Maybe VideoCodecSettings),
                                          _vdName :: !Text}
                          deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VideoDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vdHeight' - Output video height, in pixels. Must be an even number. For most codecs, you can leave this field and width blank in order to use the height and width (resolution) from the source. Note, however, that leaving blank is not recommended. For the Frame Capture codec, height and width are required.
--
-- * 'vdSharpness' - Changes the strength of the anti-alias filter used for scaling. 0 is the softest setting, 100 is the sharpest. A setting of 50 is recommended for most content.
--
-- * 'vdWidth' - Output video width, in pixels. Must be an even number. For most codecs, you can leave this field and height blank in order to use the height and width (resolution) from the source. Note, however, that leaving blank is not recommended. For the Frame Capture codec, height and width are required.
--
-- * 'vdScalingBehavior' - STRETCHTOOUTPUT configures the output position to stretch the video to the specified output resolution (height and width). This option will override any position value. DEFAULT may insert black boxes (pillar boxes or letter boxes) around the video to provide the specified output resolution.
--
-- * 'vdRespondToAfd' - Indicates how to respond to the AFD values in the input stream. RESPOND causes input video to be clipped, depending on the AFD value, input display aspect ratio, and output display aspect ratio, and (except for FRAMECAPTURE codec) includes the values in the output. PASSTHROUGH (does not apply to FRAMECAPTURE codec) ignores the AFD values and includes the values in the output, so input video is not clipped. NONE ignores the AFD values and does not include the values through to the output, so input video is not clipped.
--
-- * 'vdCodecSettings' - Video codec settings.
--
-- * 'vdName' - The name of this VideoDescription. Outputs will use this name to uniquely identify this Description.  Description names should be unique within this Live Event.
videoDescription
    :: Text -- ^ 'vdName'
    -> VideoDescription
videoDescription pName_
  = VideoDescription'{_vdHeight = Nothing,
                      _vdSharpness = Nothing, _vdWidth = Nothing,
                      _vdScalingBehavior = Nothing,
                      _vdRespondToAfd = Nothing,
                      _vdCodecSettings = Nothing, _vdName = pName_}

-- | Output video height, in pixels. Must be an even number. For most codecs, you can leave this field and width blank in order to use the height and width (resolution) from the source. Note, however, that leaving blank is not recommended. For the Frame Capture codec, height and width are required.
vdHeight :: Lens' VideoDescription (Maybe Int)
vdHeight = lens _vdHeight (\ s a -> s{_vdHeight = a})

-- | Changes the strength of the anti-alias filter used for scaling. 0 is the softest setting, 100 is the sharpest. A setting of 50 is recommended for most content.
vdSharpness :: Lens' VideoDescription (Maybe Natural)
vdSharpness = lens _vdSharpness (\ s a -> s{_vdSharpness = a}) . mapping _Nat

-- | Output video width, in pixels. Must be an even number. For most codecs, you can leave this field and height blank in order to use the height and width (resolution) from the source. Note, however, that leaving blank is not recommended. For the Frame Capture codec, height and width are required.
vdWidth :: Lens' VideoDescription (Maybe Int)
vdWidth = lens _vdWidth (\ s a -> s{_vdWidth = a})

-- | STRETCHTOOUTPUT configures the output position to stretch the video to the specified output resolution (height and width). This option will override any position value. DEFAULT may insert black boxes (pillar boxes or letter boxes) around the video to provide the specified output resolution.
vdScalingBehavior :: Lens' VideoDescription (Maybe VideoDescriptionScalingBehavior)
vdScalingBehavior = lens _vdScalingBehavior (\ s a -> s{_vdScalingBehavior = a})

-- | Indicates how to respond to the AFD values in the input stream. RESPOND causes input video to be clipped, depending on the AFD value, input display aspect ratio, and output display aspect ratio, and (except for FRAMECAPTURE codec) includes the values in the output. PASSTHROUGH (does not apply to FRAMECAPTURE codec) ignores the AFD values and includes the values in the output, so input video is not clipped. NONE ignores the AFD values and does not include the values through to the output, so input video is not clipped.
vdRespondToAfd :: Lens' VideoDescription (Maybe VideoDescriptionRespondToAfd)
vdRespondToAfd = lens _vdRespondToAfd (\ s a -> s{_vdRespondToAfd = a})

-- | Video codec settings.
vdCodecSettings :: Lens' VideoDescription (Maybe VideoCodecSettings)
vdCodecSettings = lens _vdCodecSettings (\ s a -> s{_vdCodecSettings = a})

-- | The name of this VideoDescription. Outputs will use this name to uniquely identify this Description.  Description names should be unique within this Live Event.
vdName :: Lens' VideoDescription Text
vdName = lens _vdName (\ s a -> s{_vdName = a})

instance FromJSON VideoDescription where
        parseJSON
          = withObject "VideoDescription"
              (\ x ->
                 VideoDescription' <$>
                   (x .:? "height") <*> (x .:? "sharpness") <*>
                     (x .:? "width")
                     <*> (x .:? "scalingBehavior")
                     <*> (x .:? "respondToAfd")
                     <*> (x .:? "codecSettings")
                     <*> (x .: "name"))

instance Hashable VideoDescription where

instance NFData VideoDescription where

instance ToJSON VideoDescription where
        toJSON VideoDescription'{..}
          = object
              (catMaybes
                 [("height" .=) <$> _vdHeight,
                  ("sharpness" .=) <$> _vdSharpness,
                  ("width" .=) <$> _vdWidth,
                  ("scalingBehavior" .=) <$> _vdScalingBehavior,
                  ("respondToAfd" .=) <$> _vdRespondToAfd,
                  ("codecSettings" .=) <$> _vdCodecSettings,
                  Just ("name" .= _vdName)])

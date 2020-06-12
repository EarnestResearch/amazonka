{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.GlobalConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.GlobalConfiguration where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.GlobalConfigurationInputEndAction
import Network.AWS.MediaLive.Types.GlobalConfigurationLowFramerateInputs
import Network.AWS.MediaLive.Types.GlobalConfigurationOutputTimingSource
import Network.AWS.MediaLive.Types.InputLossBehavior
import Network.AWS.Prelude

-- | Placeholder documentation for GlobalConfiguration
--
-- /See:/ 'globalConfiguration' smart constructor.
data GlobalConfiguration = GlobalConfiguration'{_gcInputLossBehavior
                                                :: !(Maybe InputLossBehavior),
                                                _gcInitialAudioGain ::
                                                !(Maybe Int),
                                                _gcSupportLowFramerateInputs ::
                                                !(Maybe
                                                    GlobalConfigurationLowFramerateInputs),
                                                _gcInputEndAction ::
                                                !(Maybe
                                                    GlobalConfigurationInputEndAction),
                                                _gcOutputTimingSource ::
                                                !(Maybe
                                                    GlobalConfigurationOutputTimingSource)}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GlobalConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcInputLossBehavior' - Settings for system actions when input is lost.
--
-- * 'gcInitialAudioGain' - Value to set the initial audio gain for the Live Event.
--
-- * 'gcSupportLowFramerateInputs' - Adjusts video input buffer for streams with very low video framerates. This is commonly set to enabled for music channels with less than one video frame per second.
--
-- * 'gcInputEndAction' - Indicates the action to take when an input completes (e.g. end-of-file.) Options include immediately switching to the next sequential input (via "switchInput"), switching to the next input and looping back to the first input when last input ends (via "switchAndLoopInputs") or not switching inputs and instead transcoding black / color / slate images per the "Input Loss Behavior" configuration until an activateInput REST command is received (via "none").
--
-- * 'gcOutputTimingSource' - Indicates whether the rate of frames emitted by the Live encoder should be paced by its system clock (which optionally may be locked to another source via NTP) or should be locked to the clock of the source that is providing the input stream.
globalConfiguration
    :: GlobalConfiguration
globalConfiguration
  = GlobalConfiguration'{_gcInputLossBehavior =
                           Nothing,
                         _gcInitialAudioGain = Nothing,
                         _gcSupportLowFramerateInputs = Nothing,
                         _gcInputEndAction = Nothing,
                         _gcOutputTimingSource = Nothing}

-- | Settings for system actions when input is lost.
gcInputLossBehavior :: Lens' GlobalConfiguration (Maybe InputLossBehavior)
gcInputLossBehavior = lens _gcInputLossBehavior (\ s a -> s{_gcInputLossBehavior = a})

-- | Value to set the initial audio gain for the Live Event.
gcInitialAudioGain :: Lens' GlobalConfiguration (Maybe Int)
gcInitialAudioGain = lens _gcInitialAudioGain (\ s a -> s{_gcInitialAudioGain = a})

-- | Adjusts video input buffer for streams with very low video framerates. This is commonly set to enabled for music channels with less than one video frame per second.
gcSupportLowFramerateInputs :: Lens' GlobalConfiguration (Maybe GlobalConfigurationLowFramerateInputs)
gcSupportLowFramerateInputs = lens _gcSupportLowFramerateInputs (\ s a -> s{_gcSupportLowFramerateInputs = a})

-- | Indicates the action to take when an input completes (e.g. end-of-file.) Options include immediately switching to the next sequential input (via "switchInput"), switching to the next input and looping back to the first input when last input ends (via "switchAndLoopInputs") or not switching inputs and instead transcoding black / color / slate images per the "Input Loss Behavior" configuration until an activateInput REST command is received (via "none").
gcInputEndAction :: Lens' GlobalConfiguration (Maybe GlobalConfigurationInputEndAction)
gcInputEndAction = lens _gcInputEndAction (\ s a -> s{_gcInputEndAction = a})

-- | Indicates whether the rate of frames emitted by the Live encoder should be paced by its system clock (which optionally may be locked to another source via NTP) or should be locked to the clock of the source that is providing the input stream.
gcOutputTimingSource :: Lens' GlobalConfiguration (Maybe GlobalConfigurationOutputTimingSource)
gcOutputTimingSource = lens _gcOutputTimingSource (\ s a -> s{_gcOutputTimingSource = a})

instance FromJSON GlobalConfiguration where
        parseJSON
          = withObject "GlobalConfiguration"
              (\ x ->
                 GlobalConfiguration' <$>
                   (x .:? "inputLossBehavior") <*>
                     (x .:? "initialAudioGain")
                     <*> (x .:? "supportLowFramerateInputs")
                     <*> (x .:? "inputEndAction")
                     <*> (x .:? "outputTimingSource"))

instance Hashable GlobalConfiguration where

instance NFData GlobalConfiguration where

instance ToJSON GlobalConfiguration where
        toJSON GlobalConfiguration'{..}
          = object
              (catMaybes
                 [("inputLossBehavior" .=) <$> _gcInputLossBehavior,
                  ("initialAudioGain" .=) <$> _gcInitialAudioGain,
                  ("supportLowFramerateInputs" .=) <$>
                    _gcSupportLowFramerateInputs,
                  ("inputEndAction" .=) <$> _gcInputEndAction,
                  ("outputTimingSource" .=) <$> _gcOutputTimingSource])

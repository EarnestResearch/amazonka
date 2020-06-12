{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.OTAUpdateInfo
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.OTAUpdateInfo where

import Network.AWS.IoT.Types.ErrorInfo
import Network.AWS.IoT.Types.OTAUpdateFile
import Network.AWS.IoT.Types.OTAUpdateStatus
import Network.AWS.IoT.Types.TargetSelection
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an OTA update.
--
--
--
-- /See:/ 'oTAUpdateInfo' smart constructor.
data OTAUpdateInfo = OTAUpdateInfo'{_otauiLastModifiedDate
                                    :: !(Maybe POSIX),
                                    _otauiAwsIotJobId :: !(Maybe Text),
                                    _otauiOtaUpdateFiles ::
                                    !(Maybe (List1 OTAUpdateFile)),
                                    _otauiOtaUpdateStatus ::
                                    !(Maybe OTAUpdateStatus),
                                    _otauiTargets :: !(Maybe (List1 Text)),
                                    _otauiAwsIotJobARN :: !(Maybe Text),
                                    _otauiCreationDate :: !(Maybe POSIX),
                                    _otauiAdditionalParameters ::
                                    !(Maybe (Map Text Text)),
                                    _otauiOtaUpdateId :: !(Maybe Text),
                                    _otauiErrorInfo :: !(Maybe ErrorInfo),
                                    _otauiOtaUpdateARN :: !(Maybe Text),
                                    _otauiDescription :: !(Maybe Text),
                                    _otauiTargetSelection ::
                                    !(Maybe TargetSelection)}
                       deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OTAUpdateInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'otauiLastModifiedDate' - The date when the OTA update was last updated.
--
-- * 'otauiAwsIotJobId' - The AWS IoT job ID associated with the OTA update.
--
-- * 'otauiOtaUpdateFiles' - A list of files associated with the OTA update.
--
-- * 'otauiOtaUpdateStatus' - The status of the OTA update.
--
-- * 'otauiTargets' - The targets of the OTA update.
--
-- * 'otauiAwsIotJobARN' - The AWS IoT job ARN associated with the OTA update.
--
-- * 'otauiCreationDate' - The date when the OTA update was created.
--
-- * 'otauiAdditionalParameters' - A collection of name/value pairs
--
-- * 'otauiOtaUpdateId' - The OTA update ID.
--
-- * 'otauiErrorInfo' - Error information associated with the OTA update.
--
-- * 'otauiOtaUpdateARN' - The OTA update ARN.
--
-- * 'otauiDescription' - A description of the OTA update.
--
-- * 'otauiTargetSelection' - Specifies whether the OTA update will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the OTA update (SNAPSHOT). If continuous, the OTA update may also be run on a thing when a change is detected in a target. For example, an OTA update will run on a thing when the thing is added to a target group, even after the OTA update was completed by all things originally in the group. 
oTAUpdateInfo
    :: OTAUpdateInfo
oTAUpdateInfo
  = OTAUpdateInfo'{_otauiLastModifiedDate = Nothing,
                   _otauiAwsIotJobId = Nothing,
                   _otauiOtaUpdateFiles = Nothing,
                   _otauiOtaUpdateStatus = Nothing,
                   _otauiTargets = Nothing,
                   _otauiAwsIotJobARN = Nothing,
                   _otauiCreationDate = Nothing,
                   _otauiAdditionalParameters = Nothing,
                   _otauiOtaUpdateId = Nothing,
                   _otauiErrorInfo = Nothing,
                   _otauiOtaUpdateARN = Nothing,
                   _otauiDescription = Nothing,
                   _otauiTargetSelection = Nothing}

-- | The date when the OTA update was last updated.
otauiLastModifiedDate :: Lens' OTAUpdateInfo (Maybe UTCTime)
otauiLastModifiedDate = lens _otauiLastModifiedDate (\ s a -> s{_otauiLastModifiedDate = a}) . mapping _Time

-- | The AWS IoT job ID associated with the OTA update.
otauiAwsIotJobId :: Lens' OTAUpdateInfo (Maybe Text)
otauiAwsIotJobId = lens _otauiAwsIotJobId (\ s a -> s{_otauiAwsIotJobId = a})

-- | A list of files associated with the OTA update.
otauiOtaUpdateFiles :: Lens' OTAUpdateInfo (Maybe (NonEmpty OTAUpdateFile))
otauiOtaUpdateFiles = lens _otauiOtaUpdateFiles (\ s a -> s{_otauiOtaUpdateFiles = a}) . mapping _List1

-- | The status of the OTA update.
otauiOtaUpdateStatus :: Lens' OTAUpdateInfo (Maybe OTAUpdateStatus)
otauiOtaUpdateStatus = lens _otauiOtaUpdateStatus (\ s a -> s{_otauiOtaUpdateStatus = a})

-- | The targets of the OTA update.
otauiTargets :: Lens' OTAUpdateInfo (Maybe (NonEmpty Text))
otauiTargets = lens _otauiTargets (\ s a -> s{_otauiTargets = a}) . mapping _List1

-- | The AWS IoT job ARN associated with the OTA update.
otauiAwsIotJobARN :: Lens' OTAUpdateInfo (Maybe Text)
otauiAwsIotJobARN = lens _otauiAwsIotJobARN (\ s a -> s{_otauiAwsIotJobARN = a})

-- | The date when the OTA update was created.
otauiCreationDate :: Lens' OTAUpdateInfo (Maybe UTCTime)
otauiCreationDate = lens _otauiCreationDate (\ s a -> s{_otauiCreationDate = a}) . mapping _Time

-- | A collection of name/value pairs
otauiAdditionalParameters :: Lens' OTAUpdateInfo (HashMap Text Text)
otauiAdditionalParameters = lens _otauiAdditionalParameters (\ s a -> s{_otauiAdditionalParameters = a}) . _Default . _Map

-- | The OTA update ID.
otauiOtaUpdateId :: Lens' OTAUpdateInfo (Maybe Text)
otauiOtaUpdateId = lens _otauiOtaUpdateId (\ s a -> s{_otauiOtaUpdateId = a})

-- | Error information associated with the OTA update.
otauiErrorInfo :: Lens' OTAUpdateInfo (Maybe ErrorInfo)
otauiErrorInfo = lens _otauiErrorInfo (\ s a -> s{_otauiErrorInfo = a})

-- | The OTA update ARN.
otauiOtaUpdateARN :: Lens' OTAUpdateInfo (Maybe Text)
otauiOtaUpdateARN = lens _otauiOtaUpdateARN (\ s a -> s{_otauiOtaUpdateARN = a})

-- | A description of the OTA update.
otauiDescription :: Lens' OTAUpdateInfo (Maybe Text)
otauiDescription = lens _otauiDescription (\ s a -> s{_otauiDescription = a})

-- | Specifies whether the OTA update will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the OTA update (SNAPSHOT). If continuous, the OTA update may also be run on a thing when a change is detected in a target. For example, an OTA update will run on a thing when the thing is added to a target group, even after the OTA update was completed by all things originally in the group. 
otauiTargetSelection :: Lens' OTAUpdateInfo (Maybe TargetSelection)
otauiTargetSelection = lens _otauiTargetSelection (\ s a -> s{_otauiTargetSelection = a})

instance FromJSON OTAUpdateInfo where
        parseJSON
          = withObject "OTAUpdateInfo"
              (\ x ->
                 OTAUpdateInfo' <$>
                   (x .:? "lastModifiedDate") <*> (x .:? "awsIotJobId")
                     <*> (x .:? "otaUpdateFiles")
                     <*> (x .:? "otaUpdateStatus")
                     <*> (x .:? "targets")
                     <*> (x .:? "awsIotJobArn")
                     <*> (x .:? "creationDate")
                     <*> (x .:? "additionalParameters" .!= mempty)
                     <*> (x .:? "otaUpdateId")
                     <*> (x .:? "errorInfo")
                     <*> (x .:? "otaUpdateArn")
                     <*> (x .:? "description")
                     <*> (x .:? "targetSelection"))

instance Hashable OTAUpdateInfo where

instance NFData OTAUpdateInfo where

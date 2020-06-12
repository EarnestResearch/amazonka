{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.TapeInfo
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StorageGateway.Types.TapeInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a virtual tape.
--
--
--
-- /See:/ 'tapeInfo' smart constructor.
data TapeInfo = TapeInfo'{_tiTapeBarcode ::
                          !(Maybe Text),
                          _tiTapeStatus :: !(Maybe Text),
                          _tiTapeARN :: !(Maybe Text),
                          _tiGatewayARN :: !(Maybe Text),
                          _tiTapeSizeInBytes :: !(Maybe Integer)}
                  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TapeInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tiTapeBarcode' - The barcode that identifies a specific virtual tape.
--
-- * 'tiTapeStatus' - The status of the tape.
--
-- * 'tiTapeARN' - The Amazon Resource Name (ARN) of a virtual tape.
--
-- * 'tiGatewayARN' - The Amazon Resource Name (ARN) of the gateway. Use the 'ListGateways' operation to return a list of gateways for your account and region.
--
-- * 'tiTapeSizeInBytes' - The size, in bytes, of a virtual tape.
tapeInfo
    :: TapeInfo
tapeInfo
  = TapeInfo'{_tiTapeBarcode = Nothing,
              _tiTapeStatus = Nothing, _tiTapeARN = Nothing,
              _tiGatewayARN = Nothing,
              _tiTapeSizeInBytes = Nothing}

-- | The barcode that identifies a specific virtual tape.
tiTapeBarcode :: Lens' TapeInfo (Maybe Text)
tiTapeBarcode = lens _tiTapeBarcode (\ s a -> s{_tiTapeBarcode = a})

-- | The status of the tape.
tiTapeStatus :: Lens' TapeInfo (Maybe Text)
tiTapeStatus = lens _tiTapeStatus (\ s a -> s{_tiTapeStatus = a})

-- | The Amazon Resource Name (ARN) of a virtual tape.
tiTapeARN :: Lens' TapeInfo (Maybe Text)
tiTapeARN = lens _tiTapeARN (\ s a -> s{_tiTapeARN = a})

-- | The Amazon Resource Name (ARN) of the gateway. Use the 'ListGateways' operation to return a list of gateways for your account and region.
tiGatewayARN :: Lens' TapeInfo (Maybe Text)
tiGatewayARN = lens _tiGatewayARN (\ s a -> s{_tiGatewayARN = a})

-- | The size, in bytes, of a virtual tape.
tiTapeSizeInBytes :: Lens' TapeInfo (Maybe Integer)
tiTapeSizeInBytes = lens _tiTapeSizeInBytes (\ s a -> s{_tiTapeSizeInBytes = a})

instance FromJSON TapeInfo where
        parseJSON
          = withObject "TapeInfo"
              (\ x ->
                 TapeInfo' <$>
                   (x .:? "TapeBarcode") <*> (x .:? "TapeStatus") <*>
                     (x .:? "TapeARN")
                     <*> (x .:? "GatewayARN")
                     <*> (x .:? "TapeSizeInBytes"))

instance Hashable TapeInfo where

instance NFData TapeInfo where

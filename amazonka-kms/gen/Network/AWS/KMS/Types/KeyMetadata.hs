{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.KeyMetadata
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KMS.Types.KeyMetadata where

import Network.AWS.KMS.Types.ExpirationModelType
import Network.AWS.KMS.Types.KeyManagerType
import Network.AWS.KMS.Types.KeyState
import Network.AWS.KMS.Types.KeyUsageType
import Network.AWS.KMS.Types.OriginType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains metadata about a customer master key (CMK).
--
--
-- This data type is used as a response element for the 'CreateKey' and 'DescribeKey' operations.
--
--
-- /See:/ 'keyMetadata' smart constructor.
data KeyMetadata = KeyMetadata'{_kmOrigin ::
                                !(Maybe OriginType),
                                _kmExpirationModel ::
                                !(Maybe ExpirationModelType),
                                _kmKeyManager :: !(Maybe KeyManagerType),
                                _kmEnabled :: !(Maybe Bool),
                                _kmValidTo :: !(Maybe POSIX),
                                _kmARN :: !(Maybe Text),
                                _kmKeyState :: !(Maybe KeyState),
                                _kmAWSAccountId :: !(Maybe Text),
                                _kmKeyUsage :: !(Maybe KeyUsageType),
                                _kmCreationDate :: !(Maybe POSIX),
                                _kmDeletionDate :: !(Maybe POSIX),
                                _kmDescription :: !(Maybe Text),
                                _kmKeyId :: !Text}
                     deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KeyMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kmOrigin' - The source of the CMK's key material. When this value is @AWS_KMS@ , AWS KMS created the key material. When this value is @EXTERNAL@ , the key material was imported from your existing key management infrastructure or the CMK lacks key material.
--
-- * 'kmExpirationModel' - Specifies whether the CMK's key material expires. This value is present only when @Origin@ is @EXTERNAL@ , otherwise this value is omitted.
--
-- * 'kmKeyManager' - The CMK's manager. CMKs are either customer-managed or AWS-managed. For more information about the difference, see <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'kmEnabled' - Specifies whether the CMK is enabled. When @KeyState@ is @Enabled@ this value is true, otherwise it is false.
--
-- * 'kmValidTo' - The time at which the imported key material expires. When the key material expires, AWS KMS deletes the key material and the CMK becomes unusable. This value is present only for CMKs whose @Origin@ is @EXTERNAL@ and whose @ExpirationModel@ is @KEY_MATERIAL_EXPIRES@ , otherwise this value is omitted.
--
-- * 'kmARN' - The Amazon Resource Name (ARN) of the CMK. For examples, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms AWS Key Management Service (AWS KMS)> in the Example ARNs section of the /AWS General Reference/ .
--
-- * 'kmKeyState' - The state of the CMK. For more information about how key state affects the use of a CMK, see <http://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects the Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'kmAWSAccountId' - The twelve-digit account ID of the AWS account that owns the CMK.
--
-- * 'kmKeyUsage' - The cryptographic operations for which you can use the CMK. Currently the only allowed value is @ENCRYPT_DECRYPT@ , which means you can use the CMK for the 'Encrypt' and 'Decrypt' operations.
--
-- * 'kmCreationDate' - The date and time when the CMK was created.
--
-- * 'kmDeletionDate' - The date and time after which AWS KMS deletes the CMK. This value is present only when @KeyState@ is @PendingDeletion@ , otherwise this value is omitted.
--
-- * 'kmDescription' - The description of the CMK.
--
-- * 'kmKeyId' - The globally unique identifier for the CMK.
keyMetadata
    :: Text -- ^ 'kmKeyId'
    -> KeyMetadata
keyMetadata pKeyId_
  = KeyMetadata'{_kmOrigin = Nothing,
                 _kmExpirationModel = Nothing,
                 _kmKeyManager = Nothing, _kmEnabled = Nothing,
                 _kmValidTo = Nothing, _kmARN = Nothing,
                 _kmKeyState = Nothing, _kmAWSAccountId = Nothing,
                 _kmKeyUsage = Nothing, _kmCreationDate = Nothing,
                 _kmDeletionDate = Nothing, _kmDescription = Nothing,
                 _kmKeyId = pKeyId_}

-- | The source of the CMK's key material. When this value is @AWS_KMS@ , AWS KMS created the key material. When this value is @EXTERNAL@ , the key material was imported from your existing key management infrastructure or the CMK lacks key material.
kmOrigin :: Lens' KeyMetadata (Maybe OriginType)
kmOrigin = lens _kmOrigin (\ s a -> s{_kmOrigin = a})

-- | Specifies whether the CMK's key material expires. This value is present only when @Origin@ is @EXTERNAL@ , otherwise this value is omitted.
kmExpirationModel :: Lens' KeyMetadata (Maybe ExpirationModelType)
kmExpirationModel = lens _kmExpirationModel (\ s a -> s{_kmExpirationModel = a})

-- | The CMK's manager. CMKs are either customer-managed or AWS-managed. For more information about the difference, see <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys> in the /AWS Key Management Service Developer Guide/ .
kmKeyManager :: Lens' KeyMetadata (Maybe KeyManagerType)
kmKeyManager = lens _kmKeyManager (\ s a -> s{_kmKeyManager = a})

-- | Specifies whether the CMK is enabled. When @KeyState@ is @Enabled@ this value is true, otherwise it is false.
kmEnabled :: Lens' KeyMetadata (Maybe Bool)
kmEnabled = lens _kmEnabled (\ s a -> s{_kmEnabled = a})

-- | The time at which the imported key material expires. When the key material expires, AWS KMS deletes the key material and the CMK becomes unusable. This value is present only for CMKs whose @Origin@ is @EXTERNAL@ and whose @ExpirationModel@ is @KEY_MATERIAL_EXPIRES@ , otherwise this value is omitted.
kmValidTo :: Lens' KeyMetadata (Maybe UTCTime)
kmValidTo = lens _kmValidTo (\ s a -> s{_kmValidTo = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the CMK. For examples, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms AWS Key Management Service (AWS KMS)> in the Example ARNs section of the /AWS General Reference/ .
kmARN :: Lens' KeyMetadata (Maybe Text)
kmARN = lens _kmARN (\ s a -> s{_kmARN = a})

-- | The state of the CMK. For more information about how key state affects the use of a CMK, see <http://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects the Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
kmKeyState :: Lens' KeyMetadata (Maybe KeyState)
kmKeyState = lens _kmKeyState (\ s a -> s{_kmKeyState = a})

-- | The twelve-digit account ID of the AWS account that owns the CMK.
kmAWSAccountId :: Lens' KeyMetadata (Maybe Text)
kmAWSAccountId = lens _kmAWSAccountId (\ s a -> s{_kmAWSAccountId = a})

-- | The cryptographic operations for which you can use the CMK. Currently the only allowed value is @ENCRYPT_DECRYPT@ , which means you can use the CMK for the 'Encrypt' and 'Decrypt' operations.
kmKeyUsage :: Lens' KeyMetadata (Maybe KeyUsageType)
kmKeyUsage = lens _kmKeyUsage (\ s a -> s{_kmKeyUsage = a})

-- | The date and time when the CMK was created.
kmCreationDate :: Lens' KeyMetadata (Maybe UTCTime)
kmCreationDate = lens _kmCreationDate (\ s a -> s{_kmCreationDate = a}) . mapping _Time

-- | The date and time after which AWS KMS deletes the CMK. This value is present only when @KeyState@ is @PendingDeletion@ , otherwise this value is omitted.
kmDeletionDate :: Lens' KeyMetadata (Maybe UTCTime)
kmDeletionDate = lens _kmDeletionDate (\ s a -> s{_kmDeletionDate = a}) . mapping _Time

-- | The description of the CMK.
kmDescription :: Lens' KeyMetadata (Maybe Text)
kmDescription = lens _kmDescription (\ s a -> s{_kmDescription = a})

-- | The globally unique identifier for the CMK.
kmKeyId :: Lens' KeyMetadata Text
kmKeyId = lens _kmKeyId (\ s a -> s{_kmKeyId = a})

instance FromJSON KeyMetadata where
        parseJSON
          = withObject "KeyMetadata"
              (\ x ->
                 KeyMetadata' <$>
                   (x .:? "Origin") <*> (x .:? "ExpirationModel") <*>
                     (x .:? "KeyManager")
                     <*> (x .:? "Enabled")
                     <*> (x .:? "ValidTo")
                     <*> (x .:? "Arn")
                     <*> (x .:? "KeyState")
                     <*> (x .:? "AWSAccountId")
                     <*> (x .:? "KeyUsage")
                     <*> (x .:? "CreationDate")
                     <*> (x .:? "DeletionDate")
                     <*> (x .:? "Description")
                     <*> (x .: "KeyId"))

instance Hashable KeyMetadata where

instance NFData KeyMetadata where

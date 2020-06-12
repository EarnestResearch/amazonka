{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentDescription
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.DocumentDescription where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.DocumentFormat
import Network.AWS.SSM.Types.DocumentHashType
import Network.AWS.SSM.Types.DocumentParameter
import Network.AWS.SSM.Types.DocumentStatus
import Network.AWS.SSM.Types.DocumentType
import Network.AWS.SSM.Types.PlatformType
import Network.AWS.SSM.Types.Tag

-- | Describes a Systems Manager document. 
--
--
--
-- /See:/ 'documentDescription' smart constructor.
data DocumentDescription = DocumentDescription'{_dStatus
                                                :: !(Maybe DocumentStatus),
                                                _dDocumentType ::
                                                !(Maybe DocumentType),
                                                _dHash :: !(Maybe Text),
                                                _dSchemaVersion ::
                                                !(Maybe Text),
                                                _dSha1 :: !(Maybe Text),
                                                _dDefaultVersion ::
                                                !(Maybe Text),
                                                _dTargetType :: !(Maybe Text),
                                                _dOwner :: !(Maybe Text),
                                                _dPlatformTypes ::
                                                !(Maybe [PlatformType]),
                                                _dCreatedDate :: !(Maybe POSIX),
                                                _dDocumentFormat ::
                                                !(Maybe DocumentFormat),
                                                _dName :: !(Maybe Text),
                                                _dHashType ::
                                                !(Maybe DocumentHashType),
                                                _dParameters ::
                                                !(Maybe [DocumentParameter]),
                                                _dDocumentVersion ::
                                                !(Maybe Text),
                                                _dDescription :: !(Maybe Text),
                                                _dTags :: !(Maybe [Tag]),
                                                _dLatestVersion ::
                                                !(Maybe Text)}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DocumentDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dStatus' - The status of the Systems Manager document.
--
-- * 'dDocumentType' - The type of document. 
--
-- * 'dHash' - The Sha256 or Sha1 hash created by the system when the document was created. 
--
-- * 'dSchemaVersion' - The schema version.
--
-- * 'dSha1' - The SHA1 hash of the document, which you can use for verification.
--
-- * 'dDefaultVersion' - The default version.
--
-- * 'dTargetType' - The target type which defines the kinds of resources the document can run on. For example, /AWS::EC2::Instance. For a list of valid resource types, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the /AWS CloudFormation User Guide/ . 
--
-- * 'dOwner' - The AWS user account that created the document.
--
-- * 'dPlatformTypes' - The list of OS platforms compatible with this Systems Manager document. 
--
-- * 'dCreatedDate' - The date when the document was created.
--
-- * 'dDocumentFormat' - The document format, either JSON or YAML.
--
-- * 'dName' - The name of the Systems Manager document.
--
-- * 'dHashType' - Sha256 or Sha1.
--
-- * 'dParameters' - A description of the parameters for a document.
--
-- * 'dDocumentVersion' - The document version.
--
-- * 'dDescription' - A description of the document. 
--
-- * 'dTags' - The tags, or metadata, that have been applied to the document.
--
-- * 'dLatestVersion' - The latest version of the document.
documentDescription
    :: DocumentDescription
documentDescription
  = DocumentDescription'{_dStatus = Nothing,
                         _dDocumentType = Nothing, _dHash = Nothing,
                         _dSchemaVersion = Nothing, _dSha1 = Nothing,
                         _dDefaultVersion = Nothing, _dTargetType = Nothing,
                         _dOwner = Nothing, _dPlatformTypes = Nothing,
                         _dCreatedDate = Nothing, _dDocumentFormat = Nothing,
                         _dName = Nothing, _dHashType = Nothing,
                         _dParameters = Nothing, _dDocumentVersion = Nothing,
                         _dDescription = Nothing, _dTags = Nothing,
                         _dLatestVersion = Nothing}

-- | The status of the Systems Manager document.
dStatus :: Lens' DocumentDescription (Maybe DocumentStatus)
dStatus = lens _dStatus (\ s a -> s{_dStatus = a})

-- | The type of document. 
dDocumentType :: Lens' DocumentDescription (Maybe DocumentType)
dDocumentType = lens _dDocumentType (\ s a -> s{_dDocumentType = a})

-- | The Sha256 or Sha1 hash created by the system when the document was created. 
dHash :: Lens' DocumentDescription (Maybe Text)
dHash = lens _dHash (\ s a -> s{_dHash = a})

-- | The schema version.
dSchemaVersion :: Lens' DocumentDescription (Maybe Text)
dSchemaVersion = lens _dSchemaVersion (\ s a -> s{_dSchemaVersion = a})

-- | The SHA1 hash of the document, which you can use for verification.
dSha1 :: Lens' DocumentDescription (Maybe Text)
dSha1 = lens _dSha1 (\ s a -> s{_dSha1 = a})

-- | The default version.
dDefaultVersion :: Lens' DocumentDescription (Maybe Text)
dDefaultVersion = lens _dDefaultVersion (\ s a -> s{_dDefaultVersion = a})

-- | The target type which defines the kinds of resources the document can run on. For example, /AWS::EC2::Instance. For a list of valid resource types, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference> in the /AWS CloudFormation User Guide/ . 
dTargetType :: Lens' DocumentDescription (Maybe Text)
dTargetType = lens _dTargetType (\ s a -> s{_dTargetType = a})

-- | The AWS user account that created the document.
dOwner :: Lens' DocumentDescription (Maybe Text)
dOwner = lens _dOwner (\ s a -> s{_dOwner = a})

-- | The list of OS platforms compatible with this Systems Manager document. 
dPlatformTypes :: Lens' DocumentDescription [PlatformType]
dPlatformTypes = lens _dPlatformTypes (\ s a -> s{_dPlatformTypes = a}) . _Default . _Coerce

-- | The date when the document was created.
dCreatedDate :: Lens' DocumentDescription (Maybe UTCTime)
dCreatedDate = lens _dCreatedDate (\ s a -> s{_dCreatedDate = a}) . mapping _Time

-- | The document format, either JSON or YAML.
dDocumentFormat :: Lens' DocumentDescription (Maybe DocumentFormat)
dDocumentFormat = lens _dDocumentFormat (\ s a -> s{_dDocumentFormat = a})

-- | The name of the Systems Manager document.
dName :: Lens' DocumentDescription (Maybe Text)
dName = lens _dName (\ s a -> s{_dName = a})

-- | Sha256 or Sha1.
dHashType :: Lens' DocumentDescription (Maybe DocumentHashType)
dHashType = lens _dHashType (\ s a -> s{_dHashType = a})

-- | A description of the parameters for a document.
dParameters :: Lens' DocumentDescription [DocumentParameter]
dParameters = lens _dParameters (\ s a -> s{_dParameters = a}) . _Default . _Coerce

-- | The document version.
dDocumentVersion :: Lens' DocumentDescription (Maybe Text)
dDocumentVersion = lens _dDocumentVersion (\ s a -> s{_dDocumentVersion = a})

-- | A description of the document. 
dDescription :: Lens' DocumentDescription (Maybe Text)
dDescription = lens _dDescription (\ s a -> s{_dDescription = a})

-- | The tags, or metadata, that have been applied to the document.
dTags :: Lens' DocumentDescription [Tag]
dTags = lens _dTags (\ s a -> s{_dTags = a}) . _Default . _Coerce

-- | The latest version of the document.
dLatestVersion :: Lens' DocumentDescription (Maybe Text)
dLatestVersion = lens _dLatestVersion (\ s a -> s{_dLatestVersion = a})

instance FromJSON DocumentDescription where
        parseJSON
          = withObject "DocumentDescription"
              (\ x ->
                 DocumentDescription' <$>
                   (x .:? "Status") <*> (x .:? "DocumentType") <*>
                     (x .:? "Hash")
                     <*> (x .:? "SchemaVersion")
                     <*> (x .:? "Sha1")
                     <*> (x .:? "DefaultVersion")
                     <*> (x .:? "TargetType")
                     <*> (x .:? "Owner")
                     <*> (x .:? "PlatformTypes" .!= mempty)
                     <*> (x .:? "CreatedDate")
                     <*> (x .:? "DocumentFormat")
                     <*> (x .:? "Name")
                     <*> (x .:? "HashType")
                     <*> (x .:? "Parameters" .!= mempty)
                     <*> (x .:? "DocumentVersion")
                     <*> (x .:? "Description")
                     <*> (x .:? "Tags" .!= mempty)
                     <*> (x .:? "LatestVersion"))

instance Hashable DocumentDescription where

instance NFData DocumentDescription where

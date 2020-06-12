{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Bucket
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.Bucket where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | /See:/ 'bucket' smart constructor.
data Bucket = Bucket'{_bCreationDate :: !RFC822,
                      _bName :: !BucketName}
                deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Bucket' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bCreationDate' - Date the bucket was created.
--
-- * 'bName' - The name of the bucket.
bucket
    :: UTCTime -- ^ 'bCreationDate'
    -> BucketName -- ^ 'bName'
    -> Bucket
bucket pCreationDate_ pName_
  = Bucket'{_bCreationDate = _Time # pCreationDate_,
            _bName = pName_}

-- | Date the bucket was created.
bCreationDate :: Lens' Bucket UTCTime
bCreationDate = lens _bCreationDate (\ s a -> s{_bCreationDate = a}) . _Time

-- | The name of the bucket.
bName :: Lens' Bucket BucketName
bName = lens _bName (\ s a -> s{_bName = a})

instance FromXML Bucket where
        parseXML x
          = Bucket' <$> (x .@ "CreationDate") <*> (x .@ "Name")

instance Hashable Bucket where

instance NFData Bucket where

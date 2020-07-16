{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteProvisioningTemplateVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a fleet provisioning template version.
--
--
module Network.AWS.IoT.DeleteProvisioningTemplateVersion
    (
    -- * Creating a Request
      deleteProvisioningTemplateVersion
    , DeleteProvisioningTemplateVersion
    -- * Request Lenses
    , delTemplateName
    , delVersionId

    -- * Destructuring the Response
    , deleteProvisioningTemplateVersionResponse
    , DeleteProvisioningTemplateVersionResponse
    -- * Response Lenses
    , dltprvsnngtmpltvrsnrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteProvisioningTemplateVersion' smart constructor.
data DeleteProvisioningTemplateVersion = DeleteProvisioningTemplateVersion'{_delTemplateName
                                                                            ::
                                                                            !Text,
                                                                            _delVersionId
                                                                            ::
                                                                            !Int}
                                           deriving (Eq, Read, Show, Data,
                                                     Typeable, Generic)

-- | Creates a value of 'DeleteProvisioningTemplateVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delTemplateName' - The name of the fleet provisioning template version to delete.
--
-- * 'delVersionId' - The fleet provisioning template version ID to delete.
deleteProvisioningTemplateVersion
    :: Text -- ^ 'delTemplateName'
    -> Int -- ^ 'delVersionId'
    -> DeleteProvisioningTemplateVersion
deleteProvisioningTemplateVersion pTemplateName_
  pVersionId_
  = DeleteProvisioningTemplateVersion'{_delTemplateName
                                         = pTemplateName_,
                                       _delVersionId = pVersionId_}

-- | The name of the fleet provisioning template version to delete.
delTemplateName :: Lens' DeleteProvisioningTemplateVersion Text
delTemplateName = lens _delTemplateName (\ s a -> s{_delTemplateName = a})

-- | The fleet provisioning template version ID to delete.
delVersionId :: Lens' DeleteProvisioningTemplateVersion Int
delVersionId = lens _delVersionId (\ s a -> s{_delVersionId = a})

instance AWSRequest DeleteProvisioningTemplateVersion
         where
        type Rs DeleteProvisioningTemplateVersion =
             DeleteProvisioningTemplateVersionResponse
        request = delete ioT
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteProvisioningTemplateVersionResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteProvisioningTemplateVersion
         where

instance NFData DeleteProvisioningTemplateVersion
         where

instance ToHeaders DeleteProvisioningTemplateVersion
         where
        toHeaders = const mempty

instance ToPath DeleteProvisioningTemplateVersion
         where
        toPath DeleteProvisioningTemplateVersion'{..}
          = mconcat
              ["/provisioning-templates/", toBS _delTemplateName,
               "/versions/", toBS _delVersionId]

instance ToQuery DeleteProvisioningTemplateVersion
         where
        toQuery = const mempty

-- | /See:/ 'deleteProvisioningTemplateVersionResponse' smart constructor.
newtype DeleteProvisioningTemplateVersionResponse = DeleteProvisioningTemplateVersionResponse'{_dltprvsnngtmpltvrsnrsResponseStatus
                                                                                               ::
                                                                                               Int}
                                                      deriving (Eq, Read, Show,
                                                                Data, Typeable,
                                                                Generic)

-- | Creates a value of 'DeleteProvisioningTemplateVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dltprvsnngtmpltvrsnrsResponseStatus' - -- | The response status code.
deleteProvisioningTemplateVersionResponse
    :: Int -- ^ 'dltprvsnngtmpltvrsnrsResponseStatus'
    -> DeleteProvisioningTemplateVersionResponse
deleteProvisioningTemplateVersionResponse
  pResponseStatus_
  = DeleteProvisioningTemplateVersionResponse'{_dltprvsnngtmpltvrsnrsResponseStatus
                                                 = pResponseStatus_}

-- | -- | The response status code.
dltprvsnngtmpltvrsnrsResponseStatus :: Lens' DeleteProvisioningTemplateVersionResponse Int
dltprvsnngtmpltvrsnrsResponseStatus = lens _dltprvsnngtmpltvrsnrsResponseStatus (\ s a -> s{_dltprvsnngtmpltvrsnrsResponseStatus = a})

instance NFData
           DeleteProvisioningTemplateVersionResponse
         where

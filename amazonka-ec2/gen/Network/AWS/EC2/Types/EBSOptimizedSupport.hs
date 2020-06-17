{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EBSOptimizedSupport
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.EBSOptimizedSupport (
  EBSOptimizedSupport (
    ..
    , EOSDefault
    , EOSSupported
    , EOSUnsupported
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data EBSOptimizedSupport = EBSOptimizedSupport' (CI
                                                   Text)
                             deriving (Eq, Ord, Read, Show, Data, Typeable,
                                       Generic)

pattern EOSDefault :: EBSOptimizedSupport
pattern EOSDefault = EBSOptimizedSupport' "default"

pattern EOSSupported :: EBSOptimizedSupport
pattern EOSSupported = EBSOptimizedSupport' "supported"

pattern EOSUnsupported :: EBSOptimizedSupport
pattern EOSUnsupported = EBSOptimizedSupport' "unsupported"

{-# COMPLETE
  EOSDefault,
  EOSSupported,
  EOSUnsupported,
  EBSOptimizedSupport' #-}

instance FromText EBSOptimizedSupport where
    parser = (EBSOptimizedSupport' . mk) <$> takeText

instance ToText EBSOptimizedSupport where
    toText (EBSOptimizedSupport' ci) = original ci

-- | Represents an enum of /known/ $EBSOptimizedSupport.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum EBSOptimizedSupport where
    toEnum i = case i of
        0 -> EOSDefault
        1 -> EOSSupported
        2 -> EOSUnsupported
        _ -> (error . showText) $ "Unknown index for EBSOptimizedSupport: " <> toText i
    fromEnum x = case x of
        EOSDefault -> 0
        EOSSupported -> 1
        EOSUnsupported -> 2
        EBSOptimizedSupport' name -> (error . showText) $ "Unknown EBSOptimizedSupport: " <> original name

-- | Represents the bounds of /known/ $EBSOptimizedSupport.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded EBSOptimizedSupport where
    minBound = EOSDefault
    maxBound = EOSUnsupported

instance Hashable     EBSOptimizedSupport
instance NFData       EBSOptimizedSupport
instance ToByteString EBSOptimizedSupport
instance ToQuery      EBSOptimizedSupport
instance ToHeader     EBSOptimizedSupport

instance FromXML EBSOptimizedSupport where
    parseXML = parseXMLText "EBSOptimizedSupport"

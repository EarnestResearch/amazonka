{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.Transferable
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53Domains.Types.Transferable (
  Transferable (
    ..
    , DontKnow
    , Transferable
    , Untransferable
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | Whether the domain name can be transferred to Route 53.
--
--
-- Valid values:
--
--     * TRANSFERABLE    * The domain name can be transferred to Route 53.
--
--     * UNTRANSFERRABLE    * The domain name can't be transferred to Route 53.
--
--     * DONT_KNOW    * Reserved for future use.
--
--
--
data Transferable = Transferable' (CI Text)
                      deriving (Eq, Ord, Read, Show, Data, Typeable,
                                Generic)

pattern DontKnow :: Transferable
pattern DontKnow = Transferable' "DONT_KNOW"

pattern Transferable :: Transferable
pattern Transferable = Transferable' "TRANSFERABLE"

pattern Untransferable :: Transferable
pattern Untransferable = Transferable' "UNTRANSFERABLE"

{-# COMPLETE
  DontKnow,
  Transferable,
  Untransferable,
  Transferable' #-}

instance FromText Transferable where
    parser = (Transferable' . mk) <$> takeText

instance ToText Transferable where
    toText (Transferable' ci) = original ci

-- | Represents an enum of /known/ $Transferable.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum Transferable where
    toEnum i = case i of
        0 -> DontKnow
        1 -> Transferable
        2 -> Untransferable
        _ -> (error . showText) $ "Unknown index for Transferable: " <> toText i
    fromEnum x = case x of
        DontKnow -> 0
        Transferable -> 1
        Untransferable -> 2
        Transferable' name -> (error . showText) $ "Unknown Transferable: " <> original name

-- | Represents the bounds of /known/ $Transferable.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded Transferable where
    minBound = DontKnow
    maxBound = Untransferable

instance Hashable     Transferable
instance NFData       Transferable
instance ToByteString Transferable
instance ToQuery      Transferable
instance ToHeader     Transferable

instance FromJSON Transferable where
    parseJSON = parseJSONText "Transferable"

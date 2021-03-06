{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Gen.Import
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.Import where

import Control.Lens

import Data.List   (sort)
import Data.Maybe

import Gen.Types

import qualified Data.Set as Set

operationImports :: Library -> Operation Identity SData a -> [NS]
operationImports l o = sort $
      "Network.AWS.Request"
    : "Network.AWS.Response"
    : "Network.AWS.Lens"
    : "Network.AWS.Prelude"
    : l ^. typesNS
    : l ^. productNS
    : l ^. operationModules
   ++ maybeToList (const "Network.AWS.Pager" <$> o ^. opPager)

typeImports :: Library -> [NS]
typeImports l = sort $
      "Network.AWS.Lens"
    : "Network.AWS.Prelude"
    : signatureImport (l ^. signatureVersion)
    : l ^. typeModules

sum1Imports :: Library -> [NS]
sum1Imports l = sort $
      "Data.CaseInsensitive"
    : "Network.AWS.Prelude"
    : l ^. typeModules

productImports :: Library -> [NS]
productImports l = sort $
      "Network.AWS.Lens"
    : "Network.AWS.Prelude"
    : l ^. typeModules
   <> productMods
  where
    productMods = (l ^. typesNS <>) . mkNS . typeId . identifier <$> products

    products = filter isProduct $ l ^.. shapes . each

    isProduct (Prod _ _ _) = True
    isProduct _            = False

product1Imports :: Library -> Prod -> [NS]
product1Imports l p = sort $
      "Network.AWS.Lens"
    : "Network.AWS.Prelude"
    : l ^. typeModules
   ++ (Set.toList $ Set.map (l ^. typesNS <>) moduleDependencies)
  where
    moduleDependencies = Set.intersection dependencies moduleShapes
    dependencies = Set.map mkNS $ _prodDeps p
    moduleShapes = Set.fromList (mkNS . typeId . identifier <$> l ^.. shapes . each)


waiterImports :: Library -> [NS]
waiterImports l = sort $
      "Network.AWS.Lens"
    : "Network.AWS.Prelude"
    : "Network.AWS.Waiter"
    : l ^. typesNS
    : map (operationNS ns . _waitOpName) (l ^.. waiters . each)
  where
    ns = l ^. libraryNS

signatureImport :: Signature -> NS
signatureImport = \case
    V2 -> "Network.AWS.Sign.V2"
    _  -> "Network.AWS.Sign.V4"

testImports :: Library -> [NS]
testImports l =
    [ mkNS $ "Test.AWS." <> l ^. serviceAbbrev
    , mkNS $ "Test.AWS." <> l ^. serviceAbbrev <> ".Internal"
    ]

fixtureImports :: Library -> [NS]
fixtureImports l =
    [ l ^. libraryNS
    , mkNS $ "Test.AWS." <> l ^. serviceAbbrev <> ".Internal"
    ]

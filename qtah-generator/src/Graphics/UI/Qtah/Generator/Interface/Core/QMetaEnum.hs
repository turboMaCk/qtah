-- This file is part of Qtah.
--
-- Copyright 2015-2019 The Qtah Authors.
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Graphics.UI.Qtah.Generator.Interface.Core.QMetaEnum (
  aModule,
  c_QMetaEnum,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetConversionToGc,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkStaticMethod,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Copyable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (boolT, intT, objT, ptrT, refT, voidT, constT, charT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QMetaEnum"]
  [ QtExport $ ExportClass c_QMetaEnum ]

c_QMetaEnum =
  addReqIncludes [includeStd "QMetaEnum"] $
  classSetConversionToGc $
  classAddFeatures [Copyable] $
  classSetEntityPrefix "" $
  makeClass (ident "QMetaEnum") Nothing [] $
  collect
  [ test (qtVersion >= [5, 12]) $ mkConstMethod "enumName" [] $ ptrT $ constT charT
  , just $ mkConstMethod "isFlag" [] boolT
  , test (qtVersion >= [5, 8]) $ mkConstMethod "isScoped" [] boolT
  , just $ mkConstMethod "isValid" [] boolT
  , just $ mkConstMethod "key" [intT] $ ptrT $ constT charT
  , just $ mkConstMethod "keyCount" [] intT
  , just $ mkConstMethod' "keyToValue" "keyToValue" [ptrT $ constT charT] intT 
  , just $ mkConstMethod' "keyToValue" "keyToValueWithBool" [ptrT $ constT charT, ptrT boolT] intT 
  , just $ mkConstMethod' "keysToValue" "keysToValue" [ptrT $ constT charT] intT 
  , just $ mkConstMethod' "keysToValue" "keysToValueWithBool" [ptrT $ constT charT, ptrT boolT] intT 
  , just $ mkConstMethod "name" [] $ ptrT $ constT charT
  , just $ mkConstMethod "scope" [] $ ptrT $ constT charT
  , just $ mkConstMethod "value" [intT] intT
  , just $ mkConstMethod "valueToKey" [intT] $ ptrT $ constT charT
  , just $ mkConstMethod "valueToKeys" [intT] $ objT c_QByteArray
  ]

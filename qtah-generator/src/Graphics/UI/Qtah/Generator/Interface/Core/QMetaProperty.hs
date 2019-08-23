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

module Graphics.UI.Qtah.Generator.Interface.Core.QMetaProperty (
  aModule,
  c_QMetaProperty,
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
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Copyable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (voidT, boolT, intT, objT, ptrT, refT, voidT, enumT, constT, charT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QMetaEnum (c_QMetaEnum)
import Graphics.UI.Qtah.Generator.Interface.Core.QMetaMethod (c_QMetaMethod)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QVariant (c_QVariant, e_Type)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QMetaProperty"]
  [ QtExport $ ExportClass c_QMetaProperty ]

c_QMetaProperty =
  addReqIncludes [includeStd "QMetaProperty"] $
  classSetConversionToGc $
  classAddFeatures [Copyable] $
  classSetEntityPrefix "" $
  makeClass (ident "QMetaProperty") Nothing [] $
  collect
  [ just $ mkConstMethod "enumerator" [] $ objT c_QMetaEnum
  , just $ mkConstMethod "hasNotifySignal" [] boolT
  , test (qtVersion >= [4, 6]) $ mkConstMethod "isConstant" [] boolT
  , just $ mkConstMethod' "isDesignable" "isDesignable" [] boolT
  , just $ mkConstMethod' "isDesignable" "isDesignableWithObject" [ptrT $ constT $ objT c_QObject] boolT
  , just $ mkConstMethod "isEnumType" [] boolT
  , test (qtVersion >= [4, 6]) $ mkConstMethod "isFinal" [] boolT
  , just $ mkConstMethod "isFlagType" [] boolT
  , just $ mkConstMethod "isReadable" [] boolT
  , just $ mkConstMethod "isResettable" [] boolT
  , just $ mkConstMethod' "isScriptable" "isScriptable" [] boolT
  , just $ mkConstMethod' "isScriptable" "isScriptableWithObject" [ptrT $ constT $ objT c_QObject] boolT
  , just $ mkConstMethod' "isStored" "isStored" [] boolT
  , just $ mkConstMethod' "isStored" "isStoredWithObject" [ptrT $ constT $ objT c_QObject] boolT
  , just $ mkConstMethod' "isUser" "isUser" [] boolT
  , just $ mkConstMethod' "isUser" "isUserWithObject" [ptrT $ constT $ objT c_QObject] boolT
  , just $ mkConstMethod "isValid" [] boolT
  , just $ mkConstMethod "isWritable" [] boolT
  , just $ mkConstMethod "name" [] $ ptrT $ constT charT
  , test (qtVersion >= [4, 5]) $ mkConstMethod "notifySignal" [] $ objT c_QMetaMethod
  , test (qtVersion >= [4, 6]) $ mkConstMethod "notifySignalIndex" [] intT
  , test (qtVersion >= [4, 6]) $ mkConstMethod "propertyIndex" [] intT
  , just $ mkConstMethod "read" [ptrT $ constT $ objT c_QObject] $ objT c_QVariant
  , test (qtVersion >= [5, 5]) $ mkConstMethod "readOnGadget" [ptrT $ constT voidT] $ objT c_QVariant
  , just $ mkConstMethod "reset" [ptrT $ objT c_QObject] boolT
  , test (qtVersion >= [5, 5]) $ mkConstMethod "resetOnGadget" [ptrT voidT] boolT
  , test (qtVersion >= [5, 1]) $ mkConstMethod "revision" [] intT
  , just $ mkConstMethod' "type" "typeMetaProperty" [] $ enumT e_Type
  , just $ mkConstMethod "typeName" [] $ ptrT $ constT charT
  , test (qtVersion >= [4, 2]) $ mkConstMethod "userType" [] intT
  , just $ mkConstMethod "write" [ptrT $ objT c_QObject, refT $ constT $ objT c_QVariant] boolT
  , test (qtVersion >= [5, 5]) $ mkConstMethod "writeOnGadget" [ptrT voidT, refT $ constT $ objT c_QVariant] boolT
  ]

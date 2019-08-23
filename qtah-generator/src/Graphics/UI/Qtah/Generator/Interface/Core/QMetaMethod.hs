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

module Graphics.UI.Qtah.Generator.Interface.Core.QMetaMethod (
  aModule,
  c_QMetaMethod,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportEnum),
  Operator (OpNe, OpEq),
  addReqIncludes,
  classSetConversionToGc,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkMethod,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Copyable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (boolT, intT, objT, ptrT, refT, voidT, enumT, constT, charT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QList (c_QListQByteArray)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QMetaMethod"]
  [ QtExport $ ExportClass c_QMetaMethod
  , QtExport $ ExportEnum e_Access
  , QtExport $ ExportEnum e_MethodType ]

c_QMetaMethod =
  addReqIncludes [includeStd "QMetaMethod"] $
  classSetConversionToGc $
  classAddFeatures [Copyable] $
  classSetEntityPrefix "" $
  makeClass (ident "QMetaMethod") Nothing [] $
  collect
  [ just $ mkConstMethod "access" [] $ enumT e_Access
  , test (qtVersion >= [5, 0]) $ mkConstMethod "isValid" [] boolT
  , test (qtVersion >= [4, 6]) $ mkConstMethod "methodIndex" [] intT
  , test (qtVersion >= [5, 0]) $ mkConstMethod "methodSignature" [] $ objT c_QByteArray
  , just $ mkConstMethod "methodType" [] $ enumT e_MethodType
  , test (qtVersion >= [5, 0]) $ mkConstMethod "name" [] $ objT c_QByteArray
  , test (qtVersion >= [5, 0]) $ mkConstMethod "parameterCount" [] intT
  , just $ mkConstMethod "parameterNames" [] $ objT c_QListQByteArray
  , test (qtVersion >= [5, 0]) $ mkConstMethod "parameterType" [intT] intT
  , just $ mkConstMethod "parameterTypes" [] $ objT c_QListQByteArray
  , test (qtVersion >= [5, 0]) $ mkConstMethod "returnType" [] intT
  , test (qtVersion >= [5, 1]) $ mkConstMethod "revision" [] intT
  , just $ mkConstMethod "tag" [] $ ptrT $ constT charT
  , just $ mkConstMethod "typeName" [] $ ptrT $ constT charT
  , test (qtVersion >= [5, 0]) $ mkMethod OpNe [refT $ constT $ objT c_QMetaMethod] boolT
  , test (qtVersion >= [5, 0]) $ mkMethod OpEq [refT $ constT $ objT c_QMetaMethod] boolT

  -- TODO invoke methods
  ]

e_Access =
  makeQtEnum (ident1 "QMetaMethod" "Access") [includeStd "QMetaMethod"]
  [ (0, ["private"])
  , (1, ["protected"])
  , (2, ["public"])
  ]

e_MethodType =
  makeQtEnum (ident1 "QMetaMethod" "MethodType") [includeStd "QMetaMethod"]
  [ (0, ["method"])
  , (1, ["signal"])
  , (2, ["slot"])
  , (3, ["constructor"])
  ]
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

module Graphics.UI.Qtah.Generator.Interface.Core.QBuffer (
  aModule,
  c_QBuffer,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod',
  mkCtor,
  mkMethod',
  mkMethod
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QIODevice (c_QIODevice)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Foreign.Hoppy.Generator.Types (intT, charT, voidT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QBuffer"]
  [ QtExport $ ExportClass c_QBuffer ]

c_QBuffer =
  addReqIncludes [ includeStd "QBuffer" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QBuffer") Nothing [c_QIODevice] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QObject]
  , just $ mkCtor "newWithByteArray" [ptrT $ objT c_QByteArray]
  , just $ mkCtor "newWithByteArrayAndParent" [ptrT $ objT c_QByteArray, ptrT $ objT c_QObject]
  , just $ mkMethod' "buffer" "buffer" [] $ refT $ objT c_QByteArray
  , just $ mkConstMethod' "buffer" "bufferConst" [] $ refT $ constT $ objT c_QByteArray
  , just $ mkConstMethod' "data" "getData" [] $ refT $ constT $ objT c_QByteArray
  , just $ mkMethod "setBuffer" [ptrT $ objT c_QByteArray] voidT
  , just $ mkMethod' "setData" "setDataByteArray" [refT $ constT $ objT c_QByteArray] voidT
  , just $ mkMethod' "setData" "setDataRaw" [ptrT $ constT charT, intT] voidT
  ]
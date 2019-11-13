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

module Graphics.UI.Qtah.Generator.Interface.Core.QXmlStreamAttributes (
  aModule,
  c_QXmlStreamAttributes,
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
  )
--import Graphics.UI.Qtah.Generator.Interface.Core.QStringRef (c_QStringRef)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QLatin1String (c_QLatin1String)
import Graphics.UI.Qtah.Generator.Interface.Core.QVector (c_QVectorQXmlStreamAttribute)
import Foreign.Hoppy.Generator.Types (boolT, voidT, constT, objT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QXmlStreamAttributes"] [4, 3] $
  [QtExport $ ExportClass c_QXmlStreamAttributes]

c_QXmlStreamAttributes =
  addReqIncludes [ includeStd "QXmlStreamAttributes" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QXmlStreamAttributes") Nothing [c_QVectorQXmlStreamAttribute] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkMethod' "append" "appendWithURI" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "append" "append" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] voidT
  , test (qtVersion >= [4, 5]) $ mkConstMethod' "hasAttribute" "hasAttributeWithURI" [refT $ constT $ objT c_QString] boolT
  , test (qtVersion >= [4, 5]) $ mkConstMethod' "hasAttribute" "hasAttribute" [objT c_QLatin1String] boolT
  , test (qtVersion >= [4, 5]) $ mkConstMethod' "hasAttribute" "hasAttributeWithURIName" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] boolT
  --, just $ mkConstMethod' "value" "valueWithStrings" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] $ objT c_QStringRef
  --, just $ mkConstMethod' "value" "valueWithStringLatin" [refT $ constT $ objT c_QString, objT c_QLatin1String] $ objT c_QStringRef
  --, just $ mkConstMethod' "value" "valueWithLatins" [objT c_QLatin1String, objT c_QLatin1String] $ objT c_QStringRef
  --, just $ mkConstMethod' "value" "valueWithQualname" [refT $ constT $ objT c_QString] $ objT c_QStringRef
  --, just $ mkConstMethod' "value" "valueWithLatinQualname" [objT c_QLatin1String] $ objT c_QStringRef
  ]

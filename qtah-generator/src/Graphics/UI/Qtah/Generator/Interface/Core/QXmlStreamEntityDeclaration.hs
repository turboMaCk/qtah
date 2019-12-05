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

module Graphics.UI.Qtah.Generator.Interface.Core.QXmlStreamEntityDeclaration (
  aModule,
  c_QXmlStreamEntityDeclaration,
  qXmlStreamEntityDeclarations,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  classSetConversionToGc,
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkCtor,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Copyable, Assignable, Equatable),
  classAddFeatures,
  )
--import Graphics.UI.Qtah.Generator.Interface.Core.QStringRef (c_QStringRef)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QVector (c_QVectorQXmlStreamEntityDeclaration)
--import Foreign.Hoppy.Generator.Types (objT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QXmlStreamEntityDeclaration"]
  [QtExport $ ExportClass c_QXmlStreamEntityDeclaration]

c_QXmlStreamEntityDeclaration =
  addReqIncludes [ includeStd "QXmlStreamEntityDeclaration" ] $
  classSetConversionToGc $
  classAddFeatures [Copyable, Assignable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QXmlStreamEntityDeclaration") Nothing [] $
  collect
  [ just $ mkCtor "new" []
  --, just $ mkConstMethod "name" [] $ objT c_QStringRef
  --, just $ mkConstMethod "notationName" [] $ objT c_QStringRef
  --, just $ mkConstMethod "publicId" [] $ objT c_QStringRef
  --, just $ mkConstMethod "systemId" [] $ objT c_QStringRef
  --, just $ mkConstMethod "value" [] $ objT c_QStringRef
  ]

qXmlStreamEntityDeclarations = c_QVectorQXmlStreamEntityDeclaration

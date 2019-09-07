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

module Graphics.UI.Qtah.Generator.Interface.Core.QLibrary (
  aModule,
  c_QLibrary,
  e_LoadHint,
  bs_LoadHints,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportBitspace, ExportEnum),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkStaticMethod,
  mkStaticMethod',
  mkCtor,
  mkMethod',
  mkMethod,
  mkProp
  )
import Foreign.Hoppy.Generator.Types (charT, intT, boolT, voidT, bitspaceT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qfunctionpointer)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QLibrary"] $
  collect
  [ just $ QtExport $ ExportClass c_QLibrary
  , just $ QtExport $ ExportEnum e_LoadHint
  , just $ QtExport $ ExportBitspace bs_LoadHints
  ]

c_QLibrary =
  addReqIncludes [ includeStd "QLibrary" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QLibrary") Nothing [c_QObject] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QObject]
  , just $ mkCtor "newWithFilename" [refT $ constT $ objT c_QString]
  , just $ mkCtor "newWithFilenameAndParent" [refT $ constT $ objT c_QString, ptrT $ objT c_QObject]
  , just $ mkCtor "newWithFilenameAndMajorVersion" [refT $ constT $ objT c_QString, intT]
  , just $ mkCtor "newWithFilenameAndMajorVersionAndParent" [refT $ constT $ objT c_QString, intT, ptrT $ objT c_QObject]
  , just $ mkCtor "newWithFilenameAndVersion" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString]
  , just $ mkCtor "newWithFilenameAndVersionAndParent" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString, ptrT $ objT c_QObject]
  , test (qtVersion >= [4, 2]) $ mkConstMethod "errorString" [] $ objT c_QString
  , just $ mkStaticMethod "isLibrary" [refT $ constT $ objT c_QString] boolT
  , just $ mkConstMethod "isLoaded" [] boolT
  , just $ mkMethod "load" [] boolT
  , -- TODO Make these functions take QStrings.
    just $ mkMethod' "resolve" "resolve" [ptrT $ constT $ charT] qfunctionpointer
  , just $ mkStaticMethod' "resolve" "resolveStatic" [refT $ constT $ objT c_QString, ptrT $ constT charT] qfunctionpointer
  , just $ mkStaticMethod' "resolve" "resolveStaticWithMajorVersion" [refT $ constT $ objT c_QString, intT, ptrT $ constT charT] qfunctionpointer
  , test (qtVersion >= [4, 4]) $ mkStaticMethod' "resolve" "resolveStaticWithVersion" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString, ptrT $ constT charT] qfunctionpointer
  , just $ mkMethod' "setFileNameAndVersion" "setFileNameAndMajorVersion" [refT $ constT $ objT c_QString, intT] voidT
  , test (qtVersion >= [4, 4]) $ mkMethod' "setFileNameAndVersion" "setFileNameAndVersion" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] voidT
  , just $ mkMethod "unload" [] boolT
  , just $ mkProp "fileName" $ objT c_QString
  , just $ mkProp "loadHints" $ bitspaceT bs_LoadHints
  ]

(e_LoadHint, bs_LoadHints) =
  makeQtEnumBitspace (ident1 "QLibrary" "LoadHint") "LoadHints" [includeStd "QLibrary"]
  [ (0x01, ["resolve", "all", "symbols", "hint"])
  , (0x02, ["export", "external", "symbols", "hint"])
  , (0x04, ["load", "archive", "member", "hint"])
  , (0x08, ["prevent", "unload", "hint"])
  , (0x10, ["deep", "bind", "hint"])
  ]

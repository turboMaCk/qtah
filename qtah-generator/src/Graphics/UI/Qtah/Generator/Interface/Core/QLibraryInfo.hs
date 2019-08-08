module Graphics.UI.Qtah.Generator.Interface.Core.QLibraryInfo (
  aModule,
  c_QLibraryInfo,
  e_LibraryLocation,
  ) where


import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportEnum),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  ident2,
  includeLocal,
  includeStd,
  makeClass,
  makeFnMethod,
  mkConstMethod,
  mkConstMethod',
  mkStaticMethod,
  mkStaticMethod',
  mkCtor,
  mkMethod',
  mkMethod
  )
  

import Foreign.Hoppy.Generator.Types (boolT, voidT, enumT, bitspaceT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QVersionNumber (c_QVersionNumber)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}



aModule =
  AQtModule $
  makeQtModule ["Core", "QLibraryInfo"] $
  collect
  [ just $ QtExport $ ExportClass c_QLibraryInfo
  , just $ QtExport $ ExportEnum e_LibraryLocation
  ]


c_QLibraryInfo =
  addReqIncludes [ includeStd "QLibraryInfo" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QLibraryInfo") Nothing [] $
  collect
  [ test (qtVersion >= [5, 0]) $ mkStaticMethod "isDebugBuild" [] $ boolT
  , just $ mkStaticMethod "location" [enumT e_LibraryLocation] $ objT c_QString
  , test (qtVersion >= [5, 8]) $ mkStaticMethod "version" [] $ objT c_QVersionNumber
  ]
  

e_LibraryLocation =
  makeQtEnum (ident1 "QLibraryInfo" "LibraryLocation") [includeStd "QLibraryInfo"]
  [ (0, ["prefix", "path"])
  , (1, ["documentation", "path"])
  , (2, ["headers", "path"])
  , (3, ["libraries", "path"])
  , (4, ["library", "executables", "path"])
  , (5, ["binaries", "path"])
  , (6, ["plugins", "path"])
  , (7, ["imports", "path"])
  , (8, ["qml2", "imports", "path"])
  , (9, ["arch", "data", "path"])
  , (10, ["data", "path"])
  , (11, ["translations", "path"])
  , (12, ["examples", "path"])
  , (13, ["tests", "path"])
  , (100, ["settings", "path"])
  ]
  
  
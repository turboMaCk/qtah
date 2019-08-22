module Graphics.UI.Qtah.Generator.Interface.Core.QStandardPaths (
  aModule,
  c_QStandardPaths,
  e_StandardLocation,
  e_LocateOption,
  bs_LocateOptions,
  ) where


import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportBitspace, ExportEnum),
  addReqIncludes,
  classSetEntityPrefix,
  classSetDtorPrivate,
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
  
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Foreign.Hoppy.Generator.Types (boolT, voidT, enumT, bitspaceT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}



aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QStandardPaths"] [5, 0] $
  collect
  [ just $ QtExport $ ExportClass c_QStandardPaths
  , just $ QtExport $ ExportEnum e_StandardLocation
  , just $ QtExport $ ExportEnum e_LocateOption
  , just $ QtExport $ ExportBitspace bs_LocateOptions
  ]


c_QStandardPaths =
  addReqIncludes [ includeStd "QStandardPaths" ] $
  classSetEntityPrefix "" $
  classSetDtorPrivate $ 
  makeClass (ident "QStandardPaths") Nothing [] $
  collect
  [just $ mkStaticMethod "displayName" [enumT e_StandardLocation] $ objT c_QString
  , just $ mkStaticMethod' "findExecutable" "findExecutable" [constT $ objT c_QString ] $ objT c_QString
  , just $ mkStaticMethod' "findExecutable" "findExecutableWithPaths" [constT $ objT c_QString, constT $ objT c_QStringList] $ objT c_QString
  , just $ mkStaticMethod' "locate" "locate" [enumT e_StandardLocation, constT $ objT c_QString ] $ objT c_QString
  , just $ mkStaticMethod' "locate" "locateWithOptions" [enumT e_StandardLocation, constT $ objT c_QString, bitspaceT bs_LocateOptions] $ objT c_QString
  , just $ mkStaticMethod' "locateAll" "locateAll" [enumT e_StandardLocation, constT $ objT c_QString ] $ objT c_QStringList
  , just $ mkStaticMethod' "locateAll" "locateAllWithOptions" [enumT e_StandardLocation, constT $ objT c_QString, bitspaceT bs_LocateOptions] $ objT c_QStringList
  , just $ mkStaticMethod "setTestModeEnabled" [boolT] voidT
  , just $ mkStaticMethod "standardLocations" [enumT e_StandardLocation] $ objT c_QStringList
  , just $ mkStaticMethod "writableLocation" [enumT e_StandardLocation] $ objT c_QString
  ]
  
  
e_StandardLocation =
  makeQtEnum (ident1 "QStandardPaths" "StandardLocation") [includeStd "QStandardPaths"] $
  collect
  [just (0, ["desktop", "location"])
  ,just (1, ["documents", "location"])
  ,just (2, ["fonts", "location"])
  ,just (3, ["applications", "location"])
  ,just (4, ["music", "location"])
  ,just (5, ["movies", "location"])
  ,just (6, ["pictures", "location"])
  ,just (7, ["temp", "location"])
  ,just (8, ["home", "location"])
  ,just (9, ["data", "location"])
  ,just (10, ["cache", "location"])
  ,just (11, ["generic", "data", "location"])
  ,just (12, ["runtime", "location"])
  ,just (13, ["config", "location"])
  ,just (14, ["download", "location"])
  ,just (15, ["generic", "cache", "location"])
  ,just (16, ["generic", "config", "location"])
  ,test (qtVersion >= [5, 4]) $ (17, ["app", "data", "location"])
  ,test (qtVersion >= [5, 5]) $ (18, ["app", "config", "location"])
  ]
  
  
(e_LocateOption, bs_LocateOptions) =
  makeQtEnumBitspace (ident1 "QStandardPaths" "LocateOption") "LocateOptions" [includeStd "QStandardPaths"]
  [ (0x0, ["locate", "file"])
  , (0x1, ["locate", "directory"])
  ]
  
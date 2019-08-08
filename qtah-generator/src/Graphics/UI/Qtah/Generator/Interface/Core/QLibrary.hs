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
  mkMethod,
  mkProp
  )

import Foreign.Hoppy.Generator.Types (charT, intT, boolT, voidT, enumT, bitspaceT, constT, objT, ptrT, refT)
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
  , just $ mkCtor "newWithFilenameParent" [refT $ constT $ objT c_QString, ptrT $ objT c_QObject]
  , just $ mkCtor "newWithFilenameVer" [refT $ constT $ objT c_QString, intT]
  , just $ mkCtor "newWithFilenameVerParent" [refT $ constT $ objT c_QString, intT, ptrT $ objT c_QObject]
  , just $ mkCtor "newWithFilenameVerstr" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString]
  , just $ mkCtor "newWithFilenameVerstrParent" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString, ptrT $ objT c_QObject]
  , test (qtVersion >= [4, 2]) $ mkConstMethod "errorString" [] $ objT c_QString
  , just $ mkStaticMethod "isLibrary" [refT $ constT $ objT c_QString] boolT
  , just $ mkConstMethod "isLoaded" [] boolT
  , just $ mkMethod "load" [] boolT
  , just $ mkMethod' "resolve" "resolve" [ptrT $ constT $ charT] qfunctionpointer
  , just $ mkStaticMethod' "resolve" "resolveWithFilenameSymbol" [refT $ constT $ objT c_QString, ptrT $ constT charT] qfunctionpointer
  , just $ mkStaticMethod' "resolve" "resolveWithFilenameVerSymbol" [refT $ constT $ objT c_QString, intT, ptrT $ constT charT] qfunctionpointer
  , test (qtVersion >= [4, 4]) $ mkStaticMethod' "resolve" "resolveWithFilenameVerstrSymbol" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString, ptrT $ constT charT] qfunctionpointer
  , just $ mkMethod' "setFileNameAndVersion" "setFileNameAndVersionVerint" [refT $ constT $ objT c_QString, intT] voidT
  , test (qtVersion >= [4, 4]) $ mkMethod' "setFileNameAndVersion" "setFileNameAndVersionVerstr" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] voidT
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

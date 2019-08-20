module Graphics.UI.Qtah.Generator.Interface.Core.QDirIterator (
  aModule,
  c_QDirIterator,
  e_IteratorFlag,
  bs_IteratorFlags,
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
  mkMethod
  )
  

import Foreign.Hoppy.Generator.Types (boolT, voidT, enumT, bitspaceT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
--import Graphics.UI.Qtah.Generator.Interface.Core.QFileInfo (c_QFileInfo)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Graphics.UI.Qtah.Generator.Interface.Core.QDir (c_QDir, bs_Filters)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types


{-# ANN module "HLint: ignore Use camelCase" #-}


aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QDirIterator"] [4, 3] $
  collect
  [ just $ QtExport $ ExportClass c_QDirIterator
  , just $ QtExport $ ExportEnum e_IteratorFlag
  , just $ QtExport $ ExportBitspace bs_IteratorFlags
  ]


c_QDirIterator =
  addReqIncludes [ includeStd "QDirIterator" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QDirIterator") Nothing [] $
  collect
  [ just $ mkCtor "new" [refT $ constT $ objT c_QDir]
  , just $ mkCtor "newWithDirFlags" [refT $ constT $ objT c_QDir, bitspaceT bs_IteratorFlags] 
  , just $ mkCtor "newWithStr" [refT $ constT $ objT c_QString]
  , just $ mkCtor "newWithStrFlags" [refT $ constT $ objT c_QString, bitspaceT bs_IteratorFlags]
  , just $ mkCtor "newWithStrFiltres" [refT $ constT $ objT c_QString, bitspaceT bs_Filters]
  , just $ mkCtor "newWithStrFiltersFlags" [refT $ constT $ objT c_QString, bitspaceT bs_Filters, bitspaceT bs_IteratorFlags]
  , just $ mkCtor "newWithStrStrList" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QStringList]
  , just $ mkCtor "newWithStrStrListFilters" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QStringList, bitspaceT bs_Filters]
  , just $ mkCtor "newWithStrStrListFiltersFlags" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QStringList, bitspaceT bs_Filters, bitspaceT bs_IteratorFlags]
  --, just $ mkConstMethod "fileInfo" [] $ objT c_QFileInfo
  , just $ mkConstMethod "fileName" [] $ objT c_QString
  , just $ mkConstMethod "filePath" [] $ objT c_QString
  , just $ mkConstMethod "hasNext" [] boolT
  , just $ mkMethod "next" [] $ objT c_QString
  , just $ mkConstMethod "path" [] $ objT c_QString
  ]
  
  
(e_IteratorFlag, bs_IteratorFlags) =
  makeQtEnumBitspace (ident1 "QDirIterator" "IteratorFlag") "IteratorFlags" [includeStd "QDirIterator"]
  [ (0x0, ["no", "iterator", "flags"])
  , (0x2, ["subdirectories"])
  , (0x1, ["follow", "symlinks"])
  ]
  

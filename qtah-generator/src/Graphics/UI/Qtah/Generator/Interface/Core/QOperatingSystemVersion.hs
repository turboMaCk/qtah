module Graphics.UI.Qtah.Generator.Interface.Core.QOperatingSystemVersion (
  aModule,
  c_QOperatingSystemVersion,
  e_OSType,
  ) where


import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportEnum),
  addReqIncludes,
  classSetConversionToGc,
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

import Foreign.Hoppy.Generator.Spec.ClassFeature (
    ClassFeature (Copyable),
    classAddFeatures,
    )  

import Foreign.Hoppy.Generator.Types (boolT, intT, voidT, enumT, bitspaceT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}



aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QOperatingSystemVersion"] [5, 9] $
  collect
  [ just $ QtExport $ ExportClass c_QOperatingSystemVersion
  , just $ QtExport $ ExportEnum e_OSType
  ]


c_QOperatingSystemVersion =
  addReqIncludes [ includeStd "QOperatingSystemVersion" ] $
  classSetConversionToGc $ 
  classAddFeatures [Copyable] $
  classSetEntityPrefix "" $
  makeClass (ident "QOperatingSystemVersion") Nothing [] $
  collect
  [ just $ mkCtor "new" [enumT e_OSType, intT]
  , just $ mkCtor "newWithVMinor" [enumT e_OSType, intT, intT]
  , just $ mkCtor "newWithVMinorVMicro" [enumT e_OSType, intT, intT, intT]
  , just $ mkStaticMethod "current" [] $ objT c_QOperatingSystemVersion
  , just $ mkStaticMethod "currentType" [] $ enumT e_OSType
  -- TODO bool QOperatingSystemVersion::isAnyOfType(std::initializer_list<OSType> types) const
  , just $ mkConstMethod "majorVersion" [] intT
  , just $ mkConstMethod "microVersion" [] intT
  , just $ mkConstMethod "minorVersion" [] intT
  , just $ mkConstMethod "name" [] $ objT c_QString
  , just $ mkConstMethod "segmentCount" [] intT
  , just $ mkConstMethod' "type" "typeOSType" [] $ enumT e_OSType
  ]
  
  
e_OSType =
  makeQtEnum (ident1 "QOperatingSystemVersion" "OSType") [includeStd "QOperatingSystemVersion"]
  [ (0, ["unknown"])
  , (1, ["windows"])
  , (2, ["mac", "o", "s"])
  , (3, ["i", "o", "s"])
  , (4, ["tv", "o", "s"])
  , (5, ["watch", "o", "s"])
  , (6, ["android"])
  ]
  
  
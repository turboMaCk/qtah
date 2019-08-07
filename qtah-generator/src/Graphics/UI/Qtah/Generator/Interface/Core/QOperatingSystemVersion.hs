module Graphics.UI.Qtah.Generator.Interface.Core.QOperatingSystemVersion (
  aModule,
  c_QOperatingSystemVersion,
  e_OSType,
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

import Foreign.Hoppy.Generator.Types (boolT, intT, voidT, enumT, bitspaceT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
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
  , just $ mkConstMethod "type" [] $ enumT e_OSType

  , test (qtVersion >= [5, 9]) $ mkStaticMethod "AndroidJellyBean" [] $ constT $ objT c_QOperatingSystemVersion
  , test (qtVersion >= [5, 9]) $ mkStaticMethod "AndroidJellyBean_MR1" [] $ constT $ objT c_QOperatingSystemVersion
  , test (qtVersion >= [5, 9]) $ mkStaticMethod "AndroidJellyBean_MR2" [] $ constT $ objT c_QOperatingSystemVersion
  , test (qtVersion >= [5, 9]) $ mkStaticMethod "AndroidKitKat" [] $ constT $ objT c_QOperatingSystemVersion
  , test (qtVersion >= [5, 9]) $ mkStaticMethod "AndroidLollipop" [] $ constT $ objT c_QOperatingSystemVersion
  , test (qtVersion >= [5, 9]) $ mkStaticMethod "AndroidLollipop_MR1" [] $ constT $ objT c_QOperatingSystemVersion
  , test (qtVersion >= [5, 9]) $ mkStaticMethod "AndroidMarshmallow" [] $ constT $ objT c_QOperatingSystemVersion
  , test (qtVersion >= [5, 9]) $ mkStaticMethod "AndroidNougat" [] $ constT $ objT c_QOperatingSystemVersion
  , test (qtVersion >= [5, 9]) $ mkStaticMethod "AndroidNougat_MR1" [] $ constT $ objT c_QOperatingSystemVersion
  , test (qtVersion >= [5, 9, 2]) $ mkStaticMethod "AndroidOreo" [] $ constT $ objT c_QOperatingSystemVersion
  , test (qtVersion >= [5, 9, 1]) $ mkStaticMethod "MacOSHighSierra" [] $ constT $ objT c_QOperatingSystemVersion
  , test (qtVersion >= [5, 11, 2]) $ mkStaticMethod "MacOSMojave" [] $ constT $ objT c_QOperatingSystemVersion
  , test (qtVersion >= [5, 9]) $ mkStaticMethod "MacOSSierra" [] $ constT $ objT c_QOperatingSystemVersion
  , test (qtVersion >= [5, 9]) $ mkStaticMethod "OSXElCapitan" [] $ constT $ objT c_QOperatingSystemVersion
  , test (qtVersion >= [5, 9]) $ mkStaticMethod "OSXMavericks" [] $ constT $ objT c_QOperatingSystemVersion
  , test (qtVersion >= [5, 9]) $ mkStaticMethod "OSXYosemite" [] $ constT $ objT c_QOperatingSystemVersion
  , test (qtVersion >= [5, 9]) $ mkStaticMethod "Windows7" [] $ constT $ objT c_QOperatingSystemVersion
  , test (qtVersion >= [5, 9]) $ mkStaticMethod "Windows8" [] $ constT $ objT c_QOperatingSystemVersion
  , test (qtVersion >= [5, 9]) $ mkStaticMethod "Windows10" [] $ constT $ objT c_QOperatingSystemVersion
  , test (qtVersion >= [5, 9]) $ mkStaticMethod "Windows8_1" [] $ constT $ objT c_QOperatingSystemVersion
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
  
  
module Graphics.UI.Qtah.Generator.Interface.Core.QSysInfo (
  aModule,
  c_QSysInfo,
  e_Endian,
  e_Sizes,
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
  
import Data.Bits ((.|.), finiteBitSize)
import Foreign.Ptr (IntPtr)
import Foreign.Hoppy.Generator.Types (voidT, enumT, bitspaceT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}



aModule =
  AQtModule $
  makeQtModule ["Core", "QSysInfo"] $
  collect
  [ just $ QtExport $ ExportClass c_QSysInfo
  , just $ QtExport $ ExportEnum e_Endian
  , just $ QtExport $ ExportEnum e_Sizes
  ]


c_QSysInfo =
  addReqIncludes [ includeStd "QSysInfo" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QSysInfo") Nothing [] $
  collect
  [ test (qtVersion >= [5, 11]) $ mkStaticMethod "bootUniqueId" [] $ objT c_QByteArray
  , test (qtVersion >= [5, 4]) $ mkStaticMethod "buildAbi" [] $ objT c_QString
  , test (qtVersion >= [5, 4]) $ mkStaticMethod "buildCpuArchitecture" [] $ objT c_QString
  , test (qtVersion >= [5, 4]) $ mkStaticMethod "currentCpuArchitecture" [] $ objT c_QString
  , test (qtVersion >= [5, 4]) $ mkStaticMethod "kernelType" [] $ objT c_QString
  , test (qtVersion >= [5, 4]) $ mkStaticMethod "kernelVersion" [] $ objT c_QString
  , test (qtVersion >= [5, 6]) $ mkStaticMethod "machineHostName" [] $ objT c_QString
  , test (qtVersion >= [5, 11]) $ mkStaticMethod "machineUniqueId" [] $ objT c_QByteArray
  , test (qtVersion >= [5, 4]) $ mkStaticMethod "prettyProductName" [] $ objT c_QString
  , test (qtVersion >= [5, 4]) $ mkStaticMethod "productType" [] $ objT c_QString
  , test (qtVersion >= [5, 4]) $ mkStaticMethod "productVersion" [] $ objT c_QString 
  ]
  
  
e_Endian =
  makeQtEnum (ident1 "QSysInfo" "Endian") [includeStd "QSysInfo"] $
  let bigEndian = 0
      littleEndian = 1
      byteOrder = bigEndian .|. littleEndian
  in  [ (bigEndian, ["big", "endian"])
      , (littleEndian, ["little", "endian"])
      , (byteOrder, ["byte", "order"]) ]
  
  
e_Sizes =
  makeQtEnum (ident1 "QSysInfo" "Sizes") [includeStd "QSysInfo"] $
  let wordSize = finiteBitSize (undefined :: IntPtr)
  in  [ (wordSize, ["word", "size"])]
  
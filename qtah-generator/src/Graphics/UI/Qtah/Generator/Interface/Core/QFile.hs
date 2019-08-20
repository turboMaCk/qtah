module Graphics.UI.Qtah.Generator.Interface.Core.QFile (
  aModule,
  c_QFile,
  decoderFn,
  ) where


import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Type,
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
  

import Graphics.UI.Qtah.Generator.Interface.Core.QFileDevice (c_QFileDevice, bs_FileHandleFlags, bs_Permissions)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QIODevice (c_QIODevice, bs_OpenMode)
import Foreign.Hoppy.Generator.Types (boolT, intT, charT, voidT, enumT, bitspaceT, constT, objT, ptrT, refT, fnT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qint64)


{-# ANN module "HLint: ignore Use camelCase" #-}



aModule =
  AQtModule $
  makeQtModule ["Core", "QFile"] $
  [QtExport $ ExportClass c_QFile]
  

c_QFile =
  addReqIncludes [ includeStd "QFile" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QFile") Nothing [c_QFileDevice] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QObject]
  , just $ mkCtor "newWithName" [refT $ objT c_QString]
  , just $ mkCtor "newWithNameParent" [refT $ constT $ objT c_QString, ptrT $ objT c_QObject]
  , just $ mkMethod' "copy" "copy" [refT $ constT $ objT c_QString] boolT
  , just $ mkStaticMethod' "copy" "copyWithFilename" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] boolT
  , just $ mkStaticMethod' "decodeName" "decodeName" [refT $ constT $ objT c_QByteArray] $ objT c_QString
  , just $ mkStaticMethod' "decodeName" "decodeNamelocalFileName" [ptrT $ constT $ charT] $ objT c_QString
  , just $ mkStaticMethod "encodeName" [refT $ constT $ objT c_QString] $ objT c_QByteArray
  , just $ mkStaticMethod' "exists" "existsWithFilename" [refT $ constT $ objT c_QString] boolT
  , just $ mkConstMethod' "exists" "exists" [] boolT
  , just $ mkConstMethod "fileName" [] $ objT c_QString
  , just $ mkMethod' "link" "link" [refT $ constT $ objT c_QString] boolT
  , just $ mkStaticMethod' "link" "linkWithFilename" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] boolT
  , just $ mkMethod' "open" "open" [bitspaceT bs_OpenMode] boolT
  -- TODO bool QFile::open(FILE *fh, QIODevice::OpenMode mode, QFileDevice::FileHandleFlags handleFlags = DontCloseHandle)
  , just $ mkMethod' "open" "openWithFileDesc" [intT, bitspaceT bs_OpenMode ] boolT
  , just $ mkMethod' "open" "openWithFileDescFlags" [intT, bitspaceT bs_OpenMode, bitspaceT bs_FileHandleFlags] boolT
  , just $ mkConstMethod' "permissions" "permissions" [] $ bitspaceT bs_Permissions
  , just $ mkStaticMethod' "permissions" "permissionsWithFilename" [refT $ constT $ objT c_QString] $ bitspaceT bs_Permissions
  , just $ mkMethod' "remove" "remove" [] boolT
  , just $ mkStaticMethod' "remove" "removeWithFilename" [refT $ constT $ objT c_QString] boolT
  , just $ mkMethod' "rename" "rename" [refT $ constT $ objT c_QString] boolT
  , just $ mkStaticMethod' "rename" "renameOldToNewName" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] boolT
  , just $ mkMethod' "resize" "resize" [qint64] boolT
  , just $ mkStaticMethod' "resize" "resizeWithFilename" [refT $ constT $ objT c_QString, qint64] boolT
  , just $ mkMethod "setFileName" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "setPermissions" "setPermissions" [bitspaceT bs_Permissions] boolT
  , just $ mkStaticMethod' "setPermissions" "setPermissionsWithFilename" [refT $ constT $ objT c_QString, bitspaceT bs_Permissions] boolT
  , just $ mkConstMethod "size" [] qint64
  , test (qtVersion >= [4, 2]) $ mkStaticMethod' "symLinkTarget" "symLinkTargetWithFilename" [refT $ constT $ objT c_QString] $ objT c_QString
  , test (qtVersion >= [4, 2]) $ mkConstMethod' "symLinkTarget" "symLinkTarget" [] $ objT c_QString
  ]
  
  
decoderFn :: Type
decoderFn = ptrT $ fnT [refT $ constT $ objT c_QByteArray] $ objT c_QString

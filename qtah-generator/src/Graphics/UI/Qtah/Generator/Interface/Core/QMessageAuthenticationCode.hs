module Graphics.UI.Qtah.Generator.Interface.Core.QMessageAuthenticationCode (
  aModule,
  c_QMessageAuthenticationCode,
  ) where



import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
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
  

import Foreign.Hoppy.Generator.Types (charT, intT, boolT, voidT, enumT, bitspaceT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QIODevice (c_QIODevice)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QCryptographicHash (e_Algorithm)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}



aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QMessageAuthenticationCode"] [5, 1] $
  [QtExport $ ExportClass c_QMessageAuthenticationCode]
  

c_QMessageAuthenticationCode =
  addReqIncludes [ includeStd "QMessageAuthenticationCode" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QMessageAuthenticationCode") Nothing [] $
  collect
  [ just $ mkCtor "new" [enumT e_Algorithm]
  , just $ mkCtor "newWithKey" [enumT e_Algorithm, refT $ constT $ objT c_QByteArray]
  , just $ mkMethod' "addData" "addData" [ptrT $ constT charT, intT] voidT
  , just $ mkMethod' "addData" "addDataQBytearray" [refT $ constT $ objT c_QByteArray] voidT
  , just $ mkMethod' "addData" "addDataDevice" [ptrT $ objT c_QIODevice] boolT
  , just $ mkStaticMethod "hash" [refT $ constT $ objT c_QByteArray, refT $ constT $ objT c_QByteArray, enumT e_Algorithm] $ objT c_QByteArray
  , just $ mkMethod' "reset" "reset" [] voidT
  , just $ mkConstMethod' "result" "resetReturnByte" [] $ objT c_QByteArray
  , just $ mkMethod "setKey" [refT $ constT $ objT c_QByteArray] voidT
  ]
  
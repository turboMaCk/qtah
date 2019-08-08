module Graphics.UI.Qtah.Generator.Interface.Core.QTime (
  aModule,
  c_QTime,
  ) where


import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Operator (OpShl, OpShr),
  classSetConversionToGc,
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
  
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Comparable, Equatable, Copyable),
  classAddFeatures,
  )
  
import Foreign.Hoppy.Generator.Types (boolT, intT, voidT, enumT, bitspaceT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_DateFormat)

{-# ANN module "HLint: ignore Use camelCase" #-}



aModule =
  AQtModule $
  makeQtModule ["Core", "QTime"] $
  [QtExport $ ExportClass c_QTime]
  

c_QTime =
  addReqIncludes [ includeStd "QTime" ] $
  classSetConversionToGc $
  classAddFeatures [Comparable, Equatable, Copyable] $
  classSetEntityPrefix "" $
  makeClass (ident "QTime") Nothing [] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithHM" [intT, intT]
  , just $ mkCtor "newWithHMS" [intT, intT, intT]
  , just $ mkCtor "newWithHMSms" [intT, intT, intT, intT]
  , just $ mkConstMethod "addMSecs" [intT] $ objT c_QTime
  , just $ mkConstMethod "addSecs" [intT] $ objT c_QTime
  , just $ mkStaticMethod "currentTime" [] $ objT c_QTime
  , just $ mkConstMethod "elapsed" [] intT
  , just $ mkStaticMethod "fromMSecsSinceStartOfDay" [intT] $ objT c_QTime
  , just $ mkStaticMethod' "fromString" "fromString" [refT $ constT $ objT c_QString] $ objT c_QTime
  , just $ mkStaticMethod' "fromString" "fromStringWithDateFormat" [refT $ constT $ objT c_QString, enumT e_DateFormat] $ objT c_QTime
  , just $ mkStaticMethod' "fromString" "fromStrings" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] $ objT c_QTime
  , just $ mkConstMethod "hour" [] intT
  , just $ mkConstMethod "isNull" [] boolT
  , just $ mkConstMethod' "isValid" "isValid" [] boolT
  , just $ mkMethod' "isValid" "isValidWithHMS" [intT, intT, intT] boolT
  , just $ mkMethod' "isValid" "isValidWithHMSms" [intT, intT, intT, intT] boolT
  , just $ mkConstMethod "minute" [] intT
  , just $ mkConstMethod "msec" [] intT
  , just $ mkConstMethod "msecsSinceStartOfDay" [] intT
  , just $ mkConstMethod "msecsTo" [refT $ constT $ objT c_QTime] intT
  , just $ mkMethod "restart" [] intT
  , just $ mkConstMethod "second" [] intT
  , just $ mkConstMethod "secsTo" [refT $ constT $ objT c_QTime] intT
  , just $ mkMethod' "setHMS" "setHMS" [intT, intT, intT ] boolT
  , just $ mkMethod' "setHMS" "setHMSWithMs" [intT, intT, intT, intT] boolT
  , just $ mkMethod "start" [] voidT
  , just $ mkConstMethod' "toString" "toString" [] $ objT c_QString
  , just $ mkConstMethod' "toString" "toStringWithDateformat" [enumT e_DateFormat] $ objT c_QString
  --, just $ mkMethod OpShl [refT $ objT c_QDataStream, refT $ objT c_QTime] $ refT $ objT c_QDataStream
  --, just $ mkMethod OpShr [refT $ objT c_QDataStream, refT $ objT c_QTime] $ refT $ objT c_QDataStream
  ]
  
  


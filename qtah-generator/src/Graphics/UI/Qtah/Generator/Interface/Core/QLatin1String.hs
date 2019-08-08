module Graphics.UI.Qtah.Generator.Interface.Core.QLatin1String (
  aModule,
  c_QLatin1String,
  ) where


import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Operator (OpArray, OpNe, OpLt, OpLe, OpEq, OpGt, OpGe),
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
  ClassFeature (Equatable, Comparable, Copyable),
  classAddFeatures,
  )

import Foreign.Hoppy.Generator.Types (toGcT, charT, intT, boolT, voidT, enumT, bitspaceT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QLatin1Char (c_QLatin1Char)
import Graphics.UI.Qtah.Generator.Interface.Core.QChar (c_QChar)
--import Graphics.UI.Qtah.Generator.Interface.Core.QStringView (c_QStringView)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_CaseSensitivity)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}



aModule =
  AQtModule $
  makeQtModule ["Core", "QLatin1String"] $
  [QtExport $ ExportClass c_QLatin1String]
  

c_QLatin1String =
  addReqIncludes [ includeStd "QLatin1String" ] $
  classSetConversionToGc $
  classAddFeatures [Equatable, Comparable, Copyable] $
  classSetEntityPrefix "" $
  makeClass (ident "QLatin1String") Nothing [] $
  collect
  [ test (qtVersion >= [5, 6]) $ mkCtor "new" []
  , just $ mkCtor "newStr" [ptrT $ constT charT]
  , test (qtVersion >= [5, 10]) $ mkCtor "newFirstLast" [ptrT $ constT charT, ptrT $ constT charT]
  , just $ mkCtor "newStrSize" [ptrT $ constT charT, intT]
  , just $ mkCtor "newStrbyte" [refT $ constT $ objT c_QByteArray]
  , test (qtVersion >= [5, 8]) $ mkConstMethod' "at" "at" [intT] $ toGcT $ objT c_QLatin1Char
  , test (qtVersion >= [5, 10]) $ mkConstMethod "back" [] $ toGcT $ objT c_QLatin1Char
  , test (qtVersion >= [5, 10]) $ mkMethod "chop" [intT] voidT
  , test (qtVersion >= [5, 10]) $ mkConstMethod "chopped" [intT] $ objT c_QLatin1String
  , just $ mkConstMethod' "data" "dataString" [] $ ptrT $ constT charT
  , test (qtVersion >= [5, 10]) $ mkConstMethod "front" [] $ toGcT $ objT c_QLatin1Char
  , test (qtVersion >= [5, 10]) $ mkConstMethod "isEmpty" [] boolT
  , test (qtVersion >= [5, 10]) $ mkConstMethod "isNull" [] boolT
  , just $ mkConstMethod "latin1" [] $ ptrT $ constT charT
  , test (qtVersion >= [5, 8]) $ mkConstMethod "left" [intT] $ objT c_QLatin1String
  , test (qtVersion >= [5, 8]) $ mkConstMethod' "mid" "mid" [intT] $ objT c_QLatin1String
  , test (qtVersion >= [5, 8]) $ mkConstMethod' "mid" "midStartLength" [intT, intT] $ objT c_QLatin1String
  , test (qtVersion >= [5, 8]) $ mkConstMethod "right" [intT] $ objT c_QLatin1String
  , just $ mkConstMethod "size" [] intT
  , test (qtVersion >= [5, 10]) $ mkConstMethod "trimmed" [] $ objT c_QLatin1String
  , test (qtVersion >= [5, 10]) $ mkMethod "truncate" [intT] voidT
  --, test (qtVersion >= [5, 10]) $ mkConstMethod' "endsWith" "endsWith" [objT c_QStringView] boolT
  --, test (qtVersion >= [5, 10]) $ mkConstMethod' "endsWith" "endsWithCase" [objT c_QStringView, enumT e_CaseSensitivity] boolT
  , test (qtVersion >= [5, 10]) $ mkConstMethod' "endsWith" "endsWithQLatin" [objT c_QLatin1String] boolT
  , test (qtVersion >= [5, 10]) $ mkConstMethod' "endsWith" "endsWithWithQLatinCase" [objT c_QLatin1String, enumT e_CaseSensitivity] boolT
  , test (qtVersion >= [5, 10]) $ mkConstMethod' "endsWith" "endsWithQChar" [objT c_QChar] boolT
  , test (qtVersion >= [5, 10]) $ mkConstMethod' "endsWith" "endsWithQCharCase" [objT c_QChar, enumT e_CaseSensitivity] boolT
  --, test (qtVersion >= [5, 10]) $ mkConstMethod' "startsWith" "startsWith" [objT c_QStringView] boolT
  --, test (qtVersion >= [5, 10]) $ mkConstMethod' "startsWith" "startsWithCase" [objT c_QStringView, enumT e_CaseSensitivity] boolT
  , test (qtVersion >= [5, 10]) $ mkConstMethod' "startsWith" "startsWithQLatin" [objT c_QLatin1String] boolT
  , test (qtVersion >= [5, 10]) $ mkConstMethod' "startsWith" "startsWithQLatinCase" [objT c_QLatin1String, enumT e_CaseSensitivity] boolT
  , test (qtVersion >= [5, 10]) $ mkConstMethod' "startsWith" "startsWithQChar" [objT c_QChar] boolT
  , test (qtVersion >= [5, 10]) $ mkConstMethod' "startsWith" "startsWithQCharCase" [objT c_QChar, enumT e_CaseSensitivity] boolT
  , just $ mkConstMethod' OpNe "notEqToQString" [refT $ constT $ objT c_QString] boolT
  , test (qtVersion >= [4, 3]) $ mkConstMethod' OpNe "notEqToChar" [ptrT $ constT charT] boolT
  , test (qtVersion >= [5, 0]) $ mkConstMethod' OpNe "notEqToQBytearray" [refT $ constT $ objT c_QByteArray] boolT
  , just $ mkConstMethod' OpLt "lessThanQString" [refT $ constT $ objT c_QString] boolT
  , test (qtVersion >= [4, 3]) $ mkConstMethod' OpLt "lessThanChar" [ptrT $ constT charT] boolT
  , test (qtVersion >= [5, 0]) $ mkConstMethod' OpLt "lessThanQBytearray" [refT $ constT $ objT c_QByteArray] boolT
  , just $ mkConstMethod' OpLe "lessOrEqThanQString" [refT $ constT $ objT c_QString] boolT
  , test (qtVersion >= [4, 3]) $ mkConstMethod' OpLe "lessOrEqThanChar" [ptrT $ constT charT] boolT
  , test (qtVersion >= [5, 0]) $ mkConstMethod' OpLe "lessOrEqThanQBytearray" [refT $ constT $ objT c_QByteArray] boolT
  , just $ mkConstMethod' OpEq "equalToQString" [refT $ constT $ objT c_QString] boolT
  , test (qtVersion >= [4, 3]) $ mkConstMethod' OpEq "equalToChar" [ptrT $ constT charT] boolT
  , test (qtVersion >= [5, 0]) $ mkConstMethod' OpEq "equalToQBytearray" [refT $ constT $ objT c_QByteArray] boolT
  , just $ mkConstMethod' OpGt "greaterThanQString" [refT $ constT $ objT c_QString] boolT
  , test (qtVersion >= [4, 3]) $ mkConstMethod' OpGt "greaterThanChar" [ptrT $ constT charT] boolT
  , test (qtVersion >= [5, 0]) $ mkConstMethod' OpGt "greaterThanQBytearray" [refT $ constT $ objT c_QByteArray] boolT
  , just $ mkConstMethod' OpGe "greaterOrEqThanQString" [refT $ constT $ objT c_QString] boolT
  , test (qtVersion >= [4, 3]) $ mkConstMethod' OpGe "greaterOrEqThanChar" [ptrT $ constT charT] boolT
  , test (qtVersion >= [5, 0]) $ mkConstMethod' OpGe "greaterOrEqThanQBytearray" [refT $ constT $ objT c_QByteArray] boolT
  , test (qtVersion >= [5, 8]) $ mkConstMethod' OpArray "get" [intT] $ toGcT $ objT c_QLatin1Char 
  ]
  
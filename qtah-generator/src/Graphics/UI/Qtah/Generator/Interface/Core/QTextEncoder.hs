module Graphics.UI.Qtah.Generator.Interface.Core.QTextEncoder (
  aModule,
  c_QTextEncoder,
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
  

import Foreign.Hoppy.Generator.Types (intT, voidT, enumT, bitspaceT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QTextCodec (c_QTextCodec) --bs_ConversionFlags)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QChar (c_QChar)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
-- TODO import Graphics.UI.Qtah.Generator.Interface.Core.QStringView (c_QStringView)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types



{-# ANN module "HLint: ignore Use camelCase" #-}



aModule =
  AQtModule $
  makeQtModule ["Core", "QTextEncoder"] $
  [QtExport $ ExportClass c_QTextEncoder]
  
  

c_QTextEncoder =
  addReqIncludes [ includeStd "QTextEncoder" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QTextEncoder") Nothing [] $
  collect
  [ 
    just $ mkCtor "new" [ptrT $ objT c_QTextCodec]
  --, test (qtVersion >= [4, 7]) $ mkCtor "newWithConverFlags" [ptrT $ objT c_QTextCodec, bitspaceT bs_ConversionFlags]
  --, test (qtVersion >= [5, 10]) $ mkMethod' "fromUnicode" "fromUnicodeWithStringview" [objT c_QStringView] $ objT c_QByteArray 
  , just $ mkMethod' "fromUnicode" "fromUnicodeWithString" [refT $ constT $ objT c_QString] $ objT c_QByteArray
  , just $ mkMethod' "fromUnicode" "fromUnicodeWithQChar" [ptrT $ constT $ objT c_QChar, intT] $ objT c_QByteArray
  ]
  
  


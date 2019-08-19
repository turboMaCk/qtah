module Graphics.UI.Qtah.Generator.Interface.Core.QTextDecoder (
  aModule,
  c_QTextDecoder,
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
  

import Foreign.Hoppy.Generator.Types (intT, charT, voidT, enumT, bitspaceT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QTextCodec (c_QTextCodec)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types


{-# ANN module "HLint: ignore Use camelCase" #-}


aModule =
  AQtModule $
  makeQtModule ["Core", "QTextDecoder"] $
  [QtExport $ ExportClass c_QTextDecoder]
  

c_QTextDecoder =
  addReqIncludes [ includeStd "QTextDecoder" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QTextDecoder") Nothing [] $
  collect
  [ just $ mkCtor "new" [ptrT $ constT $ objT c_QTextCodec] 
   -- TODO QTextDecoder::QTextDecoder(const QTextCodec *codec, QTextCodec::ConversionFlags flags)
  , just $ mkMethod' "toUnicode" "toUnicode" [ptrT $ constT $ charT, intT] $ objT c_QString
  , just $ mkMethod' "toUnicode" "toUnicodeWithByte" [refT $ constT $ objT c_QByteArray] $ objT c_QString
  , just $ mkMethod' "toUnicode" "toUnicodeWithStr" [ptrT $ objT c_QString, ptrT $ constT $ charT, intT] voidT
  ]
  
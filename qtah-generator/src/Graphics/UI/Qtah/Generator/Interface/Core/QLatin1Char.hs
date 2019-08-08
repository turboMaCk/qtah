module Graphics.UI.Qtah.Generator.Interface.Core.QLatin1Char (
  aModule,
  c_QLatin1Char,
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
  
import Foreign.Hoppy.Generator.Types (charT, ushortT, voidT, enumT, bitspaceT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}



aModule =
  AQtModule $
  makeQtModule ["Core", "QLatin1Char"] $
  [QtExport $ ExportClass c_QLatin1Char]
  

c_QLatin1Char =
  addReqIncludes [ includeStd "QLatin1Char" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QLatin1Char") Nothing [] $
  collect
  [ just $ mkCtor "new" [charT]
  , just $ mkConstMethod "toLatin1" [] charT 
  , just $ mkConstMethod "unicode" [] ushortT
  ]
  
module Graphics.UI.Qtah.Generator.Interface.Core.QMessageLogContext (
  aModule,
  c_QMessageLogContext,
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
  

import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types


{-# ANN module "HLint: ignore Use camelCase" #-}



aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QMessageLogContext"] [5, 0] $
  [QtExport $ ExportClass c_QMessageLogContext]
  

c_QMessageLogContext =
  addReqIncludes [ includeStd "QMessageLogContext" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QMessageLogContext") Nothing [] $
  collect
  [ 
 
  ]
  
  


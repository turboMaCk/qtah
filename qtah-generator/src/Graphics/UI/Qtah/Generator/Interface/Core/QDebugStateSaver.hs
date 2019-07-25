module Graphics.UI.Qtah.Generator.Interface.Core.QDebugStateSaver (
  aModule,
  c_QDebugStateSaver,
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
  

import Foreign.Hoppy.Generator.Types (voidT, enumT, bitspaceT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QDebug (c_QDebug)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types



{-# ANN module "HLint: ignore Use camelCase" #-}



aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QDebugStateSaver"] [5, 1]
  [QtExport $ ExportClass c_QDebugStateSaver]
  
  

c_QDebugStateSaver =
  addReqIncludes [ includeStd "QDebugStateSaver" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QDebugStateSaver") Nothing [] $
  collect
  [ 
     just $ mkCtor "newWithDebug" [refT $ objT c_QDebug]
  ]
  
  


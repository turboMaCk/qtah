module Graphics.UI.Qtah.Generator.Interface.Core.QPauseAnimation (
  aModule,
  c_QPauseAnimation,
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
  

import Graphics.UI.Qtah.Generator.Interface.Core.QAbstractAnimation (c_QAbstractAnimation)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Foreign.Hoppy.Generator.Types (intT, boolT, voidT, enumT, bitspaceT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types


{-# ANN module "HLint: ignore Use camelCase" #-}


aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QPauseAnimation"] [4, 6] $
  [QtExport $ ExportClass c_QPauseAnimation]
  

c_QPauseAnimation =
  addReqIncludes [ includeStd "QPauseAnimation" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QPauseAnimation") Nothing [c_QAbstractAnimation] $
  collect
  [ just $ mkConstMethod "duration" [] $ intT
  , just $ mkMethod "setDuration" [intT] voidT 
  ]
  
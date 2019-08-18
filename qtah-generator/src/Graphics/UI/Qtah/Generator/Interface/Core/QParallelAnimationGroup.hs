module Graphics.UI.Qtah.Generator.Interface.Core.QParallelAnimationGroup (
  aModule,
  c_QParallelAnimationGroup,
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
  

import Graphics.UI.Qtah.Generator.Interface.Core.QAnimationGroup (c_QAnimationGroup)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Foreign.Hoppy.Generator.Types (, voidT, enumT, bitspaceT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types


{-# ANN module "HLint: ignore Use camelCase" #-}


aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QParallelAnimationGroup"] [4, 6] $
  [QtExport $ ExportClass c_QParallelAnimationGroup]
  

c_QParallelAnimationGroup =
  addReqIncludes [ includeStd "QParallelAnimationGroup" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QParallelAnimationGroup") Nothing [c_QAnimationGroup] $
  collect
  [ 
  -- TODO Methods 
  ]
  
  


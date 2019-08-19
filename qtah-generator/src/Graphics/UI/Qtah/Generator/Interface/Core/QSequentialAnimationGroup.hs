module Graphics.UI.Qtah.Generator.Interface.Core.QSequentialAnimationGroup (
  aModule,
  c_QSequentialAnimationGroup,
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
import Graphics.UI.Qtah.Generator.Interface.Core.QPauseAnimation (c_QPauseAnimation)
import Graphics.UI.Qtah.Generator.Interface.Core.QAbstractAnimation (c_QAbstractAnimation)
import Foreign.Hoppy.Generator.Types (intT, voidT, enumT, bitspaceT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Internal.Listener (c_ListenerQAbstractAnimation)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types


{-# ANN module "HLint: ignore Use camelCase" #-}


aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QSequentialAnimationGroup"] [4, 6] $
  (QtExport $ ExportClass c_QSequentialAnimationGroup) :
  map QtExportSignal signals
  

c_QSequentialAnimationGroup =
  addReqIncludes [ includeStd "QSequentialAnimationGroup" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QSequentialAnimationGroup") Nothing [c_QAnimationGroup] $
  collect
  [ just $ mkMethod "addPause" [intT] $ ptrT $ objT c_QPauseAnimation
  , just $ mkMethod "insertPause" [intT, intT] $ ptrT $ objT c_QPauseAnimation
  , just $ mkConstMethod "duration" [] intT
  , just $ mkConstMethod "currentAnimation" [] $ ptrT $ objT c_QAbstractAnimation
  ]
  

signals :: [Signal]
signals =
  collect
  [ just $ makeSignal c_QSequentialAnimationGroup "currentAnimationChanged" c_ListenerQAbstractAnimation
  ]
  
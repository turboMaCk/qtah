module Graphics.UI.Qtah.Generator.Interface.Core.QRandomGenerator64 (
  aModule,
  c_QRandomGenerator64,
  ) where


import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Operator (OpCall),
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
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (quint64)
import Graphics.UI.Qtah.Generator.Interface.Core.QRandomGenerator (c_QRandomGenerator)


{-# ANN module "HLint: ignore Use camelCase" #-}



aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QRandomGenerator64"] [5, 10] $
  [QtExport $ ExportClass c_QRandomGenerator64]
  

c_QRandomGenerator64 =
  addReqIncludes [ includeStd "QRandomGenerator64" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QRandomGenerator64") Nothing [c_QRandomGenerator] $
  collect
  [ just $ mkMethod "generate" [] quint64
  , just $ mkMethod OpCall [] quint64
  ]
  
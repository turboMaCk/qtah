module Graphics.UI.Qtah.Generator.Interface.Core.QPropertyAnimation (
  aModule,
  c_QPropertyAnimation,
  ) where



import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
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
  mkMethod,
  mkProp
  )
  

import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Copyable),
  classAddFeatures,
  )
  

import Graphics.UI.Qtah.Generator.Interface.Core.QVariantAnimation (c_QVariantAnimation)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Foreign.Hoppy.Generator.Types (voidT, enumT, bitspaceT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types


{-# ANN module "HLint: ignore Use camelCase" #-}


aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QPropertyAnimation"] [4, 6] $
  [QtExport $ ExportClass c_QPropertyAnimation]
  

c_QPropertyAnimation =
  addReqIncludes [ includeStd "QPropertyAnimation" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QPropertyAnimation") Nothing [c_QVariantAnimation] $
  collect
  [ just $ mkProp "propertyName" $ objT c_QByteArray
  , just $ mkProp "targetObject" $ ptrT $ objT c_QObject
  ]
  
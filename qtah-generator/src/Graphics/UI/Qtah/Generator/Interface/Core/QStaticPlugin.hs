module Graphics.UI.Qtah.Generator.Interface.Core.QStaticPlugin (
  aModule,
  c_QStaticPlugin,
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
  

import Foreign.Hoppy.Generator.Types (charT, voidT, enumT, bitspaceT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
--import Graphics.UI.Qtah.Generator.Interface.Core.QJsonObject (c_QJsonObject)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}



aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QStaticPlugin"] [5, 2] $
  [QtExport $ ExportClass c_QStaticPlugin]
  

c_QStaticPlugin =
  addReqIncludes [ includeStd "QStaticPlugin" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QStaticPlugin") Nothing [] $
  collect
  [ just $ mkMethod "instance" [] $ ptrT $ objT c_QObject
  , just $ mkConstMethod "metaData" [] $ objT c_QJsonObject
  , just $ mkMethod "rawMetaData" [] $ ptrT $ constT charT 
  ]
  
module Graphics.UI.Qtah.Generator.Interface.Core.QPluginLoader (
  aModule,
  c_QPluginLoader,
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

import Foreign.Hoppy.Generator.Types (charT, boolT, voidT, enumT, bitspaceT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QLibrary (c_QLibrary, bs_LoadHints)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
--import Graphics.UI.Qtah.Generator.Interface.Core.QJsonObject (c_QJsonObject)
--import Graphics.UI.Qtah.Generator.Interface.Core.QStaticPlugin (c_QStaticPlugin)
--import Graphics.UI.Qtah.Generator.Interface.Core.QVector (c_QVectorQStaticPlugin)
import Graphics.UI.Qtah.Generator.Interface.Core.QList (c_QListQObject)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}



aModule =
  AQtModule $
  makeQtModule ["Core", "QPluginLoader"] $
  [QtExport $ ExportClass c_QPluginLoader]
  

c_QPluginLoader =
  addReqIncludes [ includeStd "QPluginLoader" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QPluginLoader") Nothing [c_QObject] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QObject]
  , just $ mkCtor "newWithFilename" [refT $ constT $ objT c_QString]
  , just $ mkCtor "newWithFilenameParent" [refT $ constT $ objT c_QString, ptrT $ objT c_QObject]
  , test (qtVersion >= [4, 2]) $ mkConstMethod "errorString" [] $ objT c_QString
  , just $ mkMethod' "instance" "pluginInstance" [] $ ptrT $ objT c_QObject
  , just $ mkConstMethod "isLoaded" [] $ boolT
  , just $ mkMethod "load" [] boolT
  --, just $ mkConstMethod "metaData" [] $ objT c_QJsonObject
  , just $ mkStaticMethod "staticInstances" [] $ objT c_QListQObject
  --, TODO QVector<QStaticPlugin> QPluginLoader::staticPlugins()
  , just $ mkMethod "unload" [] boolT
  ]
  
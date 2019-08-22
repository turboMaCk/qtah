module Graphics.UI.Qtah.Generator.Interface.Core.QFileSystemWatcher (
  aModule,
  c_QFileSystemWatcher,
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
  

import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Foreign.Hoppy.Generator.Types (boolT, voidT, enumT, bitspaceT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Internal.Listener (c_ListenerQString)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types


{-# ANN module "HLint: ignore Use camelCase" #-}


aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QFileSystemWatcher"] [4, 2] $
  (QtExport $ ExportClass c_QFileSystemWatcher) :
  map QtExportSignal signals
  

c_QFileSystemWatcher =
  addReqIncludes [ includeStd "QFileSystemWatcher" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QFileSystemWatcher") Nothing [c_QObject] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QObject] 
  , just $ mkCtor "newWithList" [refT $ constT $ objT c_QStringList]
  , just $ mkCtor "newWithListParent" [refT $ constT $ objT c_QStringList, ptrT $ objT c_QObject]
  , just $ mkMethod "addPath" [refT $ constT $ objT c_QString] boolT
  , just $ mkMethod "addPaths" [refT $ constT $ objT c_QStringList] $ objT c_QStringList
  , just $ mkConstMethod "directories" [] $ objT c_QStringList
  , just $ mkConstMethod "files" [] $ objT c_QStringList
  , just $ mkMethod "removePath" [refT $ constT $ objT c_QString] boolT 
  , just $ mkMethod "removePaths" [refT $ constT $ objT c_QStringList] $ objT c_QStringList
  ]
  

signals :: [Signal]
signals =
  collect
  [ just $ makeSignal c_QFileSystemWatcher "directoryChanged" c_ListenerQString
  , just $ makeSignal c_QFileSystemWatcher "fileChanged" c_ListenerQString
  ]
  
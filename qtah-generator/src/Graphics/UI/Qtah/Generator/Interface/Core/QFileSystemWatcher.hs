-- This file is part of Qtah.
--
-- Copyright 2015-2019 The Qtah Authors.
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Graphics.UI.Qtah.Generator.Interface.Core.QFileSystemWatcher (
  aModule,
  c_QFileSystemWatcher,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Foreign.Hoppy.Generator.Types (boolT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (c_ListenerQString)
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
  , just $ mkCtor "newWithPaths" [refT $ constT $ objT c_QStringList]
  , just $ mkCtor "newWithPathsAndParent" [refT $ constT $ objT c_QStringList, ptrT $ objT c_QObject]
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

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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QTreeWidget (
  aModule,
  c_QTreeWidget,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Class,
  CppEnum,
  Export (ExportClass, ExportEnum),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  )

import Foreign.Hoppy.Generator.Types (intT, objT, ptrT, voidT, enumT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QList (c_QListQTreeWidgetItem)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QTreeWidgetItem (c_QTreeWidgetItem)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_SortOrder)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  c_Listener,
  c_ListenerPtrQTreeWidgetItem,
  c_ListenerPtrQTreeWidgetItemInt,
  c_ListenerPtrQTreeWidgetItemPtrQTreeWidgetItem,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QTreeView (c_QTreeView)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule :: AModule
aModule =
  AQtModule $
  makeQtModule ["Widgets", "QTreeWidget"] $
  QtExport (ExportClass c_QTreeWidget) :
  map QtExportSignal signals


c_QTreeWidget :: Class
c_QTreeWidget =
  addReqIncludes [includeStd "QTreeWidget"] $
  classSetEntityPrefix "" $
  makeClass (ident "QTreeWidget") Nothing [c_QTreeView] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , test (qtVersion >= [4, 1]) $
    mkMethod "addTopLevelItem" [ptrT $ objT c_QTreeWidgetItem] voidT
  , just $ mkConstMethod "currentItem" [] (ptrT $ objT c_QTreeWidgetItem)
  , just $ mkConstMethod "headerItem" [] (ptrT $ objT c_QTreeWidgetItem)
  , test (qtVersion >= [4, 2]) $
    mkConstMethod "invisibleRootItem" [] (ptrT $ objT c_QTreeWidgetItem)
  , just $ mkMethod "setCurrentItem" [ptrT $ objT c_QTreeWidgetItem] voidT
  , just $ mkMethod "setHeaderItem" [ptrT $ objT c_QTreeWidgetItem] voidT
  , test (qtVersion >= [4, 2]) $
    mkMethod "setHeaderLabel" [objT c_QString] voidT
  , just $ mkMethod "setHeaderLabels" [objT c_QStringList] voidT
  , just $ mkMethod "sortItems" [intT, enumT e_SortOrder] voidT
  , just $ mkConstMethod "topLevelItem" [intT] (ptrT $ objT c_QTreeWidgetItem)
  , just $ mkConstMethod "topLevelItemCount" [] intT
  -- TODO add more methods
  ]


signals :: [Signal]
signals =
  [ makeSignal c_QTreeWidget "currentItemChanged" c_ListenerPtrQTreeWidgetItemPtrQTreeWidgetItem
  , makeSignal c_QTreeWidget "itemActivated" c_ListenerPtrQTreeWidgetItemInt
  , makeSignal c_QTreeWidget "itemChanged" c_ListenerPtrQTreeWidgetItemInt
  , makeSignal c_QTreeWidget "itemClicked" c_ListenerPtrQTreeWidgetItemInt
  , makeSignal c_QTreeWidget "itemCollapsed" c_ListenerPtrQTreeWidgetItem
  , makeSignal c_QTreeWidget "itemDoubleClicked" c_ListenerPtrQTreeWidgetItemInt
  , makeSignal c_QTreeWidget "itemEntered" c_ListenerPtrQTreeWidgetItemInt
  , makeSignal c_QTreeWidget "itemExpanded" c_ListenerPtrQTreeWidgetItem
  , makeSignal c_QTreeWidget "itemPressed" c_ListenerPtrQTreeWidgetItemInt
  , makeSignal c_QTreeWidget "itemSelectionChanged" c_Listener
  ]


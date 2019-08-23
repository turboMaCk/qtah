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
  mkMethod',
  mkProp,
  )

import Foreign.Hoppy.Generator.Types (intT, objT, ptrT, voidT, enumT, constT, refT, bitspaceT, boolT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QList (c_QListQTreeWidgetItem)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QTreeWidgetItem (c_QTreeWidgetItem)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_SortOrder)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractItemView (e_ScrollHint)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  c_Listener,
  c_ListenerPtrQTreeWidgetItem,
  c_ListenerPtrQTreeWidgetItemInt,
  c_ListenerPtrQTreeWidgetItemPtrQTreeWidgetItem,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QTreeView (c_QTreeView)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Interface.Core.QItemSelectionModel (bs_SelectionFlags, c_QItemSelectionModel)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (bs_MatchFlags, bs_DropActions)

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
  , test (qtVersion >= [4, 1]) $ mkMethod "addTopLevelItem" [ptrT $ objT c_QTreeWidgetItem] voidT
  , just $ mkMethod "addTopLevelItems" [refT $ constT $ objT c_QListQTreeWidgetItem] voidT
  , just $ mkMethod' "closePersistentEditor" "closePersistentEditor" [ptrT $ objT c_QTreeWidgetItem] voidT
  , just $ mkMethod' "closePersistentEditor" "closePersistentEditorWithColumn" [ptrT $ objT c_QTreeWidgetItem, intT] voidT
  , just $ mkProp "columnCount" intT
  , test (qtVersion >= [4, 1]) $ mkConstMethod "currentColumn" [] intT
  , just $ mkConstMethod "currentItem" [] (ptrT $ objT c_QTreeWidgetItem)
  , just $ mkMethod' "editItem" "editItem" [ptrT $ objT c_QTreeWidgetItem] voidT
  , just $ mkMethod' "editItem" "editItemWithColumn" [ptrT $ objT c_QTreeWidgetItem, intT] voidT
  , just $ mkConstMethod' "findItems" "findItems" [objT c_QString, bitspaceT bs_MatchFlags] $ objT c_QListQTreeWidgetItem
  , just $ mkConstMethod' "findItems" "findItemsWithColumn" [objT c_QString, bitspaceT bs_MatchFlags, intT] $ objT c_QListQTreeWidgetItem
  , just $ mkConstMethod "headerItem" [] (ptrT $ objT c_QTreeWidgetItem)
  , just $ mkConstMethod "indexOfTopLevelItem" [ptrT $ objT c_QTreeWidgetItem] intT
  , just $ mkMethod "insertTopLevelItem" [intT, ptrT $ objT c_QTreeWidgetItem] voidT
  , test (qtVersion >= [4, 1]) $ mkMethod "insertTopLevelItems" [intT, refT $ constT $ objT c_QListQTreeWidgetItem] voidT
  , test (qtVersion >= [4, 2]) $ mkConstMethod "invisibleRootItem" [] (ptrT $ objT c_QTreeWidgetItem)
  , test (qtVersion >= [5, 10]) $ mkConstMethod' "isPersistentEditorOpen" "isPersistentEditorOpen" [ptrT $ objT c_QTreeWidgetItem] boolT
  , test (qtVersion >= [5, 10]) $ mkConstMethod' "isPersistentEditorOpen" "isPersistentEditorOpenWithColumn" [ptrT $ objT c_QTreeWidgetItem, intT] boolT
  , test (qtVersion >= [4, 3]) $ mkConstMethod "itemAbove" [constT $ ptrT $ objT c_QTreeWidgetItem] (ptrT $ objT c_QTreeWidgetItem)
  , just $ mkConstMethod' "itemAt" "itemAt" [constT $ objT c_QPoint] (ptrT $ objT c_QTreeWidgetItem)
  , just $ mkConstMethod' "itemAt" "itemAtWithCoordinates" [intT, intT] (ptrT $ objT c_QTreeWidgetItem)
  , test (qtVersion >= [4, 3]) $ mkConstMethod "itemBelow" [constT $ ptrT $ objT c_QTreeWidgetItem] (ptrT $ objT c_QTreeWidgetItem)
  -- TODO QTreeWidgetItem *QTreeWidget::itemFromIndex(const QModelIndex &index) const
  , test (qtVersion >= [4, 1]) $ mkConstMethod "itemWidget" [ptrT $ objT c_QTreeWidgetItem, intT] $ ptrT $ objT c_QWidget
  , just $ mkMethod' "openPersistentEditor" "openPersistentEditor" [ptrT $ objT c_QTreeWidgetItem] voidT
  , just $ mkMethod' "openPersistentEditor" "openPersistentEditorWithColumn" [ptrT $ objT c_QTreeWidgetItem, intT] voidT
  , test (qtVersion >= [4, 3]) $ mkMethod "removeItemWidget" [ptrT $ objT c_QTreeWidgetItem, intT] voidT
  , just $ mkConstMethod "selectedItems" [] $ objT c_QListQTreeWidgetItem
  , just $ mkMethod' "setCurrentItem" "setCurrentItem" [ptrT $ objT c_QTreeWidgetItem] voidT
  , test (qtVersion >= [4, 1]) $ mkMethod' "setCurrentItem" "setCurrentItemWithColumn" [ptrT $ objT c_QTreeWidgetItem, intT] voidT
  , test (qtVersion >= [4, 4]) $ mkMethod' "setCurrentItem" "setCurrentItemWithColFlags" [ptrT $ objT c_QTreeWidgetItem, intT, bitspaceT bs_SelectionFlags] voidT
  , just $ mkMethod "setHeaderItem" [ptrT $ objT c_QTreeWidgetItem] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod "setHeaderLabel" [objT c_QString] voidT
  , just $ mkMethod "setHeaderLabels" [objT c_QStringList] voidT
  , test (qtVersion >= [4, 1]) $ mkMethod "setItemWidget" [ptrT $ objT c_QTreeWidgetItem, intT, ptrT $ objT c_QWidget] voidT
  , just $ mkMethod "setSelectionModel" [ptrT $ objT c_QItemSelectionModel] voidT
  , test (qtVersion >= [4, 1]) $ mkConstMethod "sortColumn" [] intT
  , just $ mkMethod "sortItems" [intT, enumT e_SortOrder] voidT
  , just $ mkMethod "takeTopLevelItem" [intT] $ ptrT $ objT c_QTreeWidgetItem
  , just $ mkConstMethod "topLevelItem" [intT] (ptrT $ objT c_QTreeWidgetItem)
  , just $ mkConstMethod "topLevelItemCount" [] intT
  , just $ mkConstMethod "visualItemRect" [constT $ ptrT $ objT c_QTreeWidgetItem] $ objT c_QRect
  , just $ mkMethod "clear" [] voidT
  , just $ mkMethod "collapseItem" [constT $ ptrT $ objT c_QTreeWidgetItem] voidT
  , just $ mkMethod "expandItem" [constT $ ptrT $ objT c_QTreeWidgetItem] voidT
  , just $ mkMethod' "scrollToItem" "scrollToItem" [constT $ ptrT $ objT c_QTreeWidgetItem] voidT
  , just $ mkMethod' "scrollToItem" "scrollToItemWithHint" [constT $ ptrT $ objT c_QTreeWidgetItem, enumT e_ScrollHint] voidT
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


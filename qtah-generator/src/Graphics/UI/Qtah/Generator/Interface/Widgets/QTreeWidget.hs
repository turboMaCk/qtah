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
  -- modules
  aModule,
  itemModule,
  -- classes
  c_QTreeWidget,
  c_QTreeWidgetItem,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Class,
  CppEnum,
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  np,
  )
import Foreign.Hoppy.Generator.Types (intT, objT, ptrT, voidT, boolT, enumT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Graphics.UI.Qtah.Generator.Interface.Core.QVariant (c_QVariant)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_SortOrder)
import Graphics.UI.Qtah.Generator.Interface.Gui.QIcon (c_QIcon)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  listener,
  listenerPtrQTreeWidgetItem,
  listenerPtrQTreeWidgetItemInt,
  listenerPtrQTreeWidgetItemPtrQTreeWidgetItem,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QTreeView (c_QTreeView)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule :: AModule
aModule =
  AQtModule $
  makeQtModule ["Widgets", "QTreeWidget"] $
  qtExport c_QTreeWidget :
  map QtExportSignal signals

itemModule :: AModule
itemModule =
  AQtModule $
  makeQtModule ["Widgets", "QTreeWidgetItem"]
  [ qtExport c_QTreeWidgetItem
  , qtExport e_ChildIndicatorPolicy
  ]

c_QTreeWidget :: Class
c_QTreeWidget =
  addReqIncludes [includeStd "QTreeWidget"] $
  classSetEntityPrefix "" $
  makeClass (ident "QTreeWidget") Nothing [c_QTreeView] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , test (qtVersion >= [4, 1]) $
    mkMethod "addTopLevelItem" [ptrT $ objT c_QTreeWidgetItem] voidT
  , just $ mkConstMethod "currentItem" np (ptrT $ objT c_QTreeWidgetItem)
  , just $ mkConstMethod "headerItem" np (ptrT $ objT c_QTreeWidgetItem)
  , test (qtVersion >= [4, 2]) $
    mkConstMethod "invisibleRootItem" np (ptrT $ objT c_QTreeWidgetItem)
  , just $ mkMethod "setCurrentItem" [ptrT $ objT c_QTreeWidgetItem] voidT
  , just $ mkMethod "setHeaderItem" [ptrT $ objT c_QTreeWidgetItem] voidT
  , test (qtVersion >= [4, 2]) $
    mkMethod "setHeaderLabel" [objT c_QString] voidT
  , just $ mkMethod "setHeaderLabels" [objT c_QStringList] voidT
  , just $ mkMethod "sortItems" [intT, enumT e_SortOrder] voidT
  , just $ mkConstMethod "topLevelItem" [intT] (ptrT $ objT c_QTreeWidgetItem)
  , just $ mkConstMethod "topLevelItemCount" np intT
  -- TODO add more methods
  ]

c_QTreeWidgetItem :: Class
c_QTreeWidgetItem =
  addReqIncludes [includeStd "QTreeWidgetItem"] $
  classSetEntityPrefix "" $
  makeClass (ident "QTreeWidgetItem") Nothing [] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithType" [intT]
  , just $ mkCtor "newWithStrings" [objT c_QStringList]
  , just $ mkCtor "newWithStringsAndType" [objT c_QStringList, intT]
  , just $ mkCtor "newWithParentTree" [ptrT $ objT c_QTreeWidget]
  , just $ mkCtor "newWithParentTreeAndType" [ptrT $ objT c_QTreeWidget, intT]
  , just $ mkCtor "newWithParentTreeAndStrings" [ptrT $ objT c_QTreeWidget, objT c_QStringList]
  , just $ mkCtor "newWithParentTreeAndStringsAndType"
    [ptrT $ objT c_QTreeWidget, objT c_QStringList, intT]
  , just $ mkCtor "newWithParentItem" [ptrT $ objT c_QTreeWidgetItem]
  , just $ mkCtor "newWithParentItemAndType" [ptrT $ objT c_QTreeWidgetItem, intT]
  , just $ mkCtor "newWithParentItemAndStrings" [ptrT $ objT c_QTreeWidgetItem, objT c_QStringList]
  , just $ mkCtor "newWithParentItemAndStringsAndType"
    [ptrT $ objT c_QTreeWidgetItem, objT c_QStringList, intT]
  , just $ mkConstMethod "child" [intT] (ptrT $ objT c_QTreeWidgetItem)
  , just $ mkConstMethod "childCount" np intT
  , just $ mkConstMethod "childIndicatorPolicy" np (enumT e_ChildIndicatorPolicy)
  , just $ mkConstMethod "columnCount" np intT
  , just $ mkConstMethod' "data" "getData" [intT, intT] (objT c_QVariant)
  , test (qtVersion >= [4, 2]) $ mkConstMethod "isHidden" np boolT
  , just $ mkConstMethod "parent" np (ptrT $ objT c_QTreeWidgetItem)
  , just $ mkMethod "setChildIndicatorPolicy" [enumT e_ChildIndicatorPolicy] voidT
  , just $ mkMethod "setData" [intT, intT, objT c_QVariant] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod "setHidden" [boolT] voidT
  , just $ mkMethod "setIcon" [intT, objT c_QIcon] voidT
  , just $ mkMethod "setText" [intT, objT c_QString] voidT
  , just $ mkConstMethod "text" [intT] (objT c_QString)
  , just $ mkConstMethod' "type" "getType" np intT
  -- TODO add more methods
  ]

signals :: [Signal]
signals =
  [ makeSignal c_QTreeWidget "currentItemChanged" listenerPtrQTreeWidgetItemPtrQTreeWidgetItem
  , makeSignal c_QTreeWidget "itemActivated" listenerPtrQTreeWidgetItemInt
  , makeSignal c_QTreeWidget "itemChanged" listenerPtrQTreeWidgetItemInt
  , makeSignal c_QTreeWidget "itemClicked" listenerPtrQTreeWidgetItemInt
  , makeSignal c_QTreeWidget "itemCollapsed" listenerPtrQTreeWidgetItem
  , makeSignal c_QTreeWidget "itemDoubleClicked" listenerPtrQTreeWidgetItemInt
  , makeSignal c_QTreeWidget "itemEntered" listenerPtrQTreeWidgetItemInt
  , makeSignal c_QTreeWidget "itemExpanded" listenerPtrQTreeWidgetItem
  , makeSignal c_QTreeWidget "itemPressed" listenerPtrQTreeWidgetItemInt
  , makeSignal c_QTreeWidget "itemSelectionChanged" listener
  ]

e_ChildIndicatorPolicy :: CppEnum
e_ChildIndicatorPolicy =
  makeQtEnum (ident1 "QTreeWidgetItem" "ChildIndicatorPolicy") [includeStd "QTreeWidgetItem"]
  [ "ShowIndicator"
  , "DontShowIndicator"
  , "DontShowIndicatorWhenChildless"
  ]

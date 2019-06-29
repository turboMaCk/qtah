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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QTreeView (
  aModule,
  c_QTreeView,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Class,
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkCtor,
  mkMethod,
  mkMethod',
  mkConstMethod,
  mkProp,
  )
import Foreign.Hoppy.Generator.Types (boolT, intT, ptrT, objT, voidT, constT, refT, enumT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QModelIndex (c_QModelIndex)
import Graphics.UI.Qtah.Generator.Interface.Core.QVector (c_QVectorInt)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QAbstractItemModel (c_QAbstractItemModel)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractItemView (c_QAbstractItemView, e_ScrollHint)
import Graphics.UI.Qtah.Generator.Interface.Core.QItemSelectionModel (c_QItemSelectionModel)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_SortOrder)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  c_Listener,
  c_ListenerRefConstQModelIndex,
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule :: AModule
aModule =
  AQtModule $
  makeQtModule ["Widgets", "QTreeView"] $
  QtExport (ExportClass c_QTreeView) :
  map QtExportSignal signals

c_QTreeView :: Class
c_QTreeView =
  addReqIncludes [includeStd "QTreeView"] $
  classSetEntityPrefix "" $
  makeClass (ident "QTreeView") Nothing [c_QAbstractItemView] $
  collect
  [
  -- Properties
    test (qtVersion >= [4, 2]) $ mkProp "allColumnsShowFocus" boolT
  , test (qtVersion >= [4, 2]) $ mkBoolIsProp "animated"
  , test (qtVersion >= [4, 3]) $ mkProp "autoExpandDelay" intT
  , test (qtVersion >= [4, 4]) $ mkProp "expandsOnDoubleClick" boolT
  , test (qtVersion >= [4, 4]) $ mkBoolIsProp "headerHidden"
  , just $ mkProp "indentation" intT
  , just $ mkProp "itemsExpandable" boolT
  , just $ mkProp "rootIsDecorated" boolT
  , test (qtVersion >= [4, 2]) $ mkBoolIsProp "sortingEnabled"
  , just $ mkProp "uniformRowHeights" boolT
  , test (qtVersion >= [4, 3]) $ mkProp "wordWrap" boolT
  -- Public Functions
  , just $ mkCtor "new" []
  , just $ mkConstMethod "columnAt" [intT] intT
  , just $ mkConstMethod "columnViewportPosition" [intT] intT
  , just $ mkConstMethod "columnWidth" [intT] intT
  , just $ mkMethod' "dataChanged" "dataChanged" [refT $ constT $ objT c_QModelIndex, refT $ constT $ objT c_QModelIndex] voidT
  , just $ mkMethod' "dataChanged" "dataChangedWithRoles" [refT $ constT $ objT c_QModelIndex, refT $ constT $ objT c_QModelIndex, refT $ constT $ objT c_QVectorInt] voidT
  -- QHeaderView *QTreeView::header() const
  , just $ mkConstMethod "indexAbove" [refT $ constT $ objT c_QModelIndex] $ objT c_QModelIndex
  , just $ mkConstMethod "indexAt" [refT $ constT $ objT c_QPoint] $ objT c_QModelIndex
  , just $ mkConstMethod "indexBelow" [refT $ constT $ objT c_QModelIndex] $ objT c_QModelIndex
  , just $ mkConstMethod "isColumnHidden" [intT] boolT
  , just $ mkConstMethod "isExpanded" [refT $ constT $ objT c_QModelIndex] boolT
  , test (qtVersion >= [4, 3]) $ mkConstMethod "isFirstColumnSpanned" [intT, refT $ constT $ objT c_QModelIndex] boolT
  , just $ mkConstMethod "isRowHidden" [intT, refT $ constT $ objT c_QModelIndex] boolT
  , just $ mkMethod "keyboardSearch" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod "reset" [] voidT
  , just $ mkMethod' "scrollTo" "scrollTo" [refT $ constT $ objT c_QModelIndex] voidT
  , just $ mkMethod' "scrollTo" "scrollToWithHint" [refT $ constT $ objT c_QModelIndex, enumT e_ScrollHint] voidT
  , just $ mkMethod "setColumnHidden" [intT, boolT] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod "setColumnWidth" [intT, intT] voidT
  , just $ mkMethod "setExpanded" [refT $ constT $ objT c_QModelIndex, boolT] voidT
  , test (qtVersion >= [4, 3]) $ mkMethod "setFirstColumnSpanned" [intT, refT $ constT $ objT c_QModelIndex, boolT] voidT
  -- TODO void QTreeView::setHeader(QHeaderView *header)
  , just $ mkMethod "setModel" [ptrT $ objT c_QAbstractItemModel] voidT
  , just $ mkMethod "setRootIndex" [refT $ constT $ objT c_QModelIndex] voidT
  , just $ mkMethod "setRowHidden" [intT, refT $ constT $ objT c_QModelIndex, boolT] voidT
  , just $ mkMethod "setSelectionModel" [ptrT $ objT c_QItemSelectionModel] voidT
  , test (qtVersion >= [5, 2]) $ mkMethod "setTreePosition" [intT] voidT
  , just $ mkMethod "selectAll" [] voidT
  , just $ mkConstMethod "visualRect" [refT $ constT $ objT c_QModelIndex] $ objT c_QRect
  -- Public Slots
  , just $ mkMethod "collapse" [objT c_QModelIndex] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod "collapseAll" [] voidT
  , just $ mkMethod "expand" [objT c_QModelIndex] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod "expandAll" [] voidT
  , test (qtVersion >= [5, 13]) $ mkMethod' "expandRecursively" "expandRecursively" [refT $ constT $ objT c_QModelIndex] voidT
  , test (qtVersion >= [5, 13]) $ mkMethod' "expandRecursively" "expandRecursivelyWithDepth" [refT $ constT $ objT c_QModelIndex, intT] voidT
  , test (qtVersion >= [4, 3]) $ mkMethod "expandToDepth" [intT] voidT
  , just $ mkMethod "hideColumn" [intT] voidT
  , just $ mkMethod "resizeColumnToContents" [intT] voidT
  , just $ mkMethod "showColumn" [intT] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod "sortByColumn" [intT, enumT e_SortOrder] voidT
  ]

signals :: [Signal]
signals =
  [ makeSignal c_QTreeView "collapsed" c_ListenerRefConstQModelIndex
  , makeSignal c_QTreeView "expanded" c_ListenerRefConstQModelIndex
  ]

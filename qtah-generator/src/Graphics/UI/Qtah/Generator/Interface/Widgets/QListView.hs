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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QListView (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportEnum),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkCtor,
  mkProp,
  mkConstMethod,
  mkMethod,
  mkMethod',
  )
import Foreign.Hoppy.Generator.Types (boolT, intT, objT, ptrT, refT, voidT, enumT, constT, charT, bitspaceT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QModelIndex (c_QModelIndex)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractItemView (e_ScrollHint)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractItemView (c_QAbstractItemView)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Internal.Listener (c_ListenerRefConstQListQModelIndexVoid)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (bs_Alignment)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QListView"] $
  QtExport (ExportClass c_QListView) :
  map QtExportSignal signals ++
  [ QtExport $ ExportEnum e_Flow
  , QtExport $ ExportEnum e_LayoutMode
  , QtExport $ ExportEnum e_Movement
  , QtExport $ ExportEnum e_ResizeMode
  , QtExport $ ExportEnum e_ViewMode
  ]

c_QListView =
  addReqIncludes [includeStd "QListView"] $
  classSetEntityPrefix "" $
  makeClass (ident "QListView") Nothing [c_QAbstractItemView] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newFromParent" [ptrT $ objT c_QWidget]
  , test (qtVersion >= [4, 2]) $ mkProp "batchSize" intT
  , just $ mkMethod "clearPropertyFlags" [] voidT
  , just $ mkProp "flow" $ enumT e_Flow
  , just $ mkProp "gridSize" $ objT c_QSize
  , just $ mkBoolIsProp "wrapping"
  , test (qtVersion >= [5, 12]) $ mkProp "itemAlignment" $ bitspaceT bs_Alignment
  , just $ mkProp "layoutMode" $ enumT e_LayoutMode
  , just $ mkProp "modelColumn" intT
  , just $ mkProp "movement" $ enumT e_Movement
  , just $ mkProp "resizeMode" $ enumT e_ResizeMode
  , test (qtVersion >= [4, 3]) $ mkBoolIsProp "selectionRectVisible"
  , just $ mkProp "spacing" intT
  , test (qtVersion >= [4, 1]) $ mkProp "uniformItemSizes" boolT
  , just $ mkProp "viewMode" $ enumT e_ViewMode
  , test (qtVersion >= [4, 2]) $ mkProp "wordWrap" boolT

  , just $ mkConstMethod "indexAt" [refT $ constT $ objT c_QPoint] $ objT c_QModelIndex
  , just $ mkConstMethod "isRowHidden" [intT] boolT
  , just $ mkMethod' "scrollTo" "scrollTo" [refT $ constT $ objT c_QModelIndex] voidT
  , just $ mkMethod' "scrollTo" "scrollToWithHint" [refT $ constT $ objT c_QModelIndex, enumT e_ScrollHint] voidT
  , just $ mkMethod "setRowHidden" [intT, boolT] voidT
  , just $ mkMethod "visualRect" [refT $ constT $ objT c_QModelIndex] $ objT c_QRect
  ]


signals = 
  collect
  [ test (qtVersion >= [4, 2]) $ makeSignal c_QListView "indexesMoved" c_ListenerRefConstQListQModelIndexVoid ]


e_Flow = 
  makeQtEnum (ident1 "QListView" "Flow") [includeStd "QListView"]
  [ (0, ["left", "to", "right"])
  , (1, ["top", "to", "bottom"])
  ]

e_LayoutMode =
  makeQtEnum (ident1 "QListView" "LayoutMode") [includeStd "QListView"]
  [ (0, ["single", "pass"])
  , (1, ["batched"])
  ]

e_Movement =
  makeQtEnum (ident1 "QListView" "Movement") [includeStd "QListView"]
  [ (0, ["static"])
  , (1, ["free"])
  , (2, ["snap"])
  ]

e_ResizeMode =
  makeQtEnum (ident1 "QListView" "ResizeMode") [includeStd "QListView"]
  [ (0, ["fixed"])
  , (1, ["adjust"])
  ]

e_ViewMode =
  makeQtEnum (ident1 "QListView" "ViewMode") [includeStd "QListView"]
  [ (0, ["list", "mode"])
  , (1, ["icon", "mode"])
  ]



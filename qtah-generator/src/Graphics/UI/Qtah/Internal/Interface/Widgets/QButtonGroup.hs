-- This file is part of Qtah.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QButtonGroup (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Type (TBool, TInt, TObj, TObjToHeap, TPtr, TVoid),
  addReqIncludes,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  mkProps,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QList (c_QListQAbstractButton)
import Graphics.UI.Qtah.Internal.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Internal.Interface.Listener (
  c_ListenerInt,
  c_ListenerIntBool,
  c_ListenerPtrQAbstractButton,
  c_ListenerPtrQAbstractButtonBool,
  )
import Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractButton (c_QAbstractButton)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QButtonGroup"] $
  (QtExport $ ExportClass c_QButtonGroup) :
  map QtExportSignal signals

c_QButtonGroup =
  addReqIncludes [includeStd "QButtonGroup"] $
  makeClass (ident "QButtonGroup") Nothing [c_QObject]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QObject]
  ] $
  collect
  [ just $ mkMethod' "addButton" "addButton" [TPtr $ TObj c_QAbstractButton] TVoid
  , just $ mkMethod' "addButton" "addButtonWithId" [TPtr $ TObj c_QAbstractButton, TInt] TVoid
  , test (qtVersion >= [4, 1]) $ mkConstMethod "button" [TInt] $ TPtr $ TObj c_QAbstractButton
  , just $ mkConstMethod' "buttons" "buttonsNew" [] $ TObjToHeap c_QListQAbstractButton
  , just $ mkConstMethod "checkedButton" [] $ TPtr $ TObj c_QAbstractButton
  , test (qtVersion >= [4, 1]) $ mkConstMethod "checkedId" [] TInt
  , test (qtVersion >= [4, 1]) $ mkConstMethod "id" [TPtr $ TObj c_QAbstractButton] TInt
  , just $ mkMethod "removeButton" [TPtr $ TObj c_QAbstractButton] TVoid
  , test (qtVersion >= [4, 1]) $ mkMethod "setId" [TPtr $ TObj c_QAbstractButton, TInt] TVoid
  ] ++
  mkProps
  [ mkProp "exclusive" TBool
  ]

signals =
  collect
  [ just $ makeSignal c_QButtonGroup "buttonClicked" c_ListenerPtrQAbstractButton
  , just $ makeSignal c_QButtonGroup "buttonClickedId" c_ListenerInt
  , test (qtVersion >= [4, 2]) $
    makeSignal c_QButtonGroup "buttonPressed" c_ListenerPtrQAbstractButton
  , test (qtVersion >= [4, 2]) $ makeSignal c_QButtonGroup "buttonPressedId" c_ListenerInt
  , test (qtVersion >= [4, 2]) $
    makeSignal c_QButtonGroup "buttonReleased" c_ListenerPtrQAbstractButton
  , test (qtVersion >= [4, 2]) $ makeSignal c_QButtonGroup "buttonReleasedId" c_ListenerInt
  , test (qtVersion >= [5, 2]) $
    makeSignal c_QButtonGroup "buttonToggled" c_ListenerPtrQAbstractButtonBool
  , test (qtVersion >= [5, 2]) $
    makeSignal c_QButtonGroup "buttonToggledId" c_ListenerIntBool
  ]
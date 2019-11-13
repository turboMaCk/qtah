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

module Graphics.UI.Qtah.Generator.Interface.Core.QAbstractAnimation (
  aModule,
  c_QAbstractAnimation,
  e_DeletionPolicy,
  e_Direction,
  e_State,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportEnum),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkMethod',
  mkMethod
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Foreign.Hoppy.Generator.Types (boolT, enumT, intT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  c_ListenerInt,
  c_Listener,
  c_ListenerStateState,
  c_ListenerDirection,
  )
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QAbstractAnimation"] [4, 6] $
  QtExport (ExportClass c_QAbstractAnimation) :
  map QtExportSignal signals ++
  collect
  [ just $ QtExport $ ExportEnum e_DeletionPolicy
  , just $ QtExport $ ExportEnum e_Direction
  , just $ QtExport $ ExportEnum e_State
  ]

c_QAbstractAnimation =
  addReqIncludes [ includeStd "QAbstractAnimation" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QAbstractAnimation") Nothing [c_QObject] $
  collect
  [ just $ mkConstMethod "currentLoopTime" [] intT
  , just $ mkConstMethod "duration" [] intT
  --, just $ mkConstMethod "group" [] $ ptrT $ objT c_QAnimationGroup
  , just $ mkMethod "pause" [] voidT
  , just $ mkMethod "resume" [] voidT
  , just $ mkMethod "setPaused" [boolT] voidT
  , just $ mkMethod' "start" "start" [] voidT
  , just $ mkMethod' "start" "startWithDeletionPolicy" [enumT e_DeletionPolicy] voidT
  , just $ mkMethod "stop" [] voidT
  , just $ mkConstMethod "totalDuration" [] intT

    -- TODO
  ]

signals :: [Signal]
signals =
  collect
  [ just $ makeSignal c_QAbstractAnimation "currentLoopChanged" c_ListenerInt
  , just $ makeSignal c_QAbstractAnimation "directionChanged" c_ListenerDirection
  , just $ makeSignal c_QAbstractAnimation "finished" c_Listener
  , just $ makeSignal c_QAbstractAnimation "stateChanged" c_ListenerStateState
  ]

e_DeletionPolicy =
  makeQtEnum (ident1 "QAbstractAnimation" "DeletionPolicy") [includeStd "QAbstractAnimation"]
  [ (0, ["keep", "when", "stopped"])
  , (1, ["delete", "when", "stopped"])
  ]

e_Direction =
  makeQtEnum (ident1 "QAbstractAnimation" "Direction") [includeStd "QAbstractAnimation"]
  [ (0, ["forward"])
  , (1, ["backward"])
  ]

e_State =
  makeQtEnum (ident1 "QAbstractAnimation" "State") [includeStd "QAbstractAnimation"]
  [ (0, ["stopped"])
  , (1, ["paused"])
  , (2, ["running"])
  ]

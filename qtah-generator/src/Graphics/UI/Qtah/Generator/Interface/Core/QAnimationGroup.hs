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

module Graphics.UI.Qtah.Generator.Interface.Core.QAnimationGroup (
  aModule,
  c_QAnimationGroup,
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
import Graphics.UI.Qtah.Generator.Interface.Core.QAbstractAnimation (c_QAbstractAnimation, e_State, e_Direction)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Foreign.Hoppy.Generator.Types (intT, voidT, enumT, bitspaceT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QAnimationGroup"] [4, 6] $
  [QtExport $ ExportClass c_QAnimationGroup]

c_QAnimationGroup =
  addReqIncludes [ includeStd "QAnimationGroup" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QAnimationGroup") Nothing [c_QAbstractAnimation] $
  collect
  [ just $ mkMethod "addAnimation" [ptrT $ objT c_QAbstractAnimation] voidT
  , just $ mkConstMethod "animationAt" [intT] $ ptrT $ objT c_QAbstractAnimation
  , just $ mkConstMethod "animationCount" [] intT
  , just $ mkMethod "clear" [] voidT
  , just $ mkConstMethod "indexOfAnimation" [ptrT $ objT c_QAbstractAnimation] intT
  , just $ mkMethod "insertAnimation" [intT, ptrT $ objT c_QAbstractAnimation] voidT
  , just $ mkMethod "removeAnimation" [ptrT $ objT c_QAbstractAnimation] voidT
  , just $ mkMethod "takeAnimation" [intT] $ ptrT $ objT c_QAbstractAnimation
  , just $ mkConstMethod "duration" [] intT
  ]

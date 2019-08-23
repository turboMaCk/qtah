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

module Graphics.UI.Qtah.Generator.Interface.Core.QVariantAnimation (
  aModule,
  c_QVariantAnimation,
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
  mkMethod,
  mkProp
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QAbstractAnimation (c_QAbstractAnimation)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QVariant (c_QVariant)
import Foreign.Hoppy.Generator.Types (intT, voidT, enumT, bitspaceT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Internal.Listener (c_ListenerQVariant)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qreal)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QVariantAnimation"] [4, 6] $
  (QtExport $ ExportClass c_QVariantAnimation) :
  map QtExportSignal signals

c_QVariantAnimation =
  addReqIncludes [ includeStd "QVariantAnimation" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QVariantAnimation") Nothing [c_QAbstractAnimation] $
  collect
  [ just $ mkProp "duration" intT
  , just $ mkProp "endValue" $ objT c_QVariant
  , just $ mkProp "startValue" $ objT c_QVariant
  , just $ mkConstMethod "keyValueAt" [qreal] $ objT c_QVariant
  -- TODO QVariantAnimation::KeyValues QVariantAnimation::keyValues() const
  , just $ mkMethod "setKeyValueAt" [qreal, refT $ constT $ objT c_QVariant] voidT
  -- TODO just $ mkMethod "setKeyValueAt" [qreal, refT $ constT $ objT c_QVariant] $ voidT
  ]

signals :: [Signal]
signals =
  collect
  [ just $ makeSignal c_QVariantAnimation "valueChanged" c_ListenerQVariant
  ]

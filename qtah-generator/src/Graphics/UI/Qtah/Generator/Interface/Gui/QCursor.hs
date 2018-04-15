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

module Graphics.UI.Qtah.Generator.Interface.Gui.QCursor (
  aModule,
  c_QCursor,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetConversionToGc,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkCtor,
  mkStaticMethod,
  mkStaticMethod',
  np,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (enumT, intT, objT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_CursorShape)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QCursor"]
  [ qtExport c_QCursor ]

c_QCursor =
  addReqIncludes [includeStd "QCursor"] $
  classSetConversionToGc $
  classAddFeatures (if qtVersion >= [5, 11]
                    then [Assignable, Copyable, Equatable]
                    else [Assignable, Copyable]) $
  classSetEntityPrefix "" $
  makeClass (ident "QCursor") Nothing [] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithCursorShape" [enumT e_CursorShape]
    -- TODO Methods.

    -- Static methods.
  , just $ mkStaticMethod "pos" np $ objT c_QPoint
    -- TODO QPoint pos(const QScreen*)
  , just $ mkStaticMethod "setPos" [objT c_QPoint] voidT
  , just $ mkStaticMethod' "setPos" "setPosRaw" [intT, intT] voidT
    -- TODO void setPos(QScreen*, int, int)
    -- TODO void setPos(QScreen*, const QPoint&)
  ]

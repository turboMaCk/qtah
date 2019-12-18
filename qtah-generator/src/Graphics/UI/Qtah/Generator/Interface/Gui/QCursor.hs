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
  Export (ExportClass),
  addReqIncludes,
  classSetConversionToGc,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkCtor,
  mkStaticMethod,
  mkStaticMethod',
  mkConstMethod,
  mkMethod,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (enumT, intT, objT, voidT, refT, constT, ptrT, enumT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Gui.QPixmap (c_QPixmap)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Gui.QBitmap (c_QBitmap)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_CursorShape)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QCursor"]
  [ QtExport $ ExportClass c_QCursor ]

c_QCursor =
  addReqIncludes [includeStd "QCursor"] $
  classSetConversionToGc $
  classAddFeatures (if qtVersion >= [5, 10]
                    then [Assignable, Copyable, Equatable]
                    else [Assignable, Copyable]) $
  classSetEntityPrefix "" $
  makeClass (ident "QCursor") Nothing [] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithCursorShape" [enumT e_CursorShape]
  , just $ mkCtor "newWithPixmap" [refT $ constT $ objT c_QPixmap]
  , just $ mkCtor "newWithPixmapAndHotSpot" [refT $ constT $ objT c_QPixmap, intT, intT]
  , just $ mkCtor "newWithBitmap" [refT $ constT $ objT c_QBitmap, refT $ constT $ objT c_QBitmap]
  , just $ mkCtor "newWithBitmapAndHotSpot" [refT $ constT $ objT c_QBitmap, refT $ constT $ objT c_QBitmap, intT, intT]
  , just $ mkConstMethod "bitmap" [] $ ptrT $ constT $ objT c_QBitmap
  , just $ mkConstMethod "hotSpot" [] $ objT c_QPoint
  , just $ mkConstMethod "mask" [] $ ptrT $ constT $ objT c_QBitmap
  , just $ mkConstMethod "pixmap" [] $ objT c_QPixmap
  , just $ mkMethod "setShape" [enumT e_CursorShape] voidT
  , just $ mkConstMethod "shape" [] $ enumT e_CursorShape
  , test (qtVersion >= [5, 7]) $ mkMethod "swap" [refT $ objT c_QCursor] voidT

    -- Static methods.
  , just $ mkStaticMethod "pos" [] $ objT c_QPoint
    -- TODO QPoint pos(const QScreen*)
  , just $ mkStaticMethod "setPos" [objT c_QPoint] voidT
  , just $ mkStaticMethod' "setPos" "setPosRaw" [intT, intT] voidT
    -- TODO void setPos(QScreen*, int, int)
    -- TODO void setPos(QScreen*, const QPoint&)
  ]

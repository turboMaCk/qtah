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

module Graphics.UI.Qtah.Generator.Interface.Gui.QPainter (
  aModule,
  c_QPainter,
  e_RenderHint,
  fl_RenderHints,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkCtor,
  mkMethod,
  mkMethod',
  np,
  )
import Foreign.Hoppy.Generator.Types (enumT, intT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Flags (flagsT)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_GlobalColor, fl_ImageConversionFlags)
import Graphics.UI.Qtah.Generator.Interface.Gui.QImage (c_QImage)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPaintDevice (c_QPaintDevice)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QPainter"]
  [ qtExport c_QPainter
  , qtExport e_RenderHint
  , qtExport fl_RenderHints
  ]

c_QPainter =
  addReqIncludes [includeStd "QPainter"] $
  classSetEntityPrefix "" $
  makeClass (ident "QPainter") Nothing [] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithDevice" [ptrT $ objT c_QPaintDevice]
  , just $ mkMethod' "drawImage" "drawImageAtRaw" [intT, intT, objT c_QImage] voidT
  , just $ mkMethod' "drawImage" "drawImageAtRawAll"
    [intT, intT, objT c_QImage, intT, intT, intT, intT, flagsT fl_ImageConversionFlags] voidT
  , just $ mkMethod' "fillRect" "fillRectWithGlobalColor" [objT c_QRect, enumT e_GlobalColor] voidT
  , just $ mkMethod "setRenderHint" [enumT e_RenderHint] voidT
  ]

(e_RenderHint, fl_RenderHints) =
  makeQtEnumAndFlags (ident1 "QPainter" "RenderHint") "RenderHints" [includeStd "QPainter"]
  [ "Antialiasing"
  , "TextAntialiasing"
  , "SmoothPixmapTransform"
  , "HighQualityAntialiasing"
  , "NonCosmeticDefaultPen"
  , "Qt4CompatiblePainting"
  ]

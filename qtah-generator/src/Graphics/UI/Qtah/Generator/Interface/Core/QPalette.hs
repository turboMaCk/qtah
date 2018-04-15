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

-- | Top-level bindings and bindings in the @Qt::@ namespace.
module Graphics.UI.Qtah.Generator.Interface.Core.QPalette (
  aModule,
  e_ColorRole
  ) where

import Foreign.Hoppy.Generator.Spec (Include, ident1, includeStd)
import Graphics.UI.Qtah.Generator.Module (AModule(AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

qPaletteInclude :: [Include]
qPaletteInclude = [includeStd "Qt", includeStd "QPalette"]

aModule :: AModule
aModule =
  AQtModule $
  makeQtModule ["Core", "QPalette"]
  [ qtExport e_ColorRole
  ]

-- TODO QPalette

e_ColorRole =
  makeQtEnum (ident1 "QPalette" "ColorRole") qPaletteInclude
  [ "Window"
  , "WindowText"
  , "Base"
  , "AlternateBase"
  , "ToolTipBase"
  , "ToolTipText"
  , "Text"
  , "Button"
  , "ButtonText"
  , "BrightText"
  ]


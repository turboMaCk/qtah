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
  c_QPalette,
  e_ColorRole,
  ) where

import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Graphics.UI.Qtah.Generator.Interface.Gui.QColor (c_QColor)
import Graphics.UI.Qtah.Generator.Interface.Gui.QBrush (c_QBrush)
import Foreign.Hoppy.Generator.Types (int64T, constT, objT, refT, voidT, boolT, enumT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportEnum),
  Include,
  includeStd,
  addReqIncludes,
  classSetConversionToGc,
  classSetEntityPrefix,
  ident,
  ident1,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod',
  mkMethod
  )
import Graphics.UI.Qtah.Generator.Module (AModule(AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_GlobalColor)

{-# ANN module "HLint: ignore Use camelCase" #-}

qPaletteInclude :: [Include]
qPaletteInclude = [includeStd "Qt", includeStd "QPalette"]

aModule :: AModule
aModule =
  AQtModule $
  makeQtModule ["Core", "QPalette"]
  [ QtExport $ ExportClass c_QPalette
  , QtExport $ ExportEnum e_ColorRole
  , QtExport $ ExportEnum e_ColorGroup
  ]

c_QPalette =
  addReqIncludes [includeStd "QPalette"] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QPalette") Nothing [] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithColor" [refT $ constT $ objT c_QColor]
  , just $ mkCtor "newWithColors" [refT $ constT $ objT c_QColor, refT $ constT $ objT c_QColor]
  , just $ mkCtor "newWithGlobalColor" [enumT e_GlobalColor]
  , just $ mkCtor "newWithBrushes" [refT $ constT $ objT c_QBrush, refT $ constT $ objT c_QBrush, refT $ constT $ objT c_QBrush,
    refT $ constT $ objT c_QBrush, refT $ constT $ objT c_QBrush, refT $ constT $ objT c_QBrush,
    refT $ constT $ objT c_QBrush, refT $ constT $ objT c_QBrush, refT $ constT $ objT c_QBrush]
  , just $ mkConstMethod "alternateBase" [] $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod "base" [] $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod "brightText" [] $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod' "brush" "brushWithGroup" [enumT e_ColorGroup, enumT e_ColorRole] $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod' "brush" "brush" [enumT e_ColorRole] $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod "button" [] $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod "buttonText" [] $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod "cacheKey" [] int64T
  , just $ mkConstMethod' "color" "colorWithGroup" [enumT e_ColorGroup, enumT e_ColorRole] $ refT $ constT $ objT c_QColor
  , just $ mkConstMethod' "color" "color" [enumT e_ColorRole] $ refT $ constT $ objT c_QColor
  , just $ mkConstMethod "currentColorGroup" [] $ enumT e_ColorGroup
  , just $ mkConstMethod "dark" [] $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod "highlight" [] $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod "highlightedText" [] $ refT $ constT $ objT c_QBrush
  , test (qtVersion >= [4, 2]) $ mkConstMethod "isBrushSet" [enumT e_ColorGroup, enumT e_ColorRole] boolT
  , just $ mkConstMethod "isCopyOf" [refT $ constT $ objT c_QPalette] boolT
  , just $ mkConstMethod "isEqual" [enumT e_ColorGroup, enumT e_ColorGroup] boolT
  , just $ mkConstMethod "light" [] $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod "link" [] $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod "linkVisited" [] $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod "mid" [] $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod "midlight" [] $ refT $ constT $ objT c_QBrush
  , test (qtVersion >= [5, 12]) $ mkConstMethod "placeholderText" [] $ refT $ constT $ objT c_QBrush
  , just $ mkMethod' "setBrush" "setBrush" [enumT e_ColorRole, refT $ constT $ objT c_QBrush] voidT
  , just $ mkMethod' "setBrush" "setBrushWithGroup" [enumT e_ColorGroup, enumT e_ColorRole, refT $ constT $ objT c_QBrush] voidT
  , just $ mkMethod' "setColor" "setColor" [enumT e_ColorRole, refT $ constT $ objT c_QColor] voidT
  , just $ mkMethod' "setColor" "setColorWithGroup" [enumT e_ColorGroup, enumT e_ColorRole, refT $ constT $ objT c_QColor] voidT
  , just $ mkMethod "setColorGroup" [enumT e_ColorGroup, refT $ constT $ objT c_QBrush, refT $ constT $ objT c_QBrush, refT $ constT $ objT c_QBrush,
    refT $ constT $ objT c_QBrush, refT $ constT $ objT c_QBrush, refT $ constT $ objT c_QBrush,
    refT $ constT $ objT c_QBrush, refT $ constT $ objT c_QBrush, refT $ constT $ objT c_QBrush] voidT
  , just $ mkMethod "setCurrentColorGroup" [enumT e_ColorGroup] voidT
  , just $ mkConstMethod "shadow" [] $ refT $ constT $ objT c_QBrush
  , test (qtVersion >= [5, 0]) $ mkMethod "swap" [refT $ objT c_QPalette] voidT
  , just $ mkConstMethod "text" [] $ refT $ constT $ objT c_QBrush
  , test (qtVersion >= [4, 4]) $ mkConstMethod "toolTipBase" [] $ refT $ constT $ objT c_QBrush
  , test (qtVersion >= [4, 4]) $ mkConstMethod "toolTipText" [] $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod "window" [] $ refT $ constT $ objT c_QBrush
  , just $ mkConstMethod "windowText" [] $ refT $ constT $ objT c_QBrush
  ]

e_ColorGroup =
  makeQtEnum (ident1 "QPalette" "ColorGroup") qPaletteInclude
  [ (1, ["disabled"])
  , (0, ["active"])
  , (2, ["inactive"])
 --, (0, ["normal"])
  ]

e_ColorRole =
  makeQtEnum (ident1 "QPalette" "ColorRole") qPaletteInclude
  [ (10, ["window"])
  , (0,  ["window", "text"])
  , (9,  ["base"])
  , (16, ["alternate", "base"])
  , (18, ["tool", "tip", "base"])
  , (19, ["tool", "tip", "text"])
  , (6,  ["text"])
  , (1,  ["button"])
  , (8,  ["button", "text"])
  , (7,  ["bright", "text"])
  ]

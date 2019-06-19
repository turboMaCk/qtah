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
module Graphics.UI.Qtah.Generator.Interface.Core.Types (
  aModule,
  qreal,
  qulonglong,
  qlonglong,
  gluint,
  e_AlignmentFlag,
  bs_Alignment,
  e_AnchorPoint,
  e_ApplicationState,
  bs_ApplicationStates,
  e_ArrowType,
  e_AspectRatioMode,
  e_Axis,
  e_BGMode,
  e_BrushStyle,
  e_CaseSensitivity,
  e_ApplicationAttribute,
  e_CheckState,
  e_ChecksumType,
  e_ClipOperation,
  e_ConnectionType,
  e_ContextMenuPolicy,
  e_CoordinateSystem,
  e_Corner,
  e_CursorMoveStyle,
  e_CursorShape,
  e_DateFormat,
  e_DayOfWeek,
  e_DockWidgetArea,
  bs_DockWidgetAreas,
  e_DropAction,
  bs_DropActions,
  e_Edge,
  bs_Edges,
  e_EnterKeyType,
  e_EventPriority,
  e_FillRule,
  e_FindChildOption,
  bs_FindChildOptions,
  e_FocusPolicy,
  e_FocusReason,
  e_GestureFlag,
  bs_GestureFlags,
  e_GestureState,
  bs_GestureStates,
  e_GestureType,
  e_HitTestAccuracy,
  e_ImageConversionFlag,
  bs_ImageConversionFlags,
  e_GlobalColor,
  e_ImageConversionFlag,
  bs_ImageConversionFlags,
  e_InputMethodHint,
  bs_InputMethodHints,
  e_InputMethodQuery,
  bs_InputMethodQueries,
  e_ItemDataRole,
  e_ItemFlag,
  bs_ItemFlags,
  e_ItemSelectionMode,
  e_ItemSelectionOperation,
  e_Key,
  e_KeyboardModifier,
  bs_KeyboardModifiers,
  e_LayoutDirection,
  e_MaskMode,
  e_MatchFlag,
  bs_MatchFlags,
  e_MouseButton,
  bs_MouseButtons,
  e_MouseEventFlag,
  e_MouseEventFlag_minVersion,
  bs_MouseEventFlags,
  e_MouseEventSource,
  e_MouseEventSource_minVersion,
  e_NativeGestureType,
  e_NavigationMode,
  e_Orientation,
  bs_Orientations,
  e_PenCapStyle,
  e_PenJoinStyle,
  e_PenStyle,
  e_ScreenOrientation,
  e_ScreenOrientation_minVersion,
  bs_ScreenOrientations,
  e_ScrollBarPolicy,
  e_ScrollPhase,
  e_ScrollPhase_minVersion,
  e_ShortcutContext,
  e_SizeHint,
  e_SizeMode,
  e_SortOrder,
  e_TabFocusBehavior,
  e_TextElideMode,
  e_TextFlag,
  e_TextFormat,
  e_TextInteractionFlag,
  bs_TextInteractionFlags,
  e_TileRule,
  e_TimeSpec,
  e_TimerType,
  e_ToolBarArea,
  bs_ToolBarAreas,
  e_ToolButtonStyle,
  e_TouchPointState,
  bs_TouchPointStates,
  e_TransformationMode,
  e_UIEffect,
  e_WhiteSpaceMode,
  e_WidgetAttribute,
  e_WindowFrameSection,
  e_WindowModality,
  e_WindowState,
  bs_WindowStates,
  e_WindowType,
  bs_WindowFlags,
  ) where

import Data.Bits ((.|.))
import Foreign.Hoppy.Generator.Spec (
  CppEnum,
  Export (ExportBitspace, ExportEnum, ExportFn),
  Include,
  Purity (Nonpure),
  Type,
  addReqIncludes,
  ident1,
  includeStd,
  makeFn,
  )
import Foreign.Hoppy.Generator.Types (doubleT, floatT, objT, word32T, word64T, int64T)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qrealFloat, qtVersion)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule :: AModule
aModule = AQtModule $ makeQtModule ["Core", "Types"] exports

exports :: [QtExport]
exports =
  QtExportSpecials :
  (map QtExport . collect)
  [ just $ ExportEnum e_AlignmentFlag
  , just $ ExportBitspace bs_Alignment
  , just $ ExportEnum e_AnchorPoint
  , test (qtVersion >= [5, 1]) $ ExportEnum e_ApplicationState
  , test (qtVersion >= [5, 1]) $ ExportBitspace bs_ApplicationStates
  , just $ ExportEnum e_ArrowType
  , just $ ExportEnum e_AspectRatioMode
  , just $ ExportEnum e_Axis
  , just $ ExportEnum e_BGMode
  , just $ ExportEnum e_BrushStyle
  , just $ ExportEnum e_CaseSensitivity
  , just $ ExportEnum e_CheckState
  , test (qtVersion >= [5, 9]) $ ExportEnum e_ChecksumType
  , just $ ExportEnum e_ClipOperation
  , just $ ExportEnum e_ConnectionType
  , just $ ExportEnum e_ContextMenuPolicy
  , test (qtVersion >= [4, 6]) $ ExportEnum e_CoordinateSystem 
  , just $ ExportEnum e_Corner
  , just $ ExportEnum e_CursorMoveStyle
  , just $ ExportEnum e_CursorShape
  , just $ ExportEnum e_DateFormat
  , just $ ExportEnum e_DayOfWeek
  , just $ ExportEnum e_DockWidgetArea
  , just $ ExportBitspace bs_DockWidgetAreas
  , just $ ExportEnum e_DropAction
  , just $ ExportBitspace bs_DropActions
  , test (qtVersion >= [5, 1]) $ ExportEnum e_Edge
  , test (qtVersion >= [5, 1]) $ ExportBitspace bs_Edges
  , test (qtVersion >= [5, 6]) $ ExportEnum e_EnterKeyType
  , just $ ExportEnum e_EventPriority
  , just $ ExportEnum e_FillRule
  , just $ ExportEnum e_FindChildOption
  , just $ ExportBitspace bs_FindChildOptions
  , just $ ExportEnum e_FocusPolicy
  , just $ ExportEnum e_FocusReason
  , just $ ExportEnum e_GestureFlag
  , just $ ExportBitspace bs_GestureFlags
  , test (qtVersion >= [4, 6]) $ ExportEnum e_GestureState
  , test (qtVersion >= [4, 6]) $ ExportBitspace bs_GestureStates
  , test (qtVersion >= [4, 6]) $ ExportEnum e_GestureType
  , just $ ExportEnum e_HitTestAccuracy
  , just $ ExportEnum e_ImageConversionFlag
  , just $ ExportBitspace bs_ImageConversionFlags
  , just $ ExportEnum e_GlobalColor
  , just $ ExportEnum e_ImageConversionFlag
  , just $ ExportBitspace bs_ImageConversionFlags
  , just $ ExportEnum e_InputMethodHint
  , just $ ExportBitspace bs_InputMethodHints
  , just $ ExportEnum e_InputMethodQuery
  , just $ ExportBitspace bs_InputMethodQueries
  , just $ ExportEnum e_ItemDataRole
  , just $ ExportEnum e_ItemFlag
  , just $ ExportBitspace bs_ItemFlags
  , just $ ExportEnum e_ItemSelectionMode
  , just $ ExportEnum e_ItemSelectionOperation
  , just $ ExportEnum e_Key
  , just $ ExportEnum e_KeyboardModifier
  , just $ ExportBitspace bs_KeyboardModifiers
  , just $ ExportEnum e_LayoutDirection
  , just $ ExportEnum e_MaskMode
  , just $ ExportEnum e_MatchFlag
  , just $ ExportBitspace bs_MatchFlags
  , just $ ExportEnum e_MouseButton
  , just $ ExportBitspace bs_MouseButtons
  , test (qtVersion >= e_MouseEventFlag_minVersion) $ ExportEnum e_MouseEventFlag
  , test (qtVersion >= e_MouseEventFlag_minVersion) $ ExportBitspace bs_MouseEventFlags
  , test (qtVersion >= e_MouseEventSource_minVersion) $ ExportEnum e_MouseEventSource
  , test (qtVersion >= [5,2]) $ ExportEnum e_NativeGestureType
  , just $ ExportEnum e_NavigationMode
  , just $ ExportEnum e_Orientation
  , just $ ExportBitspace bs_Orientations
  , just $ ExportEnum e_PenCapStyle
  , just $ ExportEnum e_PenJoinStyle
  , just $ ExportEnum e_PenStyle
  , test (qtVersion >= e_ScreenOrientation_minVersion) $ ExportEnum e_ScreenOrientation
  , test (qtVersion >= e_ScreenOrientation_minVersion) $ ExportBitspace bs_ScreenOrientations
  , just $ ExportEnum e_ScrollBarPolicy
  , test (qtVersion >= e_ScrollPhase_minVersion) $ ExportEnum e_ScrollPhase
  , just $ ExportEnum e_ShortcutContext
  , test (qtVersion >= [4, 4]) $ ExportEnum e_SizeHint
  , test (qtVersion >= [4, 4]) $ ExportEnum e_SizeMode
  , just $ ExportEnum e_SortOrder
  , test (qtVersion >= [5, 5]) $ ExportEnum e_TabFocusBehavior
  , just $ ExportEnum e_TextElideMode
  , just $ ExportEnum e_TextFlag
  , just $ ExportEnum e_TextFormat
  , just $ ExportEnum e_TextInteractionFlag
  , just $ ExportBitspace bs_TextInteractionFlags
  , test (qtVersion >= [4, 6]) $ ExportEnum e_TileRule
  , just $ ExportEnum e_TimeSpec
  , just $ ExportEnum e_TimerType
  , just $ ExportEnum e_ToolBarArea
  , just $ ExportBitspace bs_ToolBarAreas
  , just $ ExportEnum e_ToolButtonStyle
  , test (qtVersion >= [4, 6]) $ ExportEnum e_TouchPointState
  , test (qtVersion >= [4, 6]) $ ExportBitspace bs_TouchPointStates
  , just $ ExportEnum e_TransformationMode
  , just $ ExportEnum e_UIEffect
  , just $ ExportEnum e_WhiteSpaceMode
  , just $ ExportEnum e_WidgetAttribute
  , test (qtVersion >= [4, 4]) $ ExportEnum e_WindowFrameSection
  , just $ ExportEnum e_WindowModality
  , just $ ExportEnum e_ApplicationAttribute
  , just $ ExportEnum e_WindowState
  , just $ ExportBitspace bs_WindowStates
  , just $ ExportEnum e_WindowType
  , just $ ExportBitspace bs_WindowFlags
  , test (qtVersion < [5, 0]) $ ExportFn f_escape
  ]

qtInclude :: [Include]
qtInclude = [includeStd "Qt"]

qreal :: Type
qreal = if qrealFloat then floatT else doubleT

qlonglong :: Type
qlonglong = int64T

qulonglong :: Type
qulonglong = word64T

gluint :: Type
gluint = word32T

(e_AlignmentFlag, bs_Alignment) =
  makeQtEnumBitspace (ident1 "Qt" "AlignmentFlag") "Alignment" qtInclude
  [ -- Horizontal flags.
    (0x01, ["align", "left"])
  , (0x02, ["align", "right"])
  , (0x04, ["align", "h", "center"])
  , (0x08, ["align", "justify"])
    -- Vertical flags.
  , (0x20, ["align", "top"])
  , (0x40, ["align", "bottom"])
  , (0x80, ["align", "v", "center"])
    -- Useful in right-to-left mode.
  , (0x10, ["align", "absolute"])
  ]

e_AnchorPoint =
  makeQtEnum (ident1 "Qt" "AnchorPoint") qtInclude
  [ (0, ["anchor", "left"])
  , (1, ["anchor", "horizontal", "center"])
  , (2, ["anchor", "right"])
  , (3, ["anchor", "top"])
  , (4, ["anchor", "vertical", "center"])
  , (5, ["anchor", "bottom"])
  ]

(e_ApplicationState, bs_ApplicationStates) =
  makeQtEnumBitspace (ident1 "Qt" "ApplicationState") "ApplicationStates" qtInclude
  [ (0x00000000, ["application", "suspended"])
  , (0x00000001, ["application", "hidden"])
  , (0x00000002, ["application", "inactive"])
  , (0x00000004, ["application", "active"])
  ]

e_ArrowType =
  makeQtEnum (ident1 "Qt" "ArrowType") qtInclude
  [ (0, ["no", "arrow"])
  , (1, ["up", "arrow"])
  , (2, ["down", "arrow"])
  , (3, ["left", "arrow"])
  , (4, ["right", "arrow"])
  ]

e_AspectRatioMode =
  makeQtEnum (ident1 "Qt" "AspectRatioMode") qtInclude
  [ (0, ["ignore", "aspect", "ratio"])
  , (1, ["keep", "aspect", "ratio"])
  , (2, ["keep", "aspect", "ratio", "by", "expanding"])
  ]

e_Axis =
  makeQtEnum (ident1 "Qt" "Axis") qtInclude
  [ (0, ["x", "axis"])
  , (1, ["y", "axis"])
  , (2, ["z", "axis"])
  ]

e_BGMode =
  makeQtEnum (ident1 "Qt" "BGMode") qtInclude
  [ (0, ["transparent", "mode"])
  , (1, ["opaque", "mode"])
  ]

e_BrushStyle =
  makeQtEnum (ident1 "Qt" "BrushStyle") qtInclude
  [ (0, ["no", "brush"])
  , (1, ["solid", "pattern"])
  , (2, ["dense", "1", "pattern"])
  , (3, ["dense", "2", "pattern"])
  , (4, ["dense", "3", "pattern"])
  , (5, ["dense", "4", "pattern"])
  , (6, ["dense", "5", "pattern"])
  , (7, ["dense", "6", "pattern"])
  , (8, ["dense", "7", "pattern"])
  , (9, ["hor", "pattern"])
  , (10, ["ver", "pattern"])
  , (11, ["cross", "pattern"])
  , (12, ["b", "diag", "pattern"])
  , (13, ["f", "diag", "pattern"])
  , (14, ["diag", "cross", "pattern"])
  , (15, ["linear", "gradient", "pattern"])
  , (16, ["radial", "gradient", "pattern"])
  , (17, ["conical", "gradient", "pattern"])
  , (24, ["texture", "pattern"])
  ]

e_CaseSensitivity =
  makeQtEnum (ident1 "Qt" "CaseSensitivity") qtInclude
  [ (0, ["case", "insensitive"])
  , (1, ["case", "sensitive"])
  ]

e_CheckState =
  makeQtEnum (ident1 "Qt" "CheckState") qtInclude
  [ (0, ["unchecked"])
  , (1, ["partially", "checked"])
  , (2, ["checked"])
  ]

e_ChecksumType =
  makeQtEnum (ident1 "Qt" "ChecksumType") qtInclude
  [ (0, ["checksum", "iso3309"])
  , (1, ["checksum", "itu", "v41"])
  ]

e_ClipOperation =
  makeQtEnum (ident1 "Qt" "ClipOperation") qtInclude
  [ (0, ["no", "clip"])
  , (1, ["replace", "clip"])
  , (2, ["intersect", "clip"])
  ]

e_ConnectionType =
  makeQtEnum (ident1 "Qt" "ConnectionType") qtInclude $ 
  collect
  [ just (0, ["auto", "connection"])
  , just (1, ["direct", "connection"])
  , just (2, ["queued", "connection"])
  , just (3, ["blocking", "queued", "connection"])
  , test (qtVersion >= [4,6]) (0x80, ["unique", "connection"])
  ]


e_ContextMenuPolicy :: CppEnum
e_ContextMenuPolicy =
  makeQtEnum (ident1 "Qt" "ContextMenuPolicy") qtInclude
  [ (0, ["no", "context", "menu"])
  , (4, ["prevent", "context", "menu"])
  , (1, ["default", "context", "menu"])
  , (2, ["actions", "context", "menu"])
  , (3, ["custom", "context", "menu"])
  ]

e_CoordinateSystem =
  makeQtEnum (ident1 "Qt" "CoordinateSystem") qtInclude
  [ (0, ["device", "coordinates"])
  , (1, ["logical", "coordinates"])
  ]
  

e_Corner =
  makeQtEnum (ident1 "Qt" "Corner") qtInclude
  [ (0x00000, ["top", "left", "corner"])
  , (0x00001, ["top", "right", "corner"])
  , (0x00002, ["bottom", "left", "corner"])
  , (0x00003, ["bottom", "right", "corner"])
  ]

e_CursorMoveStyle =
  makeQtEnum (ident1 "Qt" "CursorMoveStyle") qtInclude
  [ (0, ["logical", "move", "style"])
  , (1, ["visual", "move", "style"])
  ]

e_CursorShape =
  makeQtEnum (ident1 "Qt" "CursorShape") qtInclude
  [ (0,  ["arrow", "cursor"])
  , (1,  ["up", "arrow", "cursor"])
  , (2,  ["cross", "cursor"])
  , (3,  ["wait", "cursor"])
  , (4,  ["i", "beam", "cursor"])
  , (5,  ["size", "ver", "cursor"])
  , (6,  ["size", "hor", "cursor"])
  , (7,  ["size", "b", "diag", "cursor"])
  , (8,  ["size", "f", "diag", "cursor"])
  , (9,  ["size", "all", "cursor"])
  , (10, ["blank", "cursor"])
  , (11, ["split", "v", "cursor"])
  , (12, ["split", "h", "cursor"])
  , (13, ["pointing", "hand", "cursor"])
  , (14, ["forbidden", "cursor"])
  , (15, ["whats", "this", "cursor"])
  , (16, ["busy", "cursor"])
  , (17, ["open", "hand", "cursor"])
  , (18, ["closed", "hand", "cursor"])
  , (19, ["drag", "copy", "cursor"])
  , (20, ["drag", "move", "cursor"])
  , (21, ["drag", "link", "cursor"])
  , (24, ["bitmap", "cursor"])
  ]

e_DateFormat =
  makeQtEnum (ident1 "Qt" "DateFormat") qtInclude $
  let textDate = 0
      iSODate = 1
      iSODateWithMs = 9
      systemLocaleShortDate = 4
      systemLocaleLongDate = 5
      defaultLocaleShortDate = 6
      defaultLocaleLongDate = 7
      systemLocaleDate = 2
      localeDate = 3
      localDate = systemLocaleDate
      rFC2822Date = 8
  in [(textDate, ["text", "date"])
     , (iSODate, ["i", "s", "o", "date"])
     , (iSODateWithMs, ["i", "s", "o", "date", "with", "ms"])
     , (systemLocaleShortDate, ["system", "locale", "short", "date"])
     , (systemLocaleLongDate, ["system", "locale", "long", "date"])
     , (defaultLocaleShortDate, ["default", "locale", "short", "date"])
     , (defaultLocaleLongDate, ["default", "locale", "long", "date"])
     , (systemLocaleDate, ["system", "locale", "date"])
     , (localeDate, ["locale", "date"])
     , (localDate, ["local", "date"])
     , (rFC2822Date, ["r", "f", "c2822", "date"])
     ]

e_DayOfWeek =
  makeQtEnum (ident1 "Qt" "DayOfWeek") qtInclude
  [ (0, ["monday"])
  , (1, ["tuesday"])
  , (2, ["wednesday"])
  , (3, ["thursday"])
  , (4, ["friday"])
  , (5, ["saturday"])
  , (6, ["sunday"])
  ]

(e_DockWidgetArea, bs_DockWidgetAreas) =
  makeQtEnumBitspace (ident1 "Qt" "DockWidgetArea") "DockWidgetAreas" qtInclude
  [ (0x0, ["no", "dock", "widget", "area"])
  , (0x1, ["left", "dock", "widget", "area"])
  , (0x2, ["right", "dock", "widget", "area"])
  , (0x4, ["top", "dock", "widget", "area"])
  , (0x8, ["bottom", "dock", "widget", "area"])
  , (0xf, ["all", "dock", "widget", "areas"])
  ]

(e_DropAction, bs_DropActions) =
  makeQtEnumBitspace (ident1 "Qt" "DropAction") "DropActions" qtInclude
  [ (0x0, ["ignore", "action"])
  , (0x1, ["copy", "action"])
  , (0x2, ["move", "action"])
  , (0x4, ["link", "action"])
  , (0xff, ["action", "mask"])
  , (0x8002, ["target", "move", "action"])
  ]

(e_Edge, bs_Edges) =
  makeQtEnumBitspace (ident1 "Qt" "Edge") "Edges" qtInclude
  [ (0x00001, ["top", "edge"])
  , (0x00002, ["left", "edge"])
  , (0x00004, ["right", "edge"])
  , (0x00008, ["bottom", "edge"])
  ]

e_EnterKeyType =
  makeQtEnum (ident1 "Qt" "EnterKeyType") qtInclude
  [ (0, ["enter", "key", "default"])
  , (1, ["enter", "key", "return"])
  , (2, ["enter", "key", "done"])
  , (3, ["enter", "key", "go"])
  , (4, ["enter", "key", "send"])
  , (5, ["enter", "key", "search"])
  , (6, ["enter", "key", "next"])
  , (7, ["enter", "key", "previous"])
  ]

e_EventPriority =
  makeQtEnum (ident1 "Qt" "EventPriority") qtInclude
  [ (1, ["high", "event", "priority"])
  , (0, ["normal", "event", "priority"])
  , (-1, ["low", "event", "priority"])
  ]

e_FillRule =
  makeQtEnum (ident1 "Qt" "FillRule") qtInclude
  [ (0, ["odd", "even", "fill"])
  , (1, ["winding", "fill"])
  ]

(e_FindChildOption, bs_FindChildOptions) =
  makeQtEnumBitspace (ident1 "Qt" "FindChildOption") "FindChildOptions" qtInclude
  [ (0x0, ["find", "direct", "children", "only"])
  , (0x1, ["find", "children", "recursively"])
  ]

e_FocusPolicy =
  makeQtEnum (ident1 "Qt" "FocusPolicy") qtInclude $
  let tabFocus = 0x1
      clickFocus = 0x2
      strongFocus = tabFocus .|. clickFocus .|. 0x8
      wheelFocus = strongFocus .|. 0x4
      noFocus = 0
  in [ (tabFocus, ["tab", "focus"])
      , (clickFocus, ["click", "focus"])
      , (strongFocus, ["strong", "focus"])
      , (wheelFocus, ["wheel", "focus"])
      , (noFocus, ["no", "focus"])
     ]

e_FocusReason =
  makeQtEnum (ident1 "Qt" "FocusReason") qtInclude
  [ (0, ["mouse", "focus", "reason"])
  , (1, ["tab", "focus", "reason"])
  , (2, ["backtab", "focus", "reason"])
  , (3, ["active", "window", "focus", "reason"])
  , (4, ["popup", "focus", "reason"])
  , (5, ["shortcut", "focus", "reason"])
  , (6, ["menu", "bar", "focus", "reason"])
  , (7, ["other", "focus", "reason"])
  ]

(e_GestureFlag, bs_GestureFlags) =
  makeQtEnumBitspace (ident1 "Qt" "GestureFlag") "GestureFlags" qtInclude $
  collect
  [ just (0x01, ["dont", "start", "gesture", "on", "children"])
  , just (0x02, ["receive", "partial", "gestures"])
  , test (qtVersion >= [4,7]) (0x04, ["ignored", "gestures", "propagate", "to", "parent"])
  ]

(e_GestureState, bs_GestureStates) =
  makeQtEnumBitspace (ident1 "Qt" "GestureState") "GestureStates" qtInclude
  [ (0, ["no", "gesture"])
  , (1, ["gesture", "started"])
  , (2, ["gesture", "updated"])
  , (3, ["gesture", "finished"])
  , (4, ["gesture", "canceled"])
  ]

e_GestureType =
  makeQtEnum (ident1 "Qt" "GestureType") qtInclude
  [ (1, ["tap", "gesture"])
  , (2, ["tap", "and", "hold", "gesture"])
  , (3, ["pan", "gesture"])
  , (4, ["pinch", "gesture"])
  , (5, ["swipe", "gesture"])
  , (0x0100, ["custom", "gesture"])
  ]

e_HitTestAccuracy =
  makeQtEnum (ident1 "Qt" "HitTestAccuracy") qtInclude
  [ (0, ["exact", "hit"])
  , (1, ["fuzzy", "hit"])
  ]

(e_ImageConversionFlag, bs_ImageConversionFlags) =
  makeQtEnumBitspace (ident1 "Qt" "ImageConversionFlag") "ImageConversionFlags" qtInclude
  [ (0x00000000, ["auto", "color"])
  , (0x00000003, ["color", "only"])
  , (0x00000002, ["mono", "only"])
  , (0x00000000, ["diffuse", "dither"])
  , (0x00000010, ["ordered", "dither"])
  , (0x00000020, ["threshold", "dither"])
  , (0x00000000, ["threshold", "alpha", "dither"])
  , (0x00000004, ["ordered", "alpha", "dither"])
  , (0x00000008, ["diffuse", "alpha", "dither"])
  , (0x00000040, ["prefer", "dither"])
  , (0x00000080, ["avoid", "dither"])
  , (0x00000000, ["auto", "dither"])
  , (0x00000100, ["no", "opaque", "detection"])
  , (0x00000200, ["no", "format", "conversion"])
  ]

e_GlobalColor =
  makeQtEnum (ident1 "Qt" "GlobalColor") qtInclude
  [ (3, ["white"])
  , (2, ["black"])
  , (7, ["red"])
  , (13, ["dark", "red"])
  , (8, ["green"])
  , (14, ["dark", "green"])
  , (9, ["blue"])
  , (15, ["dark", "blue"])
  , (10, ["cyan"])
  , (16, ["dark", "cyan"])
  , (11, ["magenta"])
  , (17, ["dark", "magenta"])
  , (12, ["yellow"])
  , (18, ["dark", "yellow"])
  , (5, ["gray"])
  , (4, ["dark", "gray"])
  , (6, ["light", "gray"])
  , (19, ["transparent"])
  , (0, ["color0"])
  , (1, ["color1"])
  ]

--(e_ImageConversionFlag, bs_ImageConversionFlags) =
--  makeQtEnumBitspace (ident1 "Qt" "ImageConversionFlag") "ImageConversionFlags" qtInclude
  -- TODO Lots of synonyms for 0x0.  Hoppy doesn't support these.
--  [ (0x0, ["auto"])  -- Not real, this is because Hoppy doesn't support duplicate enum values.
    -- Color/mono preference:
--  , (0x3, ["color", "only"])
--  , (0x2, ["mono", "only"])
    -- Dithering mode preference for RGB channels:
--  , (0x10, ["ordered", "dither"])
--  , (0x20, ["threshold", "dither"])
    -- Dithering mode preference for alpha channel:
--  , (0x4, ["ordered", "alpha", "dither"])
--  , (0x8, ["diffuse", "alpha", "dither"])
    -- Color matching versus dithering preference:
--  , (0x40, ["prefer", "dither"])
--  , (0x80, ["avoid", "dither"])
--  , (0x100, ["no", "opaque", "detection"])
--  , (0x200, ["no", "format", "conversion"])
--  ]

(e_InputMethodHint, bs_InputMethodHints) =
  makeQtEnumBitspace (ident1 "Qt" "InputMethodHint") "InputMethodHints" qtInclude
  [ (0x0, ["imh", "none"])
  , (0x1, ["imh", "hidden", "text"])
  , (0x2, ["imh", "sensitive", "data"])
  , (0x4, ["imh", "no", "auto", "uppercase"])
  , (0x8, ["imh", "prefer", "numbers"])
  , (0x10, ["imh", "prefer", "uppercase"])
  , (0x20, ["imh", "prefer", "lowercase"])
  , (0x40, ["imh", "no", "predictive", "text"])
  , (0x80, ["imh", "date"])
  , (0x100, ["imh", "time"])
  , (0x200, ["imh", "prefer", "latin"])
  , (0x400, ["imh", "multi", "line"])
  , (0x10000, ["imh", "digits", "only"])
  , (0x20000, ["imh", "formatted", "numbers", "only"])
  , (0x40000, ["imh", "uppercase", "only"])
  , (0x80000, ["imh", "lowercase", "only"])
  , (0x100000, ["imh", "dialable", "characters", "only"])
  , (0x200000, ["imh", "email", "characters", "only"])
  , (0x400000, ["imh", "url", "characters", "only"])
  , (0x800000, ["imh", "latin", "only"])
  , (0xffff0000, ["imh", "exclusive", "input", "mask"])
  ]

(e_InputMethodQuery, bs_InputMethodQueries) =
  makeQtEnumBitspace (ident1 "Qt" "InputMethodQuery") "InputMethodQueries" qtInclude $
  collect
  [ just (0x1, ["im", "enabled"])
  , just (0x2, ["im", "micro", "focus"])
  , just (0x2, ["im", "cursor", "rectangle"])
  , just (0x4, ["im", "font"])
  , just (0x8, ["im", "cursor", "position"])
  , just (0x10, ["im", "surrounding", "text"])
  , just (0x20, ["im", "current", "selection"])
  , just (0x40, ["im", "maximum", "text", "length"])
  , just (0x80, ["im", "anchor", "position"])
  , just (0x100, ["im", "hints"])
  , just (0x200, ["im", "preferred", "language"])
  , just (0x80000000, ["im", "platform", "data"])
  , just (0x400, ["im", "absolute", "position"])
  , just (0x800, ["im", "text", "before", "cursor"])
  , just (0x1000, ["im", "text", "after", "cursor"])
  , just (0x2000, ["im", "enter", "key", "type"])
  , test (qtVersion >= [5,7]) (0x4000, ["im", "anchor", "rectangle"])
  , just (0x8000, ["im", "input", "item", "clip", "rectangle"])
  ]

-- TODO Support for custom ItemDataRole values.
e_ItemDataRole =
  makeQtEnum (ident1 "Qt" "ItemDataRole") qtInclude $
  collect
  [ -- General-purpose roles:
    just (0, ["display", "role"])
  , just (1, ["decoration", "role"])
  , just (2, ["edit", "role"])
  , just (3, ["tool", "tip", "role"])
  , just (4, ["status", "tip", "role"])
  , just (5, ["whats", "this", "role"])
  , just (13, ["size", "hint", "role"])

    -- Roles describing appearance and metadata:
  , just (6, ["font", "role"])
  , just (7, ["text", "alignment", "role"])
  , just (8, ["background", "role"])
  , just (9, ["foreground", "role"])
  , just (10, ["check", "state", "role"])
  , test (qtVersion >= [4, 8]) (14, ["initial", "sort", "order", "role"])

    -- Accessibility roles:
  , just (11, ["accessible", "text", "role"])
  , just (12, ["accessible", "description", "role"])

    -- User roles:
  , just (0x0100, ["user", "role"])
  ]

(e_ItemFlag, bs_ItemFlags) =
  makeQtEnumBitspace (ident1 "Qt" "ItemFlag") "ItemFlags" qtInclude $
  collect
  [ just (0, ["no", "item", "flags"])
  , just (1, ["item", "is", "selectable"])
  , just (2, ["item", "is", "editable"])
  , just (4, ["item", "is", "drag", "enabled"])
  , just (8, ["item", "is", "drop", "enabled"])
  , just (16, ["item", "is", "user", "checkable"])
  , just (32, ["item", "is", "enabled"])
  , just (64, ["item", "is", "auto", "tristate"])
  , just (128, ["item", "never", "has", "children"])
  , test (qtVersion >= [5, 5]) (256, ["item", "is", "user", "tristate"])
  ]

e_ItemSelectionMode =
  makeQtEnum (ident1 "Qt" "ItemSelectionMode") qtInclude
  [ (0x0, ["contains", "item", "shape"])
  , (0x1, ["intersects", "item", "shape"])
  , (0x2, ["contains", "item", "bounding", "rect"])
  , (0x3, ["intersects", "item", "bounding", "rect"])
  ]

e_ItemSelectionOperation =
  makeQtEnum (ident1 "Qt" "ItemSelectionOperation") qtInclude
  [ (0, ["replace", "selection"])
  , (1, ["add", "to", "selection"])
  ]

e_Key =
  makeQtEnum (ident1 "Qt" "Key") qtInclude
  [ (0x01000000, ["key_", "escape"])
  , (0x01000001, ["key_", "tab"])
  , (0x01000002, ["key_", "backtab"])
  , (0x01000003, ["key_", "backspace"])
  , (0x01000004, ["key_", "return"])
  , (0x01000005, ["key_", "enter"])
  , (0x01000006, ["key_", "insert"])
  , (0x01000007, ["key_", "delete"])
  , (0x01000008, ["key_", "pause"])
  , (0x01000009, ["key_", "print"])
  , (0x0100000a, ["key_", "sys", "req"])
  , (0x0100000b, ["key_", "clear"])
  , (0x01000010, ["key_", "home"])
  , (0x01000011, ["key_", "end"])
  , (0x01000012, ["key_", "left"])
  , (0x01000013, ["key_", "up"])
  , (0x01000014, ["key_", "right"])
  , (0x01000015, ["key_", "down"])
  , (0x01000016, ["key_", "page", "up"])
  , (0x01000017, ["key_", "page", "down"])
  , (0x01000020, ["key_", "shift"])
  , (0x01000021, ["key_", "control"])
  , (0x01000022, ["key_", "meta"])
  , (0x01000023, ["key_", "alt"])
  , (0x01001103, ["key_", "alt", "gr"])
  , (0x01000024, ["key_", "caps", "lock"])
  , (0x01000025, ["key_", "num", "lock"])
  , (0x01000026, ["key_", "scroll", "lock"])
  , (0x01000030, ["key_", "f1"])
  , (0x01000031, ["key_", "f2"])
  , (0x01000032, ["key_", "f3"])
  , (0x01000033, ["key_", "f4"])
  , (0x01000034, ["key_", "f5"])
  , (0x01000035, ["key_", "f6"])
  , (0x01000036, ["key_", "f7"])
  , (0x01000037, ["key_", "f8"])
  , (0x01000038, ["key_", "f9"])
  , (0x01000039, ["key_", "f10"])
  , (0x0100003a, ["key_", "f11"])
  , (0x0100003b, ["key_", "f12"])
  , (0x0100003c, ["key_", "f13"])
  , (0x0100003d, ["key_", "f14"])
  , (0x0100003e, ["key_", "f15"])
  , (0x0100003f, ["key_", "f16"])
  , (0x01000040, ["key_", "f17"])
  , (0x01000041, ["key_", "f18"])
  , (0x01000042, ["key_", "f19"])
  , (0x01000043, ["key_", "f20"])
  , (0x01000044, ["key_", "f21"])
  , (0x01000045, ["key_", "f22"])
  , (0x01000046, ["key_", "f23"])
  , (0x01000047, ["key_", "f24"])
  , (0x01000048, ["key_", "f25"])
  , (0x01000049, ["key_", "f26"])
  , (0x0100004a, ["key_", "f27"])
  , (0x0100004b, ["key_", "f28"])
  , (0x0100004c, ["key_", "f29"])
  , (0x0100004d, ["key_", "f30"])
  , (0x0100004e, ["key_", "f31"])
  , (0x0100004f, ["key_", "f32"])
  , (0x01000050, ["key_", "f33"])
  , (0x01000051, ["key_", "f34"])
  , (0x01000052, ["key_", "f35"])
  , (0x01000053, ["key_", "super", "l"])
  , (0x01000054, ["key_", "super", "r"])
  , (0x01000055, ["key_", "menu"])
  , (0x01000056, ["key_", "hyper", "l"])
  , (0x01000057, ["key_", "hyper", "r"])
  , (0x01000058, ["key_", "help"])
  , (0x01000059, ["key_", "direction", "l"])
  , (0x01000060, ["key_", "direction", "r"])
  , (0x00000020, ["key_", "space"])  -- Aka Key_Any.
  , (0x00000021, ["key_", "exclam"])
  , (0x00000022, ["key_", "quote", "dbl"])
  , (0x00000023, ["key_", "number", "sign"])
  , (0x00000024, ["key_", "dollar"])
  , (0x00000025, ["key_", "percent"])
  , (0x00000026, ["key_", "ampersand"])
  , (0x00000027, ["key_", "apostrophe"])
  , (0x00000028, ["key_", "paren", "left"])
  , (0x00000029, ["key_", "paren", "right"])
  , (0x0000002a, ["key_", "asterisk"])
  , (0x0000002b, ["key_", "plus"])
  , (0x0000002c, ["key_", "comma"])
  , (0x0000002d, ["key_", "minus"])
  , (0x0000002e, ["key_", "period"])
  , (0x0000002f, ["key_", "slash"])
  , (0x00000030, ["key_", "0"])
  , (0x00000031, ["key_", "1"])
  , (0x00000032, ["key_", "2"])
  , (0x00000033, ["key_", "3"])
  , (0x00000034, ["key_", "4"])
  , (0x00000035, ["key_", "5"])
  , (0x00000036, ["key_", "6"])
  , (0x00000037, ["key_", "7"])
  , (0x00000038, ["key_", "8"])
  , (0x00000039, ["key_", "9"])
  , (0x0000003a, ["key_", "colon"])
  , (0x0000003b, ["key_", "semicolon"])
  , (0x0000003c, ["key_", "less"])
  , (0x0000003d, ["key_", "equal"])
  , (0x0000003e, ["key_", "greater"])
  , (0x0000003f, ["key_", "question"])
  , (0x00000040, ["key_", "at"])
  , (0x00000041, ["key_", "a"])
  , (0x00000042, ["key_", "b"])
  , (0x00000043, ["key_", "c"])
  , (0x00000044, ["key_", "d"])
  , (0x00000045, ["key_", "e"])
  , (0x00000046, ["key_", "f"])
  , (0x00000047, ["key_", "g"])
  , (0x00000048, ["key_", "h"])
  , (0x00000049, ["key_", "i"])
  , (0x0000004a, ["key_", "j"])
  , (0x0000004b, ["key_", "k"])
  , (0x0000004c, ["key_", "l"])
  , (0x0000004d, ["key_", "m"])
  , (0x0000004e, ["key_", "n"])
  , (0x0000004f, ["key_", "o"])
  , (0x00000050, ["key_", "p"])
  , (0x00000051, ["key_", "q"])
  , (0x00000052, ["key_", "r"])
  , (0x00000053, ["key_", "s"])
  , (0x00000054, ["key_", "t"])
  , (0x00000055, ["key_", "u"])
  , (0x00000056, ["key_", "v"])
  , (0x00000057, ["key_", "w"])
  , (0x00000058, ["key_", "x"])
  , (0x00000059, ["key_", "y"])
  , (0x0000005a, ["key_", "z"])
  , (0x0000005b, ["key_", "bracket", "left"])
  , (0x0000005c, ["key_", "backslash"])
  , (0x0000005d, ["key_", "bracket", "right"])
  , (0x0000005e, ["key_", "ascii", "circum"])
  , (0x0000005f, ["key_", "underscore"])
  , (0x00000060, ["key_", "quote", "left"])
  , (0x0000007b, ["key_", "brace", "left"])
  , (0x0000007c, ["key_", "bar"])
  , (0x0000007d, ["key_", "brace", "right"])
  , (0x0000007e, ["key_", "ascii", "tilde"])
  , (0x0a0, ["key_nobreakspace"])
  , (0x0a1, ["key_exclamdown"])
  , (0x0a2, ["key_cent"])
  , (0x0a3, ["key_sterling"])
  , (0x0a4, ["key_currency"])
  , (0x0a5, ["key_yen"])
  , (0x0a6, ["key_brokenbar"])
  , (0x0a7, ["key_section"])
  , (0x0a8, ["key_diaeresis"])
  , (0x0a9, ["key_copyright"])
  , (0x0aa, ["key_ordfeminine"])
  , (0x0ab, ["key_guillemotleft"])
  , (0x0ac, ["key_notsign"])
  , (0x0ad, ["key_hyphen"])
  , (0x0ae, ["key_registered"])
  , (0x0af, ["key_macron"])
  , (0x0b0, ["key_degree"])
  , (0x0b1, ["key_plusminus"])
  , (0x0b2, ["key_twosuperior"])
  , (0x0b3, ["key_threesuperior"])
  , (0x0b4, ["key_acute"])
  , (0x0b5, ["key_mu"])
  , (0x0b6, ["key_paragraph"])
  , (0x0b7, ["key_periodcentered"])
  , (0x0b8, ["key_cedilla"])
  , (0x0b9, ["key_onesuperior"])
  , (0x0ba, ["key_masculine"])
  , (0x0bb, ["key_guillemotright"])
  , (0x0bc, ["key_onequarter"])
  , (0x0bd, ["key_onehalf"])
  , (0x0be, ["key_threequarters"])
  , (0x0bf, ["key_questiondown"])
  , (0x0c0, ["key_", "agrave"])
  , (0x0c1, ["key_", "aacute"])
  , (0x0c2, ["key_", "acircumflex"])
  , (0x0c3, ["key_", "atilde"])
  , (0x0c4, ["key_", "adiaeresis"])
  , (0x0c5, ["key_", "aring"])
  , (0x0c6, ["key_", "a", "e"])
  , (0x0c7, ["key_", "ccedilla"])
  , (0x0c8, ["key_", "egrave"])
  , (0x0c9, ["key_", "eacute"])
  , (0x0ca, ["key_", "ecircumflex"])
  , (0x0cb, ["key_", "ediaeresis"])
  , (0x0cc, ["key_", "igrave"])
  , (0x0cd, ["key_", "iacute"])
  , (0x0ce, ["key_", "icircumflex"])
  , (0x0cf, ["key_", "idiaeresis"])
  , (0x0d0, ["key_", "e", "t", "h"])
  , (0x0d1, ["key_", "ntilde"])
  , (0x0d2, ["key_", "ograve"])
  , (0x0d3, ["key_", "oacute"])
  , (0x0d4, ["key_", "ocircumflex"])
  , (0x0d5, ["key_", "otilde"])
  , (0x0d6, ["key_", "odiaeresis"])
  , (0x0d7, ["key_multiply"])
  , (0x0d8, ["key_", "ooblique"])
  , (0x0d9, ["key_ugrave"])
  , (0x0da, ["key_", "uacute"])
  , (0x0db, ["key_", "ucircumflex"])
  , (0x0dc, ["key_", "udiaeresis"])
  , (0x0dd, ["key_", "yacute"])
  , (0x0de, ["key_", "t", "h", "o", "r", "n"])
  , (0x0df, ["key_ssharp"])
  , (0x0f7, ["key_division"])
  , (0x0ff, ["key_ydiaeresis"])
  , (0x01001120, ["key_", "multi_key"])
  , (0x01001137, ["key_", "codeinput"])
  , (0x0100113c, ["key_", "single", "candidate"])
  , (0x0100113d, ["key_", "multiple", "candidate"])
  , (0x0100113e, ["key_", "previous", "candidate"])
  , (0x0100117e, ["key_", "mode_switch"])
  , (0x01001121, ["key_", "kanji"])
  , (0x01001122, ["key_", "muhenkan"])
  , (0x01001123, ["key_", "henkan"])
  , (0x01001124, ["key_", "romaji"])
  , (0x01001125, ["key_", "hiragana"])
  , (0x01001126, ["key_", "katakana"])
  , (0x01001127, ["key_", "hiragana_", "katakana"])
  , (0x01001128, ["key_", "zenkaku"])
  , (0x01001129, ["key_", "hankaku"])
  , (0x01001129, ["key_", "zenkaku_", "hankaku"])
  , (0x0100112b, ["key_", "touroku"])
  , (0x0100112c, ["key_", "massyo"])
  , (0x0100112d, ["key_", "kana_", "lock"])
  , (0x0100112e, ["key_", "kana_", "shift"])
  , (0x0100112f, ["key_", "eisu_", "shift"])
  , (0x01001130, ["key_", "eisu_toggle"])
  , (0x01001131, ["key_", "hangul"])
  , (0x01001132, ["key_", "hangul_", "start"])
  , (0x01001133, ["key_", "hangul_", "end"])
  , (0x01001134, ["key_", "hangul_", "hanja"])
  , (0x01001135, ["key_", "hangul_", "jamo"])
  , (0x01001136, ["key_", "hangul_", "romaja"])
  , (0x01001138, ["key_", "hangul_", "jeonja"])
  , (0x01001139, ["key_", "hangul_", "banja"])
  , (0x0100113a, ["key_", "hangul_", "pre", "hanja"])
  , (0x0100113b, ["key_", "hangul_", "post", "hanja"])
  , (0x0100113f, ["key_", "hangul_", "special"])
  , (0x01001250, ["key_", "dead_", "grave"])
  , (0x01001251, ["key_", "dead_", "acute"])
  , (0x01001252, ["key_", "dead_", "circumflex"])
  , (0x01001253, ["key_", "dead_", "tilde"])
  , (0x01001254, ["key_", "dead_", "macron"])
  , (0x01001255, ["key_", "dead_", "breve"])
  , (0x01001256, ["key_", "dead_", "abovedot"])
  , (0x01001257, ["key_", "dead_", "diaeresis"])
  , (0x01001258, ["key_", "dead_", "abovering"])
  , (0x01001259, ["key_", "dead_", "doubleacute"])
  , (0x0100125a, ["key_", "dead_", "caron"])
  , (0x0100125b, ["key_", "dead_", "cedilla"])
  , (0x0100125c, ["key_", "dead_", "ogonek"])
  , (0x0100125d, ["key_", "dead_", "iota"])
  , (0x0100125e, ["key_", "dead_", "voiced_", "sound"])
  , (0x0100125f, ["key_", "dead_", "semivoiced_", "sound"])
  , (0x01001260, ["key_", "dead_", "belowdot"])
  , (0x01001261, ["key_", "dead_", "hook"])
  , (0x01001262, ["key_", "dead_", "horn"])
  , (0x01001263, ["key_", "dead_", "stroke"])
  , (0x01001264, ["key_", "dead_", "abovecomma"])
  , (0x01001265, ["key_", "dead_", "abovereversedcomma"])
  , (0x01001266, ["key_", "dead_", "doublegrave"])
  , (0x01001267, ["key_", "dead_", "belowring"])
  , (0x01001268, ["key_", "dead_", "belowmacron"])
  , (0x01001269, ["key_", "dead_", "belowtilde"])
  , (0x0100126b, ["key_", "dead_", "belowbreve"])
  , (0x0100126c, ["key_", "dead_", "belowdiaeresis"])
  , (0x0100126d, ["key_", "dead_", "invertedbreve"])
  , (0x0100126e, ["key_", "dead_", "belowcomma"])
  , (0x0100126f, ["key_", "dead_", "currency"])
  , (0x01001280, ["key_", "dead_a"])
  , (0x01001281, ["key_", "dead_", "a"])
  , (0x01001282, ["key_", "dead_e"])
  , (0x01001283, ["key_", "dead_", "e"])
  , (0x01001284, ["key_", "dead_i"])
  , (0x01001285, ["key_", "dead_", "i"])
  , (0x01001286, ["key_", "dead_o"])
  , (0x01001287, ["key_", "dead_", "o"])
  , (0x01001288, ["key_", "dead_u"])
  , (0x01001289, ["key_", "dead_", "u"])
  , (0x0100128a, ["key_", "dead_", "small_", "schwa"])
  , (0x0100128b, ["key_", "dead_", "capital_", "schwa"])
  , (0x0100128c, ["key_", "dead_", "greek"])
  , (0x01001290, ["key_", "dead_", "lowline"])
  , (0x01001291, ["key_", "dead_", "aboveverticalline"])
  , (0x01001292, ["key_", "dead_", "belowverticalline"])
  , (0x01001293, ["key_", "dead_", "longsolidusoverlay"])
    -- TODO Additional Qt::Key_* constants.
  ]

(e_KeyboardModifier, bs_KeyboardModifiers) =
  makeQtEnumBitspace (ident1 "Qt" "KeyboardModifier") "KeyboardModifiers" qtInclude
  [ (0x00000000, ["no", "modifier"])
  , (0x02000000, ["shift", "modifier"])
  , (0x04000000, ["control", "modifier"])
  , (0x08000000, ["alt", "modifier"])
  , (0x10000000, ["meta", "modifier"])
  , (0x20000000, ["keypad", "modifier"])
  , (0x40000000, ["group", "switch", "modifier"])
  ]

e_LayoutDirection =
  makeQtEnum (ident1 "Qt" "LayoutDirection") qtInclude
  [ (0, ["left", "to", "right"])
  , (1, ["right", "to", "left"])
  , (2, ["layout", "direction", "auto"])
  ]

e_MaskMode =
  makeQtEnum (ident1 "Qt" "MaskMode") qtInclude
  [ (0, ["mask", "in", "color"])
  , (1, ["mask", "out", "color"])
  ]

(e_MatchFlag, bs_MatchFlags) =
  makeQtEnumBitspace (ident1 "Qt" "MatchFlag") "MatchFlags" qtInclude
  [ ( 0, ["match", "exactly"])
  , ( 8, ["match", "fixed", "string"])
  , ( 1, ["match", "contains"])
  , ( 2, ["match", "starts", "with"])
  , ( 3, ["match", "ends", "with"])
  , (16, ["match", "case", "sensitive"])
  , ( 4, ["match", "reg", "exp"])
  , ( 5, ["match", "wildcard"])
  , (32, ["match", "wrap"])
  , (64, ["match", "recursive"])
  ]

(e_MouseButton, bs_MouseButtons) =
  makeQtEnumBitspace (ident1 "Qt" "MouseButton") "MouseButtons" qtInclude $
  let noButton = 0x00000000
      allButtons = 0x07ffffff
      leftButton = 0x00000001
      rightButton = 0x00000002
      midButton = 0x00000004
      middleButton = midButton
      backButton = 0x00000008
      xButton1 = backButton
      extraButton1 = xButton1
      forwardButton = 0x00000010
      xButton2 = forwardButton
      extraButton2 = forwardButton
      taskButton = 0x00000020
      extraButton3 = taskButton
      extraButton4 = 0x00000040
      extraButton5 = 0x00000080
      extraButton6 = 0x00000100
      extraButton7 = 0x00000200
      extraButton8 = 0x00000400
      extraButton9 = 0x00000800
      extraButton10 = 0x00001000
      extraButton11 = 0x00002000
      extraButton12 = 0x00004000
      extraButton13 = 0x00008000
      extraButton14 = 0x00010000
      extraButton15 = 0x00020000
      extraButton16 = 0x00040000
      extraButton17 = 0x00080000
      extraButton18 = 0x00100000
      extraButton19 = 0x00200000
      extraButton20 = 0x00400000
      extraButton21 = 0x00800000
      extraButton22 = 0x01000000
      extraButton23 = 0x02000000
      extraButton24 = 0x04000000 
  in [ (noButton, ["no", "button"])
     , (allButtons, ["all", "buttons"])
     , (leftButton, ["left", "button"])
     , (rightButton, ["right", "button"])
     , (midButton, ["mid", "button"])
     , (middleButton, ["middle", "button"])
     , (backButton, ["back", "button"])
     , (xButton1, ["x", "button1"])
     , (extraButton1, ["extra", "button1"])
     , (forwardButton, ["forward", "button"])
     , (xButton2, ["x", "button2"])
     , (extraButton2, ["extra", "button2"])
     , (taskButton, ["task", "button"])
     , (extraButton3, ["extra", "button3"])
     , (extraButton4, ["extra", "button4"])
     , (extraButton5, ["extra", "button5"])
     , (extraButton6, ["extra", "button6"])
     , (extraButton7, ["extra", "button7"])
     , (extraButton8, ["extra", "button8"])
     , (extraButton9, ["extra", "button9"])
     , (extraButton10, ["extra", "button10"])
     , (extraButton11, ["extra", "button11"])
     , (extraButton12, ["extra", "button12"])
     , (extraButton13, ["extra", "button13"])
     , (extraButton14, ["extra", "button14"])
     , (extraButton15, ["extra", "button15"])
     , (extraButton16, ["extra", "button16"])
     , (extraButton17, ["extra", "button17"])
     , (extraButton18, ["extra", "button18"])
     , (extraButton19, ["extra", "button19"])
     , (extraButton20, ["extra", "button20"])
     , (extraButton21, ["extra", "button21"])
     , (extraButton22, ["extra", "button22"])
     , (extraButton23, ["extra", "button23"])
     , (extraButton24, ["extra", "button24"])
     ]


(e_MouseEventFlag, bs_MouseEventFlags) =
  makeQtEnumBitspace (ident1 "Qt" "MouseEventFlag") "MouseEventFlags" qtInclude
  [ (0x01, ["mouse", "event", "created", "double", "click"])
  ]

e_MouseEventFlag_minVersion = [5, 3]

e_MouseEventSource =
  makeQtEnum (ident1 "Qt" "MouseEventSource") qtInclude
  [ (0, ["mouse", "event", "not", "synthesized"])
  , (1, ["mouse", "event", "synthesized", "by", "system"])
  , (2, ["mouse", "event", "synthesized", "by", "qt"])
  ]

e_MouseEventSource_minVersion = [5, 3]

e_NativeGestureType =
  makeQtEnum (ident1 "Qt" "NativeGestureType") qtInclude
  [ (0, ["begin", "native", "gesture"])
  , (1, ["end", "native", "gesture"])
  , (2, ["pan", "native", "gesture"])
  , (3, ["zoom", "native", "gesture"])
  , (4, ["smart", "zoom", "native", "gesture"])
  , (5, ["rotate", "native", "gesture"])
  , (6, ["swipe", "native", "gesture"])
  ]

e_NavigationMode =
  makeQtEnum (ident1 "Qt" "NavigationMode") qtInclude
  [ (0, ["navigation", "mode", "none"])
  , (1, ["navigation", "mode", "keypad", "tab", "order"])
  , (2, ["navigation", "mode", "keypad", "directional"])
  , (3, ["navigation", "mode", "cursor", "auto"])
  , (4, ["navigation", "mode", "cursor", "force", "visible"])
  ]

(e_Orientation, bs_Orientations) =
  makeQtEnumBitspace (ident1 "Qt" "Orientation") "Orientations" qtInclude
  [ (1, ["horizontal"])
  , (2, ["vertical"])
  ]

e_PenCapStyle =
  makeQtEnum (ident1 "Qt" "PenCapStyle") qtInclude
  [ (0x00, ["flat", "cap"])
  , (0x10, ["square", "cap"])
  , (0x20, ["round", "cap"])
  ]

e_PenJoinStyle =
  makeQtEnum (ident1 "Qt" "PenJoinStyle") qtInclude
  [ (0x00, ["mitter", "join"])
  , (0x40, ["bevel", "join"])
  , (0x80, ["round", "join"])
  , (0x100, ["svg", "mitter", "join"])
  ]

e_PenStyle =
  makeQtEnum (ident1 "Qt" "PenStyle") qtInclude
  [ (0, ["no", "pen"])
  , (1, ["solid", "line"])
  , (2, ["dash", "line"])
  , (3, ["dot", "line"])
  , (4, ["dash", "dot", "line"])
  , (5, ["dash", "dot", "dot", "line"])
  , (6, ["custom", "dash", "line"])
  ]

(e_ScreenOrientation, bs_ScreenOrientations) =
  makeQtEnumBitspace (ident1 "Qt" "ScreenOrientation") "ScreenOrientations" qtInclude
  [ (0x0, ["primary", "orientation"])
  , (0x1, ["portrait", "orientation"])
  , (0x2, ["landscape", "orientation"])
  , (0x4, ["inverted", "portrait", "orientation"])
  , (0x8, ["inverted", "landscape", "orientation"])
  ]

e_ScreenOrientation_minVersion = [5, 0]

e_ScrollBarPolicy =
  makeQtEnum (ident1 "Qt" "ScrollBarPolicy") qtInclude
  [ (0, ["scroll", "bar", "as", "needed"])
  , (1, ["scroll", "bar", "always", "off"])
  , (2, ["scroll", "bar", "always", "on"])
  ]

e_ScrollPhase =
  makeQtEnum (ident1 "Qt" "ScrollPhase") qtInclude
  [ (1, ["scroll", "begin"])
  , (2, ["scroll", "update"])
  , (3, ["scroll", "end"])
  ]

e_ScrollPhase_minVersion = [5, 2]

e_ShortcutContext =
  makeQtEnum (ident1 "Qt" "ShortcutContext") qtInclude
  [ (0, ["widget", "shortcut"])
  , (3, ["widget", "with", "children", "shortcut"])
  , (1, ["window", "shortcut"])
  , (2, ["application", "shortcut"])
  ]

e_SizeHint =
  makeQtEnum (ident1 "Qt" "SizeHint") qtInclude
  [ (0, ["minimum", "size"])
  , (1, ["preferred", "size"])
  , (2, ["maximum", "size"])
  , (3, ["minimum", "descent"])
  ]

e_SizeMode =
  makeQtEnum (ident1 "Qt" "SizeMode") qtInclude
  [ (0, ["absolute", "size"])
  , (1, ["relative", "size"])
  ]

e_SortOrder =
  makeQtEnum (ident1 "Qt" "SortOrder") qtInclude
  [ (0, ["ascending", "order"])
  , (1, ["descending", "order"])
  ]

e_TabFocusBehavior =
  makeQtEnum (ident1 "Qt" "TabFocusBehavior") qtInclude
  [ (0x00, ["no", "tab", "focus"])
  , (0x01, ["tab", "focus", "text", "controls"])
  , (0x02, ["tab", "focus", "list", "controls"])
  , (0xff, ["tab", "focus", "all", "controls"])
  ]

e_TextElideMode =
  makeQtEnum (ident1 "Qt" "TextElideMode") qtInclude
  [ (0, ["elide", "left"])
  , (1, ["elide", "right"])
  , (2, ["elide", "middle"])
  , (3, ["elide", "none"])
  ]

e_TextFlag =
  makeQtEnum (ident1 "Qt" "TextFlag") qtInclude
  [ (0x0100, ["text", "single", "line"])
  , (0x0200, ["text", "dont", "clip"])
  , (0x0400, ["text", "expand", "tabs"])
  , (0x0800, ["text", "show", "mnemonic"])
  , (0x1000, ["text", "word", "wrap"])
  , (0x2000, ["text", "wrap", "anywhere"])
  , (0x8000, ["text", "hide", "mnemonic"])
  , (0x4000, ["text", "dont", "print"])
  , (0x08000000, ["text", "include", "trailing", "spaces"])
  , (0x10000, ["text", "justification", "forced"])
  ]

e_TextFormat =
  makeQtEnum (ident1 "Qt" "TextFormat") qtInclude
  [ (0, ["plain", "text"])
  , (1, ["rich", "text"])
  , (2, ["auto", "text"])
  , (3, ["log", "text"])
  ]

(e_TextInteractionFlag, bs_TextInteractionFlags) =
  makeQtEnumBitspace (ident1 "Qt" "TextInteractionFlag") "TextInteractionFlags" qtInclude $
  let noTextInteraction = 0
      textSelectableByMouse = 1
      textSelectableByKeyboard = 2
      linksAccessibleByMouse = 4
      linksAccessibleByKeyboard = 8
      textEditable = 16
      textEditorInteraction = textSelectableByMouse .|. textSelectableByKeyboard .|. textEditable
      textBrowserInteraction =
        textSelectableByMouse .|. linksAccessibleByMouse .|. linksAccessibleByKeyboard
  in [ (noTextInteraction, ["no", "text", "interaction"])
     , (textSelectableByMouse, ["text", "selectable", "by", "mouse"])
     , (textSelectableByKeyboard, ["text", "selectable", "by", "keyboard"])
     , (linksAccessibleByMouse, ["links", "accessible", "by", "mouse"])
     , (linksAccessibleByKeyboard, ["links", "accessible", "by", "keyboard"])
     , (textEditable, ["text", "editable"])
     , (textEditorInteraction, ["text", "editor", "interaction"])
     , (textBrowserInteraction, ["text", "browser", "interaction"])
     ]

e_TileRule =
  makeQtEnum (ident1 "Qt" "TileRule") qtInclude
  [ (0, ["stretch", "tile"])
  , (1, ["repeat", "tile"])
  , (2, ["round", "tile"])
  ]

e_TimeSpec =
  makeQtEnum (ident1 "Qt" "TimeSpec") qtInclude
  [ (0, ["local", "time"])
  , (1, ["u", "t", "c"])
  , (2, ["offset", "from", "u", "t", "c"])
  , (3, ["time", "zone"])
  ]

e_TimerType =
  makeQtEnum (ident1 "Qt" "TimerType") qtInclude
  [ (0, ["precise", "timer"])
  , (1, ["coarse", "timer"])
  , (2, ["very", "coarse", "timer"])
  ]

(e_ToolBarArea, bs_ToolBarAreas) =
  makeQtEnumBitspace (ident1 "Qt" "ToolBarArea") "ToolBarAreas" qtInclude
  [ (0x0, ["no", "tool", "bar", "area"])
  , (0x1, ["left", "tool", "bar", "area"])
  , (0x2, ["right", "tool", "bar", "area"])
  , (0x4, ["top", "tool", "bar", "area"])
  , (0x8, ["bottom", "tool", "bar", "area"])
  , (0xf, ["all", "tool", "bar", "areas"])
  ]

e_ToolButtonStyle =
  makeQtEnum (ident1 "Qt" "ToolButtonStyle") qtInclude
  [ (0, ["tool", "button", "icon", "only"])
  , (1, ["tool", "button", "text", "only"])
  , (2, ["tool", "button", "text", "beside", "icon"])
  , (3, ["tool", "button", "text", "under", "icon"])
  , (4, ["tool", "button", "follow", "style"])
  ]

(e_TouchPointState, bs_TouchPointStates) =
  makeQtEnumBitspace (ident1 "Qt" "TouchPointState") "TouchPointStates" qtInclude
  [ (0x01, ["touch", "point", "pressed"])
  , (0x02, ["touch", "point", "moved"])
  , (0x04, ["touch", "point", "stationary"])
  , (0x08, ["touch", "point", "released"])
  ]

e_TransformationMode =
  makeQtEnum (ident1 "Qt" "TransformationMode") qtInclude
  [ (0, ["fast", "transformation"])
  , (1, ["smooth", "transformation"])
  ]

e_UIEffect =
  makeQtEnum (ident1 "Qt" "UIEffect") qtInclude
  [ (1, ["u", "i_", "animate", "menu"])
  , (2, ["u", "i_", "fade", "menu"])
  , (3, ["u", "i_", "animate", "combo"])
  , (4, ["u", "i_", "animate", "tooltip"])
  , (5, ["u", "i_", "fade", "tooltip"])
  , (6, ["u", "i_", "animate", "tool", "box"])
  ]

e_WhiteSpaceMode =
  makeQtEnum (ident1 "Qt" "WhiteSpaceMode") qtInclude
  [ (0, ["white", "space", "normal"])
  , (1, ["white", "space", "pre"])
  , (2, ["white", "space", "no", "wrap"])
  ]

e_WidgetAttribute =
  makeQtEnum (ident1 "Qt" "WidgetAttribute") qtInclude
  [ (78, ["w", "a_", "accept", "drops"])
  , (84, ["w", "a_", "always", "show", "tool", "tips"])
  , (3, ["w", "a_", "contents", "propagated"])
  , (47, ["w", "a_", "custom", "whats", "this"])
  , (55, ["w", "a_", "delete", "on", "close"])
  , (0, ["w", "a_", "disabled"])
  , (103, ["w", "a_", "dont", "show", "on", "screen"])
  , (32, ["w", "a_", "force", "disabled"])
  , (59, ["w", "a_", "force", "updates", "disabled"])
  , (72, ["w", "a_", "group", "leader"])
  , (74, ["w", "a_", "hover"])
  , (14, ["w", "a_", "input", "method", "enabled"])
  , (77, ["w", "a_", "keyboard", "focus", "change"])
  , (33, ["w", "a_", "key", "compression"])
  , (48, ["w", "a_", "layout", "on", "entire", "rect"])
  , (92, ["w", "a_", "layout", "uses", "widget", "rect"])
  , (12, ["w", "a_", "mac", "no", "click", "through"])
  , (85, ["w", "a_", "mac", "opaque", "size", "grip"])
  , (88, ["w", "a_", "mac", "show", "focus", "rect"])
  , (89, ["w", "a_", "mac", "normal", "size"])
  , (90, ["w", "a_", "mac", "small", "size"])
  , (91, ["w", "a_", "mac", "mini", "size"])
  , (102, ["w", "a_", "mac", "variable", "size"])
  , (46, ["w", "a_", "mac", "brushed", "metal"])
  , (11, ["w", "a_", "mapped"])
  , (71, ["w", "a_", "mouse", "no", "mask"])
  , (2, ["w", "a_", "mouse", "tracking"])
  , (43, ["w", "a_", "moved"])
  , (94, ["w", "a_", "m", "s", "windows", "use", "direct3", "d"])
  , (58, ["w", "a_", "no", "child", "events", "for", "parent"])
  , (39, ["w", "a_", "no", "child", "events", "from", "children"])
  , (54, ["w", "a_", "no", "mouse", "replay"])
  , (73, ["w", "a_", "no", "mouse", "propagation"])
  , (51, ["w", "a_", "transparent", "for", "mouse", "events"])
  , (9, ["w", "a_", "no", "system", "background"])
  , (4, ["w", "a_", "opaque", "paint", "event"])
  , (49, ["w", "a_", "outside", "w", "s", "range"])
  , (8, ["w", "a_", "paint", "on", "screen"])
  , (52, ["w", "a_", "paint", "unclipped"])
  , (34, ["w", "a_", "pending", "move", "event"])
  , (35, ["w", "a_", "pending", "resize", "event"])
  , (76, ["w", "a_", "quit", "on", "close"])
  , (42, ["w", "a_", "resized"])
  , (56, ["w", "a_", "right", "to", "left"])
  , (38, ["w", "a_", "set", "cursor"])
  , (37, ["w", "a_", "set", "font"])
  , (36, ["w", "a_", "set", "palette"])
  , (86, ["w", "a_", "set", "style"])
  , (70, ["w", "a_", "show", "modal"])
  , (5, ["w", "a_", "static", "contents"])
  , (97, ["w", "a_", "style", "sheet"])
  , (131, ["w", "a_", "style", "sheet", "target"])
  , (129, ["w", "a_", "tablet", "tracking"])
  , (120, ["w", "a_", "translucent", "background"])
  , (1, ["w", "a_", "under", "mouse"])
  , (10, ["w", "a_", "updates", "disabled"])
  , (41, ["w", "a_", "window", "modified"])
  , (80, ["w", "a_", "window", "propagation"])
  , (96, ["w", "a_", "mac", "always", "show", "tool", "window"])
  , (87, ["w", "a_", "set", "locale"])
  , (93, ["w", "a_", "styled", "background"])
  , (98, ["w", "a_", "show", "without", "activating"])
  , (100, ["w", "a_", "native", "window"])
  , (101, ["w", "a_", "dont", "create", "native", "ancestors"])
  , (104, ["w", "a_", "x11", "net", "vm", "window", "type", "desktop"])
  , (105, ["w", "a_", "x11", "net", "vm", "window", "type", "dock"])
  , (106, ["w", "a_", "x11", "net", "vm", "window", "type", "tool", "bar"])
  , (107, ["w", "a_", "x11", "net", "vm", "window", "type", "menu"])
  , (108, ["w", "a_", "x11", "net", "vm", "window", "type", "utility"])
  , (109, ["w", "a_", "x11", "net", "vm", "window", "type", "splash"])
  , (110, ["w", "a_", "x11", "net", "vm", "window", "type", "dialog"])
  , (111, ["w", "a_", "x11", "net", "vm", "window", "type", "drop", "down", "menu"])
  , (112, ["w", "a_", "x11", "net", "vm", "window", "type", "popup", "menu"])
  , (113, ["w", "a_", "x11", "net", "vm", "window", "type", "tool", "tip"])
  , (114, ["w", "a_", "x11", "net", "vm", "window", "type", "notification"])
  , (115, ["w", "a_", "x11", "net", "vm", "window", "type", "combo"])
  , (116, ["w", "a_", "x11", "net", "vm", "window", "type", "d", "n", "d"])
  , (117, ["w", "a_", "mac", "framework", "scaled"])
  , (121, ["w", "a_", "accept", "touch", "events"])
  , (123, ["w", "a_", "touch", "pad", "accept", "single", "touch", "events"])
  , (126, ["w", "a_", "x11", "do", "not", "accept", "focus"])
  , (128, ["w", "a_", "always", "stack", "on", "top"])
  , (130, ["w", "a_", "contents", "margins", "respects", "safe", "area"])
  ]

e_WindowFrameSection =
  makeQtEnum (ident1 "Qt" "WindowFrameSection") qtInclude
  [ (0, ["no", "section"])
  , (1, ["left", "section"])
  , (2, ["top", "left", "section"])
  , (3, ["top", "section"])
  , (4, ["top", "right", "section"])
  , (5, ["right", "section"])
  , (6, ["bottom", "right", "section"])
  , (7, ["bottom", "section"])
  , (8, ["bottom", "left", "section"])
  , (9, ["title", "bar", "area"])
  ]

e_WindowModality =
  makeQtEnum (ident1 "Qt" "WindowModality") qtInclude
  [ (0, ["non", "modal"])
  , (1, ["window", "modal"])
  , (2, ["application", "modal"])
  ]

e_ApplicationAttribute =
  makeQtEnum (ident1 "Qt" "ApplicationAttribute") qtInclude $ 
  collect
  [ just (2, ["a", "a", "_", "dont", "show", "icons", "in", "menus"])
  , test (qtVersion >= [5,10]) (28, ["a", "a", "_", "dont", "show", "shortcuts", "in", "context", "menus"])
  , just (3, ["a", "a", "_", "native", "windows"])
  , just (4, ["a", "a", "_", "dont", "create", "native", "widget", "siblings"])
  , test (qtVersion >= [5,7]) (5, ["a", "a", "_", "plugin", "application"])
  , just (6, ["a", "a", "_", "dont", "use", "native", "menu", "bar"])
  , just (7, ["a", "a", "_", "mac", "dont", "swap", "ctrl", "and", "meta"])
  , just (8, ["a", "a", "_", "use96", "dpi"])
  , just (11, ["a", "a", "_", "synthesize", "touch", "for", "unhandled", "mouse", "events"])
  , just (12, ["a", "a", "_", "synthesize", "mouse", "for", "unhandled", "touch", "events"])
  , just (13, ["a", "a", "_", "use", "high", "dpi", "pixmaps"])
  , just (14, ["a", "a", "_", "force", "raster", "widgets"])
  , test (qtVersion >= [5,3]) (15, ["a", "a", "_", "use", "desktop", "open", "g", "l"])
  , test (qtVersion >= [5,3]) (16, ["a", "a", "_", "use", "open", "g", "l", "e", "s"])
  , test (qtVersion >= [5,4]) (17, ["a", "a", "_", "use", "software", "open", "g", "l"])
  , test (qtVersion >= [5,4])(18, ["a", "a", "_", "share", "open", "g", "l", "contexts"])
  , test (qtVersion >= [5,5]) (19, ["a", "a", "_", "set", "palette"])
  , test (qtVersion >= [5,6]) (20, ["a", "a", "_", "enable", "high", "dpi", "scaling"])
  , test (qtVersion >= [5,6]) (21, ["a", "a", "_", "disable", "high", "dpi", "scaling"])
  , test (qtVersion >= [5,7]) (22, ["a", "a", "_", "use", "style", "sheet", "propagation", "in", "widget", "styles"])
  , test (qtVersion >= [5,7]) (23, ["a", "a", "_", "dont", "use", "native", "dialogs"])
  , test (qtVersion >= [5,7]) (24, ["a", "a", "_", "synthesize", "mouse", "for", "unhandled", "tablet", "events"])
  , test (qtVersion >= [5,7]) (25, ["a", "a", "_", "compress", "high", "frequency", "events"])
  , test (qtVersion >= [5,10]) (29, ["a", "a", "_", "compress", "tablet", "events"])
  , test (qtVersion >= [5,8]) (26, ["a", "a", "_", "dont", "check", "open", "g", "l", "context", "thread", "affinity"])
  , just (27, ["a", "a", "_", "disable", "shader", "disk", "cache"])
  , test (qtVersion >= [5,10]) (30, ["a", "a", "_", "disable", "window", "context", "help", "button"])
    ]

(e_WindowState, bs_WindowStates) =
  makeQtEnumBitspace (ident1 "Qt" "WindowState") "WindowStates" qtInclude
  [ (0x00, ["window", "no", "state"])
  , (0x01, ["window", "minimized"])
  , (0x02, ["window", "maximized"])
  , (0x04, ["window", "full", "screen"])
  , (0x08, ["window", "active"])
  ]

(e_WindowType, bs_WindowFlags) =
  makeQtEnumBitspace (ident1 "Qt" "WindowType") "WindowFlags" qtInclude $
  let widget = 0x0
      window = 0x1
      dialog = 0x2 .|. window
      sheet = 0x4 .|. window
      drawer = sheet .|. dialog
      popup = 0x8 .|. window
      tool = popup .|. dialog
      toolTip = popup .|. sheet
      splashScreen = toolTip .|. dialog
      desktop = 0x10 .|. window
      subWindow = 0x12 .|. window
      foreignWindow = 0x20 .|. window
      coverWindow = 0x40 .|. window
  in [ (widget, ["widget"])
     , (window, ["window"])
     , (dialog, ["dialog"])
     , (sheet, ["sheet"])
     , (drawer, ["drawer"])
     , (popup, ["popup"])
     , (tool, ["tool"])
     , (toolTip, ["tool", "tip"])
     , (splashScreen, ["splash", "screen"])
     , (desktop, ["desktop"])
     , (subWindow, ["sub", "window"])
     , (foreignWindow, ["foreign", "window"])
     , (coverWindow, ["cover", "window"])
     ]

f_escape =
  addReqIncludes [includeStd "QTextDocument"] $
  makeFn (ident1 "Qt" "escape") Nothing Nonpure [objT c_QString] $ objT c_QString

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
  gluint,
  e_AlignmentFlag, fl_Alignment,
  e_ArrowType,
  e_AspectRatioMode,
  e_BrushStyle,
  e_CaseSensitivity,
  e_CheckState,
  e_ContextMenuPolicy,
  e_Corner,
  e_CursorMoveStyle,
  e_CursorShape,
  e_DockWidgetArea, fl_DockWidgetAreas,
  e_DropAction, fl_DropActions,
  e_EventPriority,
  e_FillRule,
  e_FocusReason,
  e_GlobalColor,
  e_ImageConversionFlag, fl_ImageConversionFlags,
  e_InputMethodHint, fl_InputMethodHints,
  e_ItemDataRole,
  e_ItemFlag, fl_ItemFlags,
  e_Key,
  e_KeyboardModifier, fl_KeyboardModifiers,
  e_LayoutDirection,
  e_MaskMode,
  e_MatchFlag, fl_MatchFlags,
  e_MouseButton, fl_MouseButtons,
  e_MouseEventFlag,
  e_MouseEventFlag_minVersion, fl_MouseEventFlags,
  e_MouseEventSource,
  e_MouseEventSource_minVersion,
  e_NavigationMode,
  e_Orientation, fl_Orientations,
  e_ScreenOrientation,
  e_ScreenOrientation_minVersion, fl_ScreenOrientations,
  e_ScrollBarPolicy,
  e_ScrollPhase,
  e_ScrollPhase_minVersion,
  e_SortOrder,
  e_TextElideMode,
  e_TextFormat,
  e_TextInteractionFlag, fl_TextInteractionFlags,
  e_ToolBarArea, fl_ToolBarAreas,
  e_ToolButtonStyle,
  e_TransformationMode,
  e_WindowModality,
  e_WindowState, fl_WindowStates,
  e_WindowType, fl_WindowFlags,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Include,
  Purity (Nonpure),
  Type,
  addReqIncludes,
  ident1,
  includeStd,
  makeFn,
  )
import Foreign.Hoppy.Generator.Types (doubleT, floatT, objT, word32T)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qrealFloat, qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule :: AModule
aModule = AQtModule $ makeQtModule ["Core", "Types"] exports

exports :: [QtExport]
exports =
  QtExportSpecials :
  collect
  [ just $ qtExport e_AlignmentFlag
  , just $ qtExport fl_Alignment
  , just $ qtExport e_ArrowType
  , just $ qtExport e_AspectRatioMode
  , just $ qtExport e_BrushStyle
  , just $ qtExport e_CaseSensitivity
  , just $ qtExport e_CheckState
  , just $ qtExport e_ContextMenuPolicy
  , just $ qtExport e_Corner
  , just $ qtExport e_CursorMoveStyle
  , just $ qtExport e_CursorShape
  , just $ qtExport e_DockWidgetArea
  , just $ qtExport fl_DockWidgetAreas
  , just $ qtExport e_DropAction
  , just $ qtExport fl_DropActions
  , just $ qtExport e_EventPriority
  , just $ qtExport e_FillRule
  , just $ qtExport e_FocusReason
  , just $ qtExport e_GlobalColor
  , just $ qtExport e_ImageConversionFlag
  , just $ qtExport fl_ImageConversionFlags
  , just $ qtExport e_InputMethodHint
  , just $ qtExport fl_InputMethodHints
  , just $ qtExport e_ItemDataRole
  , just $ qtExport e_ItemFlag
  , just $ qtExport fl_ItemFlags
  , just $ qtExport e_Key
  , just $ qtExport e_KeyboardModifier
  , just $ qtExport fl_KeyboardModifiers
  , just $ qtExport e_LayoutDirection
  , just $ qtExport e_MaskMode
  , just $ qtExport e_MatchFlag
  , just $ qtExport fl_MatchFlags
  , just $ qtExport e_MouseButton
  , just $ qtExport fl_MouseButtons
  , test (qtVersion >= e_MouseEventFlag_minVersion) $ qtExport e_MouseEventFlag
  , test (qtVersion >= e_MouseEventFlag_minVersion) $ qtExport fl_MouseEventFlags
  , test (qtVersion >= e_MouseEventSource_minVersion) $ qtExport e_MouseEventSource
  , just $ qtExport e_NavigationMode
  , just $ qtExport e_Orientation
  , just $ qtExport fl_Orientations
  , test (qtVersion >= e_ScreenOrientation_minVersion) $ qtExport e_ScreenOrientation
  , test (qtVersion >= e_ScreenOrientation_minVersion) $ qtExport fl_ScreenOrientations
  , just $ qtExport e_ScrollBarPolicy
  , test (qtVersion >= e_ScrollPhase_minVersion) $ qtExport e_ScrollPhase
  , just $ qtExport e_SortOrder
  , just $ qtExport e_TextElideMode
  , just $ qtExport e_TextFormat
  , just $ qtExport e_TextInteractionFlag
  , just $ qtExport fl_TextInteractionFlags
  , just $ qtExport e_ToolBarArea
  , just $ qtExport fl_ToolBarAreas
  , just $ qtExport e_ToolButtonStyle
  , just $ qtExport e_TransformationMode
  , just $ qtExport e_WindowModality
  , just $ qtExport e_WindowState
  , just $ qtExport fl_WindowStates
  , just $ qtExport e_WindowType
  , just $ qtExport fl_WindowFlags
  , test (qtVersion < [5, 0]) $ qtExport f_escape
  ]

qtInclude :: [Include]
qtInclude = [includeStd "Qt"]

qreal :: Type
qreal = if qrealFloat then floatT else doubleT

gluint :: Type
gluint = word32T

(e_AlignmentFlag, fl_Alignment) =
  makeQtEnumAndFlags (ident1 "Qt" "AlignmentFlag") "Alignment" qtInclude
  [ -- Horizontal flags.
    "AlignLeft"
  , "AlignRight"
  , "AlignHCenter"
  , "AlignJustify"
    -- Vertical flags.
  , "AlignTop"
  , "AlignBottom"
  , "AlignVCenter"
    -- Useful in right-to-left mode.
  , "AlignAbsolute"
  ]

e_ArrowType =
  makeQtEnum (ident1 "Qt" "ArrowType") qtInclude
  [ "NoArrow"
  , "UpArrow"
  , "DownArrow"
  , "LeftArrow"
  , "RightArrow"
  ]

e_AspectRatioMode =
  makeQtEnum (ident1 "Qt" "AspectRatioMode") qtInclude
  [ "IgnoreAspectRatio"
  , "KeepAspectRatio"
  , "KeepAspectRatioByExpanding"
  ]

e_BrushStyle =
  makeQtEnum (ident1 "Qt" "BrushStyle") qtInclude
  [ "NoBrush"
  , "SolidPattern"
  , "Dense1Pattern"
  , "Dense2Pattern"
  , "Dense3Pattern"
  , "Dense4Pattern"
  , "Dense5Pattern"
  , "Dense6Pattern"
  , "Dense7Pattern"
  , "HorPattern"
  , "VerPattern"
  , "CrossPattern"
  , "BDiagPattern"
  , "FDiagPattern"
  , "DiagCrossPattern"
  , "LinearGradientPattern"
  , "RadialGradientPattern"
  , "ConicalGradientPattern"
  , "TexturePattern"
  ]

e_CaseSensitivity =
  makeQtEnum (ident1 "Qt" "CaseSensitivity") qtInclude
  [ "CaseInsensitive"
  , "CaseSensitive"
  ]

e_CheckState =
  makeQtEnum (ident1 "Qt" "CheckState") qtInclude
  [ "Unchecked"
  , "PartiallyChecked"
  , "Checked"
  ]

e_ContextMenuPolicy =
  makeQtEnum (ident1 "Qt" "ContextMenuPolicy") qtInclude
  [ "NoContextMenu"
  , "PreventContextMenu"
  , "DefaultContextMenu"
  , "ActionsContextMenu"
  , "CustomContextMenu"
  ]

e_Corner =
  makeQtEnum (ident1 "Qt" "Corner") qtInclude
  [ "TopLeftCorner"
  , "TopRightCorner"
  , "BottomLeftCorner"
  , "BottomRightCorner"
  ]

e_CursorMoveStyle =
  makeQtEnum (ident1 "Qt" "CursorMoveStyle") qtInclude
  [ "LogicalMoveStyle"
  , "VisualMoveStyle"
  ]

e_CursorShape =
  makeQtEnum (ident1 "Qt" "CursorShape") qtInclude
  [ "ArrowCursor"
  , "UpArrowCursor"
  , "CrossCursor"
  , "WaitCursor"
  , "IBeamCursor"
  , "SizeVerCursor"
  , "SizeHorCursor"
  , "SizeBDiagCursor"
  , "SizeFDiagCursor"
  , "SizeAllCursor"
  , "BlankCursor"
  , "SplitVCursor"
  , "SplitHCursor"
  , "PointingHandCursor"
  , "ForbiddenCursor"
  , "WhatsThisCursor"
  , "BusyCursor"
  , "OpenHandCursor"
  , "ClosedHandCursor"
  , "DragCopyCursor"
  , "DragMoveCursor"
  , "DragLinkCursor"
  , "BitmapCursor"
  ]

(e_DockWidgetArea, fl_DockWidgetAreas) =
  makeQtEnumAndFlags (ident1 "Qt" "DockWidgetArea") "DockWidgetAreas" qtInclude
  [ "NoDockWidgetArea"
  , "LeftDockWidgetArea"
  , "RightDockWidgetArea"
  , "TopDockWidgetArea"
  , "BottomDockWidgetArea"
  , "AllDockWidgetAreas"
  ]

(e_DropAction, fl_DropActions) =
  makeQtEnumAndFlags (ident1 "Qt" "DropAction") "DropActions" qtInclude
  [ "IgnoreAction"
  , "CopyAction"
  , "MoveAction"
  , "LinkAction"
  , "ActionMask"
  , "TargetMoveAction"
  ]

e_EventPriority =
  makeQtEnum (ident1 "Qt" "EventPriority") qtInclude
  [ "HighEventPriority"
  , "NormalEventPriority"
  , "LowEventPriority"
  ]

e_FillRule =
  makeQtEnum (ident1 "Qt" "FillRule") qtInclude
  [ "OddEvenFill"
  , "WindingFill"
  ]

e_FocusReason =
  makeQtEnum (ident1 "Qt" "FocusReason") qtInclude
  [ "MouseFocusReason"
  , "TabFocusReason"
  , "BacktabFocusReason"
  , "ActiveWindowFocusReason"
  , "PopupFocusReason"
  , "ShortcutFocusReason"
  , "MenuBarFocusReason"
  , "OtherFocusReason"
  ]

e_GlobalColor =
  makeQtEnum (ident1 "Qt" "GlobalColor") qtInclude
  [ "white"
  , "black"
  , "red"
  , "darkRed"
  , "green"
  , "darkGreen"
  , "blue"
  , "darkBlue"
  , "cyan"
  , "darkCyan"
  , "magenta"
  , "darkMagenta"
  , "yellow"
  , "darkYellow"
  , "gray"
  , "darkGray"
  , "lightGray"
  , "transparent"
  , "color0"
  , "color1"
  ]

(e_ImageConversionFlag, fl_ImageConversionFlags) =
  makeQtEnumAndFlags (ident1 "Qt" "ImageConversionFlag") "ImageConversionFlags" qtInclude
  -- TODO Lots of synonyms for 0x0.  Hoppy doesn't support these.  We only
  -- include AutoColor and leave out e.g. DiffuseDither, ThresholdAlphaDither.
  [ "AutoColor"
    -- Color/mono preference:
  , "ColorOnly"
  , "MonoOnly"
    -- Dithering mode preference for RGB channels:
  , "OrderedDither"
  , "ThresholdDither"
    -- Dithering mode preference for alpha channel:
  , "OrderedAlphaDither"
  , "DiffuseAlphaDither"
    -- Color matching versus dithering preference:
  , "PreferDither"
  , "AvoidDither"
  , "NoOpaqueDetection"
  , "NoFormatConversion"
  ]

(e_InputMethodHint, fl_InputMethodHints) =
  makeQtEnumAndFlags (ident1 "Qt" "InputMethodHint") "InputMethodHints" qtInclude
  [ "ImhNone"
  , "ImhHiddenText"
  , "ImhSensitiveData"
  , "ImhNoAutoUppercase"
  , "ImhPreferNumbers"
  , "ImhPreferUppercase"
  , "ImhPreferLowercase"
  , "ImhNoPredictiveText"
  , "ImhDate"
  , "ImhTime"
  , "ImhPreferLatin"
  , "ImhMultiLine"
  , "ImhDigitsOnly"
  , "ImhFormattedNumbersOnly"
  , "ImhUppercaseOnly"
  , "ImhLowercaseOnly"
  , "ImhDialableCharactersOnly"
  , "ImhEmailCharactersOnly"
  , "ImhUrlCharactersOnly"
  , "ImhLatinOnly"
  , "ImhExclusiveInputMask"
  ]

-- TODO Support for custom ItemDataRole values.
e_ItemDataRole =
  makeQtEnum (ident1 "Qt" "ItemDataRole") qtInclude $
  collect
  [ -- General-purpose roles:
    just "DisplayRole"
  , just "DecorationRole"
  , just "EditRole"
  , just "ToolTipRole"
  , just "StatusTipRole"
  , just "WhatsThisRole"
  , just "SizeHintRole"

    -- Roles describing appearance and metadata:
  , just "FontRole"
  , just "TextAlignmentRole"
  , just "BackgroundRole"
  , just "ForegroundRole"
  , just "CheckStateRole"
  , test (qtVersion >= [4, 8]) "InitialSortOrderRole"

    -- Accessibility roles:
  , just "AccessibleTextRole"
  , just "AccessibleDescriptionRole"

    -- User roles:
  , just "UserRole"
  ]

(e_ItemFlag, fl_ItemFlags) =
  makeQtEnumAndFlags (ident1 "Qt" "ItemFlag") "ItemFlags" qtInclude $
  collect
  [ just "NoItemFlags"
  , just "ItemIsSelectable"
  , just "ItemIsEditable"
  , just "ItemIsDragEnabled"
  , just "ItemIsDropEnabled"
  , just "ItemIsUserCheckable"
  , just "ItemIsEnabled"
  , just "ItemIsAutoTristate"
  , just "ItemNeverHasChildren"
  , test (qtVersion >= [5, 5]) "ItemIsUserTristate"
  ]

e_Key =
  makeQtEnum (ident1 "Qt" "Key") qtInclude
  [ "Key_Escape"
  , "Key_Tab"
  , "Key_Backtab"
  , "Key_Backspace"
  , "Key_Return"
  , "Key_Enter"
  , "Key_Insert"
  , "Key_Delete"
  , "Key_Pause"
  , "Key_Print"
  , "Key_SysReq"
  , "Key_Clear"
  , "Key_Home"
  , "Key_End"
  , "Key_Left"
  , "Key_Up"
  , "Key_Right"
  , "Key_Down"
  , "Key_PageUp"
  , "Key_PageDown"
  , "Key_Shift"
  , "Key_Control"
  , "Key_Meta"
  , "Key_Alt"
  , "Key_AltGr"
  , "Key_CapsLock"
  , "Key_NumLock"
  , "Key_ScrollLock"
  , "Key_F1"
  , "Key_F2"
  , "Key_F3"
  , "Key_F4"
  , "Key_F5"
  , "Key_F6"
  , "Key_F7"
  , "Key_F8"
  , "Key_F9"
  , "Key_F10"
  , "Key_F11"
  , "Key_F12"
  , "Key_F13"
  , "Key_F14"
  , "Key_F15"
  , "Key_F16"
  , "Key_F17"
  , "Key_F18"
  , "Key_F19"
  , "Key_F20"
  , "Key_F21"
  , "Key_F22"
  , "Key_F23"
  , "Key_F24"
  , "Key_F25"
  , "Key_F26"
  , "Key_F27"
  , "Key_F28"
  , "Key_F29"
  , "Key_F30"
  , "Key_F31"
  , "Key_F32"
  , "Key_F33"
  , "Key_F34"
  , "Key_F35"
  , "Key_Super_L"
  , "Key_Super_R"
  , "Key_Menu"
  , "Key_Hyper_L"
  , "Key_Hyper_R"
  , "Key_Help"
  , "Key_Direction_L"
  , "Key_Direction_R"
  , "Key_Space"  -- Aka Key_Any.
  , "Key_Exclam"
  , "Key_QuoteDbl"
  , "Key_NumberSign"
  , "Key_Dollar"
  , "Key_Percent"
  , "Key_Ampersand"
  , "Key_Apostrophe"
  , "Key_ParenLeft"
  , "Key_ParenRight"
  , "Key_Asterisk"
  , "Key_Plus"
  , "Key_Comma"
  , "Key_Minus"
  , "Key_Period"
  , "Key_Slash"
  , "Key_0"
  , "Key_1"
  , "Key_2"
  , "Key_3"
  , "Key_4"
  , "Key_5"
  , "Key_6"
  , "Key_7"
  , "Key_8"
  , "Key_9"
  , "Key_Colon"
  , "Key_Semicolon"
  , "Key_Less"
  , "Key_Equal"
  , "Key_Greater"
  , "Key_Question"
  , "Key_At"
  , "Key_A"
  , "Key_B"
  , "Key_C"
  , "Key_D"
  , "Key_E"
  , "Key_F"
  , "Key_G"
  , "Key_H"
  , "Key_I"
  , "Key_J"
  , "Key_K"
  , "Key_L"
  , "Key_M"
  , "Key_N"
  , "Key_O"
  , "Key_P"
  , "Key_Q"
  , "Key_R"
  , "Key_S"
  , "Key_T"
  , "Key_U"
  , "Key_V"
  , "Key_W"
  , "Key_X"
  , "Key_Y"
  , "Key_Z"
  , "Key_BracketLeft"
  , "Key_Backslash"
  , "Key_BracketRight"
  , "Key_AsciiCircum"
  , "Key_Underscore"
  , "Key_QuoteLeft"
  , "Key_BraceLeft"
  , "Key_Bar"
  , "Key_BraceRight"
  , "Key_AsciiTilde"
    -- TODO Additional Qt::Key_* constants.
  ]

(e_KeyboardModifier, fl_KeyboardModifiers) =
  makeQtEnumAndFlags (ident1 "Qt" "KeyboardModifier") "KeyboardModifiers" qtInclude
  [ "NoModifier"
  , "ShiftModifier"
  , "ControlModifier"
  , "AltModifier"
  , "MetaModifier"
  , "KeypadModifier"
  , "GroupSwitchModifier"
  ]

e_LayoutDirection =
  makeQtEnum (ident1 "Qt" "LayoutDirection") qtInclude
  [ "LeftToRight"
  , "RightToLeft"
  , "LayoutDirectionAuto"
  ]

e_MaskMode =
  makeQtEnum (ident1 "Qt" "MaskMode") qtInclude
  [ "MaskInColor"
  , "MaskOutColor"
  ]

(e_MatchFlag, fl_MatchFlags) =
  makeQtEnumAndFlags (ident1 "Qt" "MatchFlag") "MatchFlags" qtInclude
  [ "MatchExactly"
  , "MatchFixedString"
  , "MatchContains"
  , "MatchStartsWith"
  , "MatchEndsWith"
  , "MatchCaseSensitive"
  , "MatchRegExp"
  , "MatchWildcard"
  , "MatchWrap"
  , "MatchRecursive"
  ]

(e_MouseButton, fl_MouseButtons) =
  makeQtEnumAndFlags (ident1 "Qt" "MouseButton") "MouseButtons" qtInclude
  [ "NoButton"
  , "AllButtons"
  , "LeftButton"
  , "RightButton"
  , "MiddleButton"
    -- TODO Other mouse buttons.  Lots of synonyms here which Hoppy doesn't support.
  ]

(e_MouseEventFlag, fl_MouseEventFlags) =
  makeQtEnumAndFlags (ident1 "Qt" "MouseEventFlag") "MouseEventFlags" qtInclude
  [ "MouseEventCreatedDoubleClick"
  ]

e_MouseEventFlag_minVersion = [5, 3]

e_MouseEventSource =
  makeQtEnum (ident1 "Qt" "MouseEventSource") qtInclude
  [ "MouseEventNotSynthesized"
  , "MouseEventSynthesizedBySystem"
  , "MouseEventSynthesizedByQt"
  ]

e_MouseEventSource_minVersion = [5, 3]

e_NavigationMode =
  makeQtEnum (ident1 "Qt" "NavigationMode") qtInclude
  [ "NavigationModeNone"
  , "NavigationModeKeypadTabOrder"
  , "NavigationModeKeypadDirectional"
  , "NavigationModeCursorAuto"
  , "NavigationModeCursorForceVisible"
  ]

(e_Orientation, fl_Orientations) =
  makeQtEnumAndFlags (ident1 "Qt" "Orientation") "Orientations" qtInclude
  [ "Horizontal"
  , "Vertical"
  ]

(e_ScreenOrientation, fl_ScreenOrientations) =
  makeQtEnumAndFlags (ident1 "Qt" "ScreenOrientation") "ScreenOrientations" qtInclude
  [ "PrimaryOrientation"
  , "PortraitOrientation"
  , "LandscapeOrientation"
  , "InvertedPortraitOrientation"
  , "InvertedLandscapeOrientation"
  ]

e_ScreenOrientation_minVersion = [5, 0]

e_ScrollBarPolicy =
  makeQtEnum (ident1 "Qt" "ScrollBarPolicy") qtInclude
  [ "ScrollBarAsNeeded"
  , "ScrollBarAlwaysOff"
  , "ScrollBarAlwaysOn"
  ]

e_ScrollPhase =
  makeQtEnum (ident1 "Qt" "ScrollPhase") qtInclude
  [ "ScrollBegin"
  , "ScrollUpdate"
  , "ScrollEnd"
  ]

e_ScrollPhase_minVersion = [5, 2]

e_SortOrder =
  makeQtEnum (ident1 "Qt" "SortOrder") qtInclude
  [ "AscendingOrder"
  , "DescendingOrder"
  ]

e_TextElideMode =
  makeQtEnum (ident1 "Qt" "TextElideMode") qtInclude
  [ "ElideLeft"
  , "ElideRight"
  , "ElideMiddle"
  , "ElideNone"
  ]

e_TextFormat =
  makeQtEnum (ident1 "Qt" "TextFormat") qtInclude $
  collect
  [ just "PlainText"
  , just "RichText"
  , just "AutoText"
  , test (qtVersion < [5, 0]) "LogText"
  ]

(e_TextInteractionFlag, fl_TextInteractionFlags) =
  makeQtEnumAndFlags (ident1 "Qt" "TextInteractionFlag") "TextInteractionFlags" qtInclude
  [ "NoTextInteraction"
  , "TextSelectableByMouse"
  , "TextSelectableByKeyboard"
  , "LinksAccessibleByMouse"
  , "LinksAccessibleByKeyboard"
  , "TextEditable"
  , "TextEditorInteraction"
  , "TextBrowserInteraction"
  ]

(e_ToolBarArea, fl_ToolBarAreas) =
  makeQtEnumAndFlags (ident1 "Qt" "ToolBarArea") "ToolBarAreas" qtInclude
  [ "NoToolBarArea"
  , "LeftToolBarArea"
  , "RightToolBarArea"
  , "TopToolBarArea"
  , "BottomToolBarArea"
  , "AllToolBarAreas"
  ]

e_ToolButtonStyle =
  makeQtEnum (ident1 "Qt" "ToolButtonStyle") qtInclude
  [ "ToolButtonIconOnly"
  , "ToolButtonTextOnly"
  , "ToolButtonTextBesideIcon"
  , "ToolButtonTextUnderIcon"
  , "ToolButtonFollowStyle"
  ]

e_TransformationMode =
  makeQtEnum (ident1 "Qt" "TransformationMode") qtInclude
  [ "FastTransformation"
  , "SmoothTransformation"
  ]

e_WindowModality =
  makeQtEnum (ident1 "Qt" "WindowModality") qtInclude
  [ "NonModal"
  , "WindowModal"
  , "ApplicationModal"
  ]

(e_WindowState, fl_WindowStates) =
  makeQtEnumAndFlags (ident1 "Qt" "WindowState") "WindowStates" qtInclude
  [ "WindowNoState"
  , "WindowMinimized"
  , "WindowMaximized"
  , "WindowFullScreen"
  , "WindowActive"
  ]

(e_WindowType, fl_WindowFlags) =
  makeQtEnumAndFlags (ident1 "Qt" "WindowType") "WindowFlags" qtInclude $
  [ "Widget"
  , "Window"
  , "Dialog"
  , "Sheet"
  , "Drawer"
  , "Popup"
  , "Tool"
  , "ToolTip"
  , "SplashScreen"
  , "Desktop"
  , "SubWindow"
  , "ForeignWindow"
  , "CoverWindow"
  ]

f_escape =
  addReqIncludes [includeStd "QTextDocument"] $
  makeFn (ident1 "Qt" "escape") Nothing Nonpure [objT c_QString] $ objT c_QString

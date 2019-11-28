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
  qfunctionpointer,
  qtmessagehandler,
  qint8,
  qint16,
  qint32,
  qint64,
  qintptr,
  qptrdiff,
  qsizetype,
  quint8,
  quint16,
  quint32,
  quint64,
  quintptr,
  wchar_t,
  e_QtMsgType,
  e_AlignmentFlag, fl_Alignment,
  e_AnchorPoint,
  e_ApplicationState, fl_ApplicationStates,
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
  e_DockWidgetArea, fl_DockWidgetAreas,
  e_DropAction, fl_DropActions,
  e_Edge, fl_Edges,
  e_EnterKeyType,
  e_EventPriority,
  e_FillRule,
  e_FindChildOption, fl_FindChildOptions,
  e_FocusPolicy,
  e_FocusReason,
  e_GestureFlag, fl_GestureFlags,
  e_GestureState, fl_GestureStates,
  e_GestureType,
  e_HitTestAccuracy,
  e_ImageConversionFlag, fl_ImageConversionFlags,
  e_GlobalColor,
  e_InputMethodHint, fl_InputMethodHints,
  e_InputMethodQuery, fl_InputMethodQueries,
  e_ItemDataRole,
  e_ItemFlag, fl_ItemFlags,
  e_ItemSelectionMode,
  e_ItemSelectionOperation,
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
  e_NativeGestureType,
  e_NavigationMode,
  e_Orientation, fl_Orientations,
  e_PenCapStyle,
  e_PenJoinStyle,
  e_PenStyle,
  e_ScreenOrientation,
  e_ScreenOrientation_minVersion, fl_ScreenOrientations,
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
  e_TextInteractionFlag, fl_TextInteractionFlags,
  e_TileRule,
  e_TimeSpec,
  e_TimerType,
  e_ToolBarArea, fl_ToolBarAreas,
  e_ToolButtonStyle,
  e_TouchPointState, fl_TouchPointStates,
  e_TransformationMode,
  e_UIEffect,
  e_WhiteSpaceMode,
  e_WidgetAttribute,
  e_WindowFrameSection,
  e_WindowModality,
  e_WindowState, fl_WindowStates,
  e_WindowType, fl_WindowFlags,
  ) where

import Data.Bits (finiteBitSize)
import Foreign.Hoppy.Generator.Spec (
  Include,
  Purity (Nonpure),
  Type,
  addReqIncludes,
  ident,
  ident1,
  includeStd,
  makeFn,
  )
import System.Info (os)
import Foreign.Hoppy.Generator.Spec.Enum (CppEnum)
import Foreign.Hoppy.Generator.Types (doubleT, floatT, objT, int8T, int16T, int32T, int64T, ssizeT, word8T, word16T, word32T, word64T, ptrT, refT, fnT, voidT, enumT, constT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qrealFloat, qtVersion)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QMessageLogContext (c_QMessageLogContext)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule :: AModule
aModule = AQtModule $ makeQtModule ["Core", "Types"] exports

exports :: [QtExport]
exports =
  QtExportSpecials :
  collect
  [ test (qtVersion >= [5, 5]) $ qtExport e_QtMsgType
  , just $ qtExport e_AlignmentFlag
  , just $ qtExport fl_Alignment
  , just $ qtExport e_AnchorPoint
  , test (qtVersion >= [5, 1]) $ qtExport e_ApplicationState
  , test (qtVersion >= [5, 1]) $ qtExport fl_ApplicationStates
  , just $ qtExport e_ArrowType
  , just $ qtExport e_AspectRatioMode
  , just $ qtExport e_Axis
  , just $ qtExport e_BGMode
  , just $ qtExport e_BrushStyle
  , just $ qtExport e_CaseSensitivity
  , just $ qtExport e_CheckState
  , test (qtVersion >= [5, 9]) $ qtExport e_ChecksumType
  , just $ qtExport e_ClipOperation
  , just $ qtExport e_ConnectionType
  , just $ qtExport e_ContextMenuPolicy
  , test (qtVersion >= [4, 6]) $ qtExport e_CoordinateSystem
  , just $ qtExport e_Corner
  , just $ qtExport e_CursorMoveStyle
  , just $ qtExport e_CursorShape
  , just $ qtExport e_DateFormat
  , just $ qtExport e_DayOfWeek
  , just $ qtExport e_DockWidgetArea
  , just $ qtExport fl_DockWidgetAreas
  , just $ qtExport e_DropAction
  , just $ qtExport fl_DropActions
  , test (qtVersion >= [5, 1]) $ qtExport e_Edge
  , test (qtVersion >= [5, 1]) $ qtExport fl_Edges
  , test (qtVersion >= [5, 6]) $ qtExport e_EnterKeyType
  , just $ qtExport e_EventPriority
  , just $ qtExport e_FillRule
  , just $ qtExport e_FindChildOption
  , just $ qtExport fl_FindChildOptions
  , just $ qtExport e_FocusPolicy
  , just $ qtExport e_FocusReason
  , just $ qtExport e_GestureFlag
  , just $ qtExport fl_GestureFlags
  , test (qtVersion >= [4, 6]) $ qtExport e_GestureState
  , test (qtVersion >= [4, 6]) $ qtExport fl_GestureStates
  , test (qtVersion >= [4, 6]) $ qtExport e_GestureType
  , just $ qtExport e_HitTestAccuracy
  , just $ qtExport e_ImageConversionFlag
  , just $ qtExport fl_ImageConversionFlags
  , just $ qtExport e_GlobalColor
  , just $ qtExport e_InputMethodHint
  , just $ qtExport fl_InputMethodHints
  , just $ qtExport e_InputMethodQuery
  , just $ qtExport fl_InputMethodQueries
  , just $ qtExport e_ItemDataRole
  , just $ qtExport e_ItemFlag
  , just $ qtExport fl_ItemFlags
  , just $ qtExport e_ItemSelectionMode
  , just $ qtExport e_ItemSelectionOperation
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
  , test (qtVersion >= [5,2]) $ qtExport e_NativeGestureType
  , just $ qtExport e_NavigationMode
  , just $ qtExport e_Orientation
  , just $ qtExport fl_Orientations
  , just $ qtExport e_PenCapStyle
  , just $ qtExport e_PenJoinStyle
  , just $ qtExport e_PenStyle
  , test (qtVersion >= e_ScreenOrientation_minVersion) $ qtExport e_ScreenOrientation
  , test (qtVersion >= e_ScreenOrientation_minVersion) $ qtExport fl_ScreenOrientations
  , just $ qtExport e_ScrollBarPolicy
  , test (qtVersion >= e_ScrollPhase_minVersion) $ qtExport e_ScrollPhase
  , just $ qtExport e_ShortcutContext
  , test (qtVersion >= [4, 4]) $ qtExport e_SizeHint
  , test (qtVersion >= [4, 4]) $ qtExport e_SizeMode
  , just $ qtExport e_SortOrder
  , test (qtVersion >= [5, 5]) $ qtExport e_TabFocusBehavior
  , just $ qtExport e_TextElideMode
  , just $ qtExport e_TextFlag
  , just $ qtExport e_TextFormat
  , just $ qtExport e_TextInteractionFlag
  , just $ qtExport fl_TextInteractionFlags
  , test (qtVersion >= [4, 6]) $ qtExport e_TileRule
  , just $ qtExport e_TimeSpec
  , just $ qtExport e_TimerType
  , just $ qtExport e_ToolBarArea
  , just $ qtExport fl_ToolBarAreas
  , just $ qtExport e_ToolButtonStyle
  , test (qtVersion >= [4, 6]) $ qtExport e_TouchPointState
  , test (qtVersion >= [4, 6]) $ qtExport fl_TouchPointStates
  , just $ qtExport e_TransformationMode
  , just $ qtExport e_UIEffect
  , just $ qtExport e_WhiteSpaceMode
  , just $ qtExport e_WidgetAttribute
  , test (qtVersion >= [4, 4]) $ qtExport e_WindowFrameSection
  , just $ qtExport e_WindowModality
  , just $ qtExport e_ApplicationAttribute
  , just $ qtExport e_WindowState
  , just $ qtExport fl_WindowStates
  , just $ qtExport e_WindowType
  , just $ qtExport fl_WindowFlags
  , test (qtVersion < [5, 0]) $ qtExport f_escape
  ]

qtInclude :: [Include]
qtInclude = [includeStd "Qt", includeStd "QtGlobal"]

qreal :: Type
qreal = if qrealFloat then floatT else doubleT

qlonglong :: Type
qlonglong = qint64

qulonglong :: Type
qulonglong = quint64

qfunctionpointer :: Type
qfunctionpointer = ptrT $ fnT [] voidT

qtmessagehandler :: Type
qtmessagehandler = ptrT $ fnT [enumT e_QtMsgType, refT $ constT $ objT c_QMessageLogContext, refT $ constT $ objT c_QString] voidT

qint8 :: Type
qint8 = int8T

qint16 :: Type
qint16 = int16T

qint32 :: Type
qint32 = int32T

qint64 :: Type
qint64 = int64T

qintptr :: Type
qintptr = if finiteBitSize (undefined :: Int) == 64 then qint64 else qint32

qptrdiff :: Type
qptrdiff = if finiteBitSize (undefined :: Int) == 64 then qint64 else qint32

qsizetype :: Type
qsizetype = ssizeT

quint8 :: Type
quint8 = word8T

quint16 :: Type
quint16 = word16T

quint32 :: Type
quint32 = word32T

quint64 :: Type
quint64 = word64T

quintptr :: Type
quintptr = if finiteBitSize (undefined :: Int) == 64 then quint64 else quint32

gluint :: Type
gluint = word32T

wchar_t :: Type
wchar_t = if os == "mingw32" then word16T else word32T

e_QtMsgType :: CppEnum
e_QtMsgType =
  makeQtEnum (ident "QtMsgType") qtInclude
  [ "QtDebugMsg"
  , "QtInfoMsg"
  , "QtWarningMsg"
  , "QtCriticalMsg"
  , "QtFatalMsg"
  , "QtSystemMsg"
  ]

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

e_AnchorPoint =
  makeQtEnum (ident1 "Qt" "AnchorPoint") qtInclude
  [ "AnchorLeft"
  , "AnchorHorizontalCenter"
  , "AnchorRight"
  , "AnchorTop"
  , "AnchorVerticalCenter"
  , "AnchorBottom"
  ]

(e_ApplicationState, fl_ApplicationStates) =
  makeQtEnumAndFlags (ident1 "Qt" "ApplicationState") "ApplicationStates" qtInclude
  [ "ApplicationSuspended"
  , "ApplicationHidden"
  , "ApplicationInactive"
  , "ApplicationActive"
  ]

e_ApplicationAttribute =
  makeQtEnum (ident1 "Qt" "ApplicationAttribute") qtInclude $
  collect
  [ just "AA_DontShowIconsInMenus"
  , test (qtVersion >= [5,10]) "AA_DontShowShortcutsInContextMenus"
  , just "AA_NativeWindows"
  , just "AA_DontCreateNativeWidgetSiblings"
  , test (qtVersion >= [5,7]) "AA_PluginApplication"
  , just "AA_DontUseNativeMenuBar"
  , just "AA_MacDontSwapCtrlAndMeta"
  , just "AA_Use96Dpi"
  , just "AA_SynthesizeTouchForUnhandledMouseEvents"
  , just "AA_SynthesizeMouseForUnhandledTouchEvents"
  , just "AA_UseHighDpiPixmaps"
  , just "AA_ForceRasterWidgets"
  , test (qtVersion >= [5,3]) "AA_UseDesktopOpenGL"
  , test (qtVersion >= [5,3]) "AA_UseOpenGLES"
  , test (qtVersion >= [5,4]) "AA_UseSoftwareOpenGL"
  , test (qtVersion >= [5,4]) "AA_ShareOpenGLContexts"
  , test (qtVersion >= [5,5]) "AA_SetPalette"
  , test (qtVersion >= [5,6]) "AA_EnableHighDpiScaling"
  , test (qtVersion >= [5,6]) "AA_DisableHighDpiScaling"
  , test (qtVersion >= [5,7]) "AA_UseStyleSheetPropagationInWidgetStyles"
  , test (qtVersion >= [5,7]) "AA_DontUseNativeDialogs"
  , test (qtVersion >= [5,7]) "AA_SynthesizeMouseForUnhandledTabletEvents"
  , test (qtVersion >= [5,7]) "AA_CompressHighFrequencyEvents"
  , test (qtVersion >= [5,10]) "AA_CompressTabletEvents"
  , test (qtVersion >= [5,8]) "AA_DontCheckOpenGLContextThreadAffinity"
  , just "AA_DisableShaderDiskCache"
  , test (qtVersion >= [5,10]) "AA_DisableWindowContextHelpButton"
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

e_Axis =
  makeQtEnum (ident1 "Qt" "Axis") qtInclude
  [ "XAxis"
  , "YAxis"
  , "ZAxis"
  ]

e_BGMode =
  makeQtEnum (ident1 "Qt" "BGMode") qtInclude
  [ "TransparentMode"
  , "OpaqueMode"
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

e_ChecksumType =
  makeQtEnum (ident1 "Qt" "ChecksumType") qtInclude
  [ "ChecksumIso3309"
  , "ChecksumItuV41"
  ]

e_ClipOperation =
  makeQtEnum (ident1 "Qt" "ClipOperation") qtInclude
  [ "NoClip"
  , "ReplaceClip"
  , "IntersectClip"
  ]

e_ConnectionType =
  makeQtEnum (ident1 "Qt" "ConnectionType") qtInclude $
  collect
  [ just "AutoConnection"
  , just "DirectConnection"
  , just "QueuedConnection"
  , just "BlockingQueuedConnection"
  , test (qtVersion >= [4, 6]) "UniqueConnection"
  ]

e_ContextMenuPolicy =
  makeQtEnum (ident1 "Qt" "ContextMenuPolicy") qtInclude
  [ "NoContextMenu"
  , "PreventContextMenu"
  , "DefaultContextMenu"
  , "ActionsContextMenu"
  , "CustomContextMenu"
  ]

e_CoordinateSystem =
  makeQtEnum (ident1 "Qt" "CoordinateSystem") qtInclude
  [ "DeviceCoordinates"
  , "LogicalCoordinates"
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

e_DateFormat =
  makeQtEnum (ident1 "Qt" "DateFormat") qtInclude
  [ "TextDate"
  , "ISODate"
  , "ISODateWithMs"
  , "SystemLocaleShortDate"
  , "SystemLocaleLongDate"
  , "DefaultLocaleShortDate"
  , "DefaultLocaleLongDate"
  , "SystemLocaleDate"
  , "LocaleDate"
  , "RFC2822Date"
  ]

e_DayOfWeek =
  makeQtEnum (ident1 "Qt" "DayOfWeek") qtInclude
  [ "Monday"
  , "Tuesday"
  , "Wednesday"
  , "Thursday"
  , "Friday"
  , "Saturday"
  , "Sunday"
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

(e_Edge, fl_Edges) =
  makeQtEnumAndFlags (ident1 "Qt" "Edge") "Edges" qtInclude
  [ "TopEdge"
  , "LeftEdge"
  , "RightEdge"
  , "BottomEdge"
  ]

e_EnterKeyType =
  makeQtEnum (ident1 "Qt" "EnterKeyType") qtInclude
  [ "EnterKeyDefault"
  , "EnterKeyReturn"
  , "EnterKeyDone"
  , "EnterKeyGo"
  , "EnterKeySend"
  , "EnterKeySearch"
  , "EnterKeyNext"
  , "EnterKeyPrevious"
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

(e_FindChildOption, fl_FindChildOptions) =
  makeQtEnumAndFlags (ident1 "Qt" "FindChildOption") "FindChildOptions" qtInclude
  [ "FindDirectChildrenOnly"
  , "FindChildrenRecursively"
  ]

e_FocusPolicy =
  makeQtEnum (ident1 "Qt" "FocusPolicy") qtInclude
  [ "TabFocus"
  , "ClickFocus"
  , "StrongFocus"
  , "WheelFocus"
  , "NoFocus"
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

(e_GestureFlag, fl_GestureFlags) =
  makeQtEnumAndFlags (ident1 "Qt" "GestureFlag") "GestureFlags" qtInclude $
  collect
  [ just "DontStartGestureOnChildren"
  , just "ReceivePartialGestures"
  , test (qtVersion >= [4,7]) "IgnoredGesturesPropagateToParent"
  ]

(e_GestureState, fl_GestureStates) =
  makeQtEnumAndFlags (ident1 "Qt" "GestureState") "GestureStates" qtInclude
  [ "NoGesture"
  , "GestureStarted"
  , "GestureUpdated"
  , "GestureFinished"
  , "GestureCanceled"
  ]

e_GestureType =
  makeQtEnum (ident1 "Qt" "GestureType") qtInclude
  [ "TapGesture"
  , "TapAndHoldGesture"
  , "PanGesture"
  , "PinchGesture"
  , "SwipeGesture"
  , "CustomGesture"
  ]

e_HitTestAccuracy =
  makeQtEnum (ident1 "Qt" "HitTestAccuracy") qtInclude
  [ "ExactHit"
  , "FuzzyHit"
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
  [ -- Color/mono preference:
    "AutoColor"
  , "ColorOnly"
  , "MonoOnly"
    -- Dithering mode preference for RGB channels:
  , "DiffuseDither"
  , "OrderedDither"
  , "ThresholdDither"
    -- Dithering mode preference for alpha channel:
  , "ThresholdAlphaDither"
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

(e_InputMethodQuery, fl_InputMethodQueries) =
  makeQtEnumAndFlags (ident1 "Qt" "InputMethodQuery") "InputMethodQueries" qtInclude $
  collect
  [ just "ImEnabled"
  , just "ImMicroFocus"
  , just "ImCursorRectangle"
  , just "ImFont"
  , just "ImCursorPosition"
  , just "ImSurroundingText"
  , just "ImCurrentSelection"
  , just "ImMaximumTextLength"
  , just "ImAnchorPosition"
  , just "ImHints"
  , just "ImPreferredLanguage"
  , just "ImPlatformData"
  , just "ImAbsolutePosition"
  , just "ImTextBeforeCursor"
  , just "ImTextAfterCursor"
  , just "ImEnterKeyType"
  , test (qtVersion >= [5,7]) "ImAnchorRectangle"
  , just "ImInputItemClipRectangle"
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

e_ItemSelectionMode =
  makeQtEnum (ident1 "Qt" "ItemSelectionMode") qtInclude
  [ "ContainsItemShape"
  , "IntersectsItemShape"
  , "ContainsItemBoundingRect"
  , "IntersectsItemBoundingRect"
  ]

e_ItemSelectionOperation =
  makeQtEnum (ident1 "Qt" "ItemSelectionOperation") qtInclude
  [ "ReplaceSelection"
  , "AddToSelection"
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
  , "Key_Space"
  , "Key_Any"  -- Alias for Key_Space.
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
  , "Key_nobreakspace"
  , "Key_exclamdown"
  , "Key_cent"
  , "Key_sterling"
  , "Key_currency"
  , "Key_yen"
  , "Key_brokenbar"
  , "Key_section"
  , "Key_diaeresis"
  , "Key_copyright"
  , "Key_ordfeminine"
  , "Key_guillemotleft"
  , "Key_notsign"
  , "Key_hyphen"
  , "Key_registered"
  , "Key_macron"
  , "Key_degree"
  , "Key_plusminus"
  , "Key_twosuperior"
  , "Key_threesuperior"
  , "Key_acute"
  , "Key_mu"
  , "Key_paragraph"
  , "Key_periodcentered"
  , "Key_cedilla"
  , "Key_onesuperior"
  , "Key_masculine"
  , "Key_guillemotright"
  , "Key_onequarter"
  , "Key_onehalf"
  , "Key_threequarters"
  , "Key_questiondown"
  , "Key_Agrave"
  , "Key_Aacute"
  , "Key_Acircumflex"
  , "Key_Atilde"
  , "Key_Adiaeresis"
  , "Key_Aring"
  , "Key_AE"
  , "Key_Ccedilla"
  , "Key_Egrave"
  , "Key_Eacute"
  , "Key_Ecircumflex"
  , "Key_Ediaeresis"
  , "Key_Igrave"
  , "Key_Iacute"
  , "Key_Icircumflex"
  , "Key_Idiaeresis"
  , "Key_ETH"
  , "Key_Ntilde"
  , "Key_Ograve"
  , "Key_Oacute"
  , "Key_Ocircumflex"
  , "Key_Otilde"
  , "Key_Odiaeresis"
  , "Key_multiply"
  , "Key_Ooblique"
  , "Key_Ugrave"
  , "Key_Uacute"
  , "Key_Ucircumflex"
  , "Key_Udiaeresis"
  , "Key_Yacute"
  , "Key_THORN"
  , "Key_ssharp"
  , "Key_division"
  , "Key_ydiaeresis"
  , "Key_Multi_key"
  , "Key_Codeinput"
  , "Key_SingleCandidate"
  , "Key_MultipleCandidate"
  , "Key_PreviousCandidate"
  , "Key_Mode_switch"
  , "Key_Kanji"
  , "Key_Muhenkan"
  , "Key_Henkan"
  , "Key_Romaji"
  , "Key_Hiragana"
  , "Key_Katakana"
  , "Key_Hiragana_Katakana"
  , "Key_Zenkaku"
  , "Key_Hankaku"
  --, "Key_Zenkaku_Hankaku"
  , "Key_Touroku"
  , "Key_Massyo"
  , "Key_Kana_Lock"
  , "Key_Kana_Shift"
  , "Key_Eisu_Shift"
  , "Key_Eisu_toggle"
  , "Key_Hangul"
  , "Key_Hangul_Start"
  , "Key_Hangul_End"
  , "Key_Hangul_Hanja"
  , "Key_Hangul_Jamo"
  , "Key_Hangul_Romaja"
  , "Key_Hangul_Jeonja"
  , "Key_Hangul_Banja"
  , "Key_Hangul_PreHanja"
  , "Key_Hangul_PostHanja"
  , "Key_Hangul_Special"
  , "Key_Dead_Grave"
  , "Key_Dead_Acute"
  , "Key_Dead_Circumflex"
  , "Key_Dead_Tilde"
  , "Key_Dead_Macron"
  , "Key_Dead_Breve"
  , "Key_Dead_Abovedot"
  , "Key_Dead_Diaeresis"
  , "Key_Dead_Abovering"
  , "Key_Dead_Doubleacute"
  , "Key_Dead_Caron"
  , "Key_Dead_Cedilla"
  , "Key_Dead_Ogonek"
  , "Key_Dead_Iota"
  , "Key_Dead_Voiced_Sound"
  , "Key_Dead_Semivoiced_Sound"
  , "Key_Dead_Belowdot"
  , "Key_Dead_Hook"
  , "Key_Dead_Horn"
  , "Key_Dead_Stroke"
  , "Key_Dead_Abovecomma"
  , "Key_Dead_Abovereversedcomma"
  , "Key_Dead_Doublegrave"
  , "Key_Dead_Belowring"
  , "Key_Dead_Belowmacron"
  , "Key_Dead_Belowtilde"
  , "Key_Dead_Belowbreve"
  , "Key_Dead_Belowdiaeresis"
  , "Key_Dead_Invertedbreve"
  , "Key_Dead_Belowcomma"
  , "Key_Dead_Currency"
  , "Key_Dead_a"
  , "Key_Dead_A"
  , "Key_Dead_e"
  , "Key_Dead_E"
  , "Key_Dead_i"
  , "Key_Dead_I"
  , "Key_Dead_o"
  , "Key_Dead_O"
  , "Key_Dead_u"
  , "Key_Dead_U"
  , "Key_Dead_Small_Schwa"
  , "Key_Dead_Capital_Schwa"
  , "Key_Dead_Greek"
  , "Key_Dead_Lowline"
  , "Key_Dead_Aboveverticalline"
  , "Key_Dead_Belowverticalline"
  , "Key_Dead_Longsolidusoverlay"
  , "Key_Back"
  , "Key_Forward"
  , "Key_Stop"
  , "Key_Refresh"
  , "Key_VolumeDown"
  , "Key_VolumeMute"
  , "Key_VolumeUp"
  , "Key_BassBoost"
  , "Key_BassUp"
  , "Key_BassDown"
  , "Key_TrebleUp"
  , "Key_TrebleDown"
  , "Key_MediaPlay"
  , "Key_MediaStop"
  , "Key_MediaPrevious"
  , "Key_MediaNext"
  , "Key_MediaRecord"
  , "Key_MediaPause"
  , "Key_MediaTogglePlayPause"
  --, "Key_HomePage"
  , "Key_Favorites"
  , "Key_Search"
  , "Key_Standby"
  , "Key_OpenUrl"
  , "Key_LaunchMail"
  , "Key_LaunchMedia"
  , "Key_Launch0"
  , "Key_Launch1"
  , "Key_Launch2"
  , "Key_Launch3"
  , "Key_Launch4"
  , "Key_Launch5"
  , "Key_Launch6"
  , "Key_Launch7"
  , "Key_Launch8"
  , "Key_Launch9"
  , "Key_LaunchA"
  , "Key_LaunchB"
  , "Key_LaunchC"
  , "Key_LaunchD"
  , "Key_LaunchE"
  , "Key_LaunchF"
  , "Key_LaunchG"
  , "Key_LaunchH"
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
  , "MidButton"
  , "MiddleButton"
  , "BackButton"
  , "XButton1"
  , "ExtraButton1"
  , "ForwardButton"
  , "XButton2"
  , "ExtraButton2"
  , "TaskButton"
  , "ExtraButton3"
  , "ExtraButton4"
  , "ExtraButton5"
  , "ExtraButton6"
  , "ExtraButton7"
  , "ExtraButton8"
  , "ExtraButton9"
  , "ExtraButton10"
  , "ExtraButton11"
  , "ExtraButton12"
  , "ExtraButton13"
  , "ExtraButton14"
  , "ExtraButton15"
  , "ExtraButton16"
  , "ExtraButton17"
  , "ExtraButton18"
  , "ExtraButton19"
  , "ExtraButton20"
  , "ExtraButton21"
  , "ExtraButton22"
  , "ExtraButton23"
  , "ExtraButton24"
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

e_NativeGestureType =
  makeQtEnum (ident1 "Qt" "NativeGestureType") qtInclude
  [ "BeginNativeGesture"
  , "EndNativeGesture"
  , "PanNativeGesture"
  , "ZoomNativeGesture"
  , "SmartZoomNativeGesture"
  , "RotateNativeGesture"
  , "SwipeNativeGesture"
  ]

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

e_PenCapStyle =
  makeQtEnum (ident1 "Qt" "PenCapStyle") qtInclude
  [ "FlatCap"
  , "SquareCap"
  , "RoundCap"
  ]

e_PenJoinStyle =
  makeQtEnum (ident1 "Qt" "PenJoinStyle") qtInclude
  [ "MiterJoin"
  , "BevelJoin"
  , "RoundJoin"
  , "SvgMiterJoin"
  ]

e_PenStyle =
  makeQtEnum (ident1 "Qt" "PenStyle") qtInclude
  [ "NoPen"
  , "SolidLine"
  , "DashLine"
  , "DotLine"
  , "DashDotLine"
  , "DashDotDotLine"
  , "CustomDashLine"
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

e_ShortcutContext =
  makeQtEnum (ident1 "Qt" "ShortcutContext") qtInclude
  [ "WidgetShortcut"
  , "WidgetWithChildrenShortcut"
  , "WindowShortcut"
  , "ApplicationShortcut"
  ]

e_SizeHint =
  makeQtEnum (ident1 "Qt" "SizeHint") qtInclude
  [ "MinimumSize"
  , "PreferredSize"
  , "MaximumSize"
  , "MinimumDescent"
  ]

e_SizeMode =
  makeQtEnum (ident1 "Qt" "SizeMode") qtInclude
  [ "AbsoluteSize"
  , "RelativeSize"
  ]

e_SortOrder =
  makeQtEnum (ident1 "Qt" "SortOrder") qtInclude
  [ "AscendingOrder"
  , "DescendingOrder"
  ]

e_TabFocusBehavior =
  makeQtEnum (ident1 "Qt" "TabFocusBehavior") qtInclude
  [ "NoTabFocus"
  , "TabFocusTextControls"
  , "TabFocusListControls"
  , "TabFocusAllControls"
  ]

e_TextElideMode =
  makeQtEnum (ident1 "Qt" "TextElideMode") qtInclude
  [ "ElideLeft"
  , "ElideRight"
  , "ElideMiddle"
  , "ElideNone"
  ]

e_TextFlag =
  makeQtEnum (ident1 "Qt" "TextFlag") qtInclude
  [ "TextSingleLine"
  , "TextDontClip"
  , "TextExpandTabs"
  , "TextShowMnemonic"
  , "TextWordWrap"
  , "TextWrapAnywhere"
  , "TextHideMnemonic"
  , "TextDontPrint"
  , "TextIncludeTrailingSpaces"
  , "TextJustificationForced"
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

e_TileRule =
  makeQtEnum (ident1 "Qt" "TileRule") qtInclude
  [ "StretchTile"
  , "RepeatTile"
  , "RoundTile"
  ]

e_TimeSpec =
  makeQtEnum (ident1 "Qt" "TimeSpec") qtInclude
  [ "LocalTime"
  , "UTC"
  , "OffsetFromUTC"
  , "TimeZone"
  ]

e_TimerType =
  makeQtEnum (ident1 "Qt" "TimerType") qtInclude
  [ "PreciseTimer"
  , "CoarseTimer"
  , "VeryCoarseTimer"
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

(e_TouchPointState, fl_TouchPointStates) =
  makeQtEnumAndFlags (ident1 "Qt" "TouchPointState") "TouchPointStates" qtInclude
  [ "TouchPointPressed"
  , "TouchPointMoved"
  , "TouchPointStationary"
  , "TouchPointReleased"
  ]

e_TransformationMode =
  makeQtEnum (ident1 "Qt" "TransformationMode") qtInclude
  [ "FastTransformation"
  , "SmoothTransformation"
  ]

e_UIEffect =
  makeQtEnum (ident1 "Qt" "UIEffect") qtInclude
  [ "UI_AnimateMenu"
  , "UI_FadeMenu"
  , "UI_AnimateCombo"
  , "UI_AnimateTooltip"
  , "UI_FadeTooltip"
  , "UI_AnimateToolBox"
  ]

e_WhiteSpaceMode =
  makeQtEnum (ident1 "Qt" "WhiteSpaceMode") qtInclude
  [ "WhiteSpaceNormal"
  , "WhiteSpacePre"
  , "WhiteSpaceNoWrap"
  ]

e_WidgetAttribute =
  makeQtEnum (ident1 "Qt" "WidgetAttribute") qtInclude
  [ "WA_AcceptDrops"
  , "WA_AlwaysShowToolTips"
  , "WA_ContentsPropagated"
  , "WA_CustomWhatsThis"
  , "WA_DeleteOnClose"
  , "WA_Disabled"
  , "WA_DontShowOnScreen"
  , "WA_ForceDisabled"
  , "WA_ForceUpdatesDisabled"
  , "WA_GroupLeader"
  , "WA_Hover"
  , "WA_InputMethodEnabled"
  , "WA_KeyboardFocusChange"
  , "WA_KeyCompression"
  , "WA_LayoutOnEntireRect"
  , "WA_LayoutUsesWidgetRect"
  , "WA_MacNoClickThrough"
  , "WA_MacOpaqueSizeGrip"
  , "WA_MacShowFocusRect"
  , "WA_MacNormalSize"
  , "WA_MacSmallSize"
  , "WA_MacMiniSize"
  , "WA_MacVariableSize"
  , "WA_MacBrushedMetal"
  , "WA_Mapped"
  , "WA_MouseNoMask"
  , "WA_MouseTracking"
  , "WA_Moved"
  , "WA_MSWindowsUseDirect3D"
  , "WA_NoChildEventsForParent"
  , "WA_NoChildEventsFromChildren"
  , "WA_NoMouseReplay"
  , "WA_NoMousePropagation"
  , "WA_TransparentForMouseEvents"
  , "WA_NoSystemBackground"
  , "WA_OpaquePaintEvent"
  , "WA_OutsideWSRange"
  , "WA_PaintOnScreen"
  , "WA_PaintUnclipped"
  , "WA_PendingMoveEvent"
  , "WA_PendingResizeEvent"
  , "WA_QuitOnClose"
  , "WA_Resized"
  , "WA_RightToLeft"
  , "WA_SetCursor"
  , "WA_SetFont"
  , "WA_SetPalette"
  , "WA_SetStyle"
  , "WA_ShowModal"
  , "WA_StaticContents"
  , "WA_StyleSheet"
  , "WA_StyleSheetTarget"
  , "WA_TabletTracking"
  , "WA_TranslucentBackground"
  , "WA_UnderMouse"
  , "WA_UpdatesDisabled"
  , "WA_WindowModified"
  , "WA_WindowPropagation"
  , "WA_MacAlwaysShowToolWindow"
  , "WA_SetLocale"
  , "WA_StyledBackground"
  , "WA_ShowWithoutActivating"
  , "WA_NativeWindow"
  , "WA_DontCreateNativeAncestors"
  , "WA_X11NetWmWindowTypeDesktop"
  , "WA_X11NetWmWindowTypeDock"
  , "WA_X11NetWmWindowTypeToolBar"
  , "WA_X11NetWmWindowTypeMenu"
  , "WA_X11NetWmWindowTypeUtility"
  , "WA_X11NetWmWindowTypeSplash"
  , "WA_X11NetWmWindowTypeDialog"
  , "WA_X11NetWmWindowTypeDropDownMenu"
  , "WA_X11NetWmWindowTypePopupMenu"
  , "WA_X11NetWmWindowTypeToolTip"
  , "WA_X11NetWmWindowTypeNotification"
  , "WA_X11NetWmWindowTypeCombo"
  , "WA_X11NetWmWindowTypeDND"
  , "WA_MacFrameworkScaled"
  , "WA_AcceptTouchEvents"
  , "WA_TouchPadAcceptSingleTouchEvents"
  , "WA_X11DoNotAcceptFocus"
  , "WA_AlwaysStackOnTop"
  , "WA_ContentsMarginsRespectsSafeArea"
  ]

e_WindowFrameSection =
  makeQtEnum (ident1 "Qt" "WindowFrameSection") qtInclude
  [ "NoSection"
  , "LeftSection"
  , "TopLeftSection"
  , "TopSection"
  , "TopRightSection"
  , "RightSection"
  , "BottomRightSection"
  , "BottomSection"
  , "BottomLeftSection"
  , "TitleBarArea"
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

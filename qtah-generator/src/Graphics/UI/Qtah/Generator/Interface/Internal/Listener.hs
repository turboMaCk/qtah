---------- GENERATED FILE, EDITS WILL BE LOST ----------

module Graphics.UI.Qtah.Generator.Interface.Internal.Listener where

import qualified Foreign.Hoppy.Generator.Version as V
import qualified Graphics.UI.Qtah.Generator.Flags as F
import qualified Foreign.Hoppy.Generator.Spec as S
import qualified Foreign.Hoppy.Generator.Types as S
import qualified Foreign.Hoppy.Generator.Std.String as String
import qualified Graphics.UI.Qtah.Generator.Module as M
import qualified Graphics.UI.Qtah.Generator.Interface.Core.QObject as QObject
import qualified Graphics.UI.Qtah.Generator.Interface.Internal.Callback as C

{-# ANN module "HLint: ignore Use camelCase" #-}

c_ListenerBool =
  S.makeClass (S.ident "ListenerBool") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_BoolVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_BoolVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerDockWidgetArea =
  S.makeClass (S.ident "ListenerDockWidgetArea") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_DockWidgetAreaVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_DockWidgetAreaVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerDockWidgetAreas =
  S.makeClass (S.ident "ListenerDockWidgetAreas") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_DockWidgetAreasVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_DockWidgetAreasVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerDouble =
  S.makeClass (S.ident "ListenerDouble") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_DoubleVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_DoubleVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerInt =
  S.makeClass (S.ident "ListenerInt") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_IntVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_IntVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerIntBool =
  S.makeClass (S.ident "ListenerIntBool") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_IntBoolVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_IntBoolVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerIntInt =
  S.makeClass (S.ident "ListenerIntInt") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_IntIntVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_IntIntVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerOrientation =
  S.makeClass (S.ident "ListenerOrientation") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_OrientationVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_OrientationVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerOrientationIntInt =
  S.makeClass (S.ident "ListenerOrientationIntInt") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_OrientationIntIntVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_OrientationIntIntVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]  

c_ListenerPtrQAbstractButton =
  S.makeClass (S.ident "ListenerPtrQAbstractButton") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_PtrQAbstractButtonVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_PtrQAbstractButtonVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerPtrQAbstractButtonBool =
  S.makeClass (S.ident "ListenerPtrQAbstractButtonBool") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_PtrQAbstractButtonBoolVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_PtrQAbstractButtonBoolVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerPtrQAbstractItemModel =
  S.makeClass (S.ident "ListenerPtrQAbstractItemModel") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_PtrQAbstractItemModelVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_PtrQAbstractItemModelVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerPtrQAction =
  S.makeClass (S.ident "ListenerPtrQAction") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_PtrQActionVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_PtrQActionVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerPtrQObject =
  S.makeClass (S.ident "ListenerPtrQObject") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_PtrQObjectVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_PtrQObjectVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerRefConstQModelIndexVoid =
  S.makeClass (S.ident "ListenerRefConstQModelIndex") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_RefConstQModelIndexVoid]
  , S.mkCtor "newWithParent" [S.callbackT C.cb_RefConstQModelIndexVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener" [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]  

c_ListenerPtrQTreeWidgetItem =
  S.makeClass (S.ident "ListenerPtrQTreeWidgetItem") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_PtrQTreeWidgetItemVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_PtrQTreeWidgetItemVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerPtrQTreeWidgetItemInt =
  S.makeClass (S.ident "ListenerPtrQTreeWidgetItemInt") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_PtrQTreeWidgetItemIntVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_PtrQTreeWidgetItemIntVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerPtrQTreeWidgetItemPtrQTreeWidgetItem =
  S.makeClass (S.ident "ListenerPtrQTreeWidgetItemPtrQTreeWidgetItem") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_PtrQTreeWidgetItemPtrQTreeWidgetItemVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_PtrQTreeWidgetItemPtrQTreeWidgetItemVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerPtrQWidgetPtrQWidget =
  S.makeClass (S.ident "ListenerPtrQWidgetPtrQWidget") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_PtrQWidgetPtrQWidgetVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_PtrQWidgetPtrQWidgetVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerQAbstractSliderAction =
  S.makeClass (S.ident "ListenerQAbstractSliderAction") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_QAbstractSliderActionVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_QAbstractSliderActionVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerQClipboardMode =
  S.makeClass (S.ident "ListenerQClipboardMode") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_QClipboardModeVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_QClipboardModeVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerQDate =
  S.makeClass (S.ident "ListenerQDate") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_QDateVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_QDateVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerQDockWidgetFeatures =
  S.makeClass (S.ident "ListenerQDockWidgetFeatures") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_QDockWidgetFeaturesVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_QDockWidgetFeaturesVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerQModelIndex =
  S.makeClass (S.ident "ListenerQModelIndex") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_QModelIndexVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_QModelIndexVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerQModelIndexIntInt =
  S.makeClass (S.ident "ListenerQModelIndexIntInt") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_QModelIndexIntIntVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_QModelIndexIntIntVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerQModelIndexIntIntQModelIndexInt =
  S.makeClass (S.ident "ListenerQModelIndexIntIntQModelIndexInt") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_QModelIndexIntIntQModelIndexIntVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_QModelIndexIntIntQModelIndexIntVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerQModelIndexQModelIndex =
  S.makeClass (S.ident "ListenerQModelIndexQModelIndex") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_QModelIndexQModelIndexVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_QModelIndexQModelIndexVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerQModelIndexQModelIndexQVectorInt =
  S.makeClass (S.ident "ListenerQModelIndexQModelIndexQVectorInt") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_QModelIndexQModelIndexQVectorIntVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_QModelIndexQModelIndexQVectorIntVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerQPoint =
  S.makeClass (S.ident "ListenerQPoint") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_QPointVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_QPointVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerQreal =
  S.makeClass (S.ident "ListenerQreal") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_QrealVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_QrealVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerQSize =
  S.makeClass (S.ident "ListenerQSize") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_QSizeVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_QSizeVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerQString =
  S.makeClass (S.ident "ListenerQString") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_QStringVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_QStringVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerQSystemTrayIconActivationReason =
  S.makeClass (S.ident "ListenerQSystemTrayIconActivationReason") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_QSystemTrayIconActivationReasonVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_QSystemTrayIconActivationReasonVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerQWindowVisibility =
  S.makeClass (S.ident "ListenerQWindowVisibility") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_QWindowVisibilityVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_QWindowVisibilityVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerRefConstQIcon =
  S.makeClass (S.ident "ListenerRefConstQIcon") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_RefConstQIconVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_RefConstQIconVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerRefConstQItemSelectionRefConstQItemSelection =
  S.makeClass (S.ident "ListenerRefConstQItemSelectionRefConstQItemSelection") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_RefConstQItemSelectionRefConstQItemSelectionVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_RefConstQItemSelectionRefConstQItemSelectionVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerScreenOrientation =
  S.makeClass (S.ident "ListenerScreenOrientation") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_ScreenOrientationVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_ScreenOrientationVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerToolBarAreas =
  S.makeClass (S.ident "ListenerToolBarAreas") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_ToolBarAreasVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_ToolBarAreasVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerToolButtonStyle =
  S.makeClass (S.ident "ListenerToolButtonStyle") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_ToolButtonStyleVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_ToolButtonStyleVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerWindowModality =
  S.makeClass (S.ident "ListenerWindowModality") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_WindowModalityVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_WindowModalityVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_ListenerWindowState =
  S.makeClass (S.ident "ListenerWindowState") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_WindowStateVoid]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_WindowStateVoid, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

c_Listener =
  S.makeClass (S.ident "Listener") Nothing [QObject.c_QObject]
  [ S.mkCtor "new" [S.callbackT C.cb_Void]
  , S.mkCtor "newWithParent"
    [S.callbackT C.cb_Void, S.ptrT $ S.objT QObject.c_QObject]
  , S.mkMethod "connectListener"
    [S.ptrT $ S.objT QObject.c_QObject, S.objT String.c_string] S.boolT
  ]

aModule :: M.AModule
aModule =
  M.AHoppyModule $
  S.addReqIncludes [S.includeLocal "listener.hpp"] $
  S.moduleModify' (S.makeModule "listener" "b_listener.hpp" "b_listener.cpp") $ do
    S.moduleAddHaskellName ["Internal", "Listener"]
    S.moduleAddExports $ V.collect
      [ V.just $ S.ExportClass c_ListenerBool
      , V.just $ S.ExportClass c_ListenerDockWidgetArea
      , V.just $ S.ExportClass c_ListenerDockWidgetAreas
      , V.just $ S.ExportClass c_ListenerDouble
      , V.just $ S.ExportClass c_ListenerInt
      , V.just $ S.ExportClass c_ListenerIntBool
      , V.just $ S.ExportClass c_ListenerIntInt
      , V.just $ S.ExportClass c_ListenerOrientation
      , V.just $ S.ExportClass c_ListenerOrientationIntInt
      , V.just $ S.ExportClass c_ListenerPtrQAbstractButton
      , V.just $ S.ExportClass c_ListenerPtrQAbstractButtonBool
      , V.just $ S.ExportClass c_ListenerPtrQAbstractItemModel
      , V.just $ S.ExportClass c_ListenerPtrQAction
      , V.just $ S.ExportClass c_ListenerPtrQObject
      , V.just $ S.ExportClass c_ListenerRefConstQModelIndexVoid
      , V.just $ S.ExportClass c_ListenerPtrQTreeWidgetItem
      , V.just $ S.ExportClass c_ListenerPtrQTreeWidgetItemInt
      , V.just $ S.ExportClass c_ListenerPtrQTreeWidgetItemPtrQTreeWidgetItem
      , V.just $ S.ExportClass c_ListenerPtrQWidgetPtrQWidget
      , V.just $ S.ExportClass c_ListenerQAbstractSliderAction
      , V.just $ S.ExportClass c_ListenerQClipboardMode
      , V.just $ S.ExportClass c_ListenerQDate
      , V.just $ S.ExportClass c_ListenerQDockWidgetFeatures
      , V.just $ S.ExportClass c_ListenerQModelIndex
      , V.just $ S.ExportClass c_ListenerQModelIndexIntInt
      , V.just $ S.ExportClass c_ListenerQModelIndexIntIntQModelIndexInt
      , V.just $ S.ExportClass c_ListenerQModelIndexQModelIndex
      , V.just $ S.ExportClass c_ListenerQModelIndexQModelIndexQVectorInt
      , V.just $ S.ExportClass c_ListenerQPoint
      , V.just $ S.ExportClass c_ListenerQreal
      , V.just $ S.ExportClass c_ListenerQSize
      , V.just $ S.ExportClass c_ListenerQString
      , V.just $ S.ExportClass c_ListenerQSystemTrayIconActivationReason
      , V.test (F.qtVersion >= [5, 0]) $ S.ExportClass c_ListenerQWindowVisibility
      , V.just $ S.ExportClass c_ListenerRefConstQIcon
      , V.just $ S.ExportClass c_ListenerRefConstQItemSelectionRefConstQItemSelection
      , V.test (F.qtVersion >= [5, 0]) $ S.ExportClass c_ListenerScreenOrientation
      , V.just $ S.ExportClass c_ListenerToolBarAreas
      , V.just $ S.ExportClass c_ListenerToolButtonStyle
      , V.just $ S.ExportClass c_ListenerWindowModality
      , V.just $ S.ExportClass c_ListenerWindowState
      , V.just $ S.ExportClass c_Listener
      ]

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

module Graphics.UI.Qtah.Generator.Interface.Core.QObject (
  aModule,
  c_QObject,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  MethodApplicability (MConst, MNormal),
  Purity (Nonpure),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
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
  mkMethod,
  mkProp,
  )
import Foreign.Hoppy.Generator.Types (fnT, boolT, constT, intT, objT, ptrT, refT, ptrT, voidT, charT, enumT, toGcT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QList (
  c_QListQByteArray,
  c_QListQObject,
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QEvent (c_QEvent)
import Graphics.UI.Qtah.Generator.Interface.Core.QMetaObject (c_QMetaObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QMetaMethod (c_QMetaMethod)
import Graphics.UI.Qtah.Generator.Interface.Core.Connection (c_Connection)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QThread (c_QThread)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QVariant (c_QVariant)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  c_ListenerPtrQObject,
  c_ListenerQString,
  )
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_ConnectionType, bs_FindChildOptions, e_TimerType)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QObject"] $
  (QtExport $ ExportClass c_QObject) :
  map QtExportSignal signals

c_QObject =
  addReqIncludes [ includeStd "QObject"
                 , includeLocal "wrap_qobject.hpp"
                 ] $
  classSetEntityPrefix "" $
  makeClass (ident "QObject") Nothing [] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QObject]
  , just $ mkMethod "blockSignals" [boolT] boolT
  , just $ mkMethod "children" [] $ objT c_QListQObject
  
  , just $ mkConstMethod' "connect" "connect" [ptrT $ constT $ objT c_QObject, ptrT $ constT charT, ptrT $ constT charT] $ objT c_Connection
  , just $ mkConstMethod' "connect" "connectWithType" [ptrT $ constT $ objT c_QObject, ptrT $ constT charT, ptrT $ constT charT, enumT e_ConnectionType] $ objT c_Connection
  
  , just $ mkStaticMethod' "connect" "connectStatic" [ptrT $ constT $ objT c_QObject, ptrT $ constT charT, ptrT $ constT $ objT c_QObject, ptrT $ constT charT] $ objT c_Connection
  , just $ mkStaticMethod' "connect" "connectWithTypeStatic" [ptrT $ constT $ objT c_QObject, ptrT $ constT charT, ptrT $ constT $ objT c_QObject, ptrT $ constT charT, enumT e_ConnectionType] $ objT c_Connection
  , just $ mkStaticMethod' "connect" "connectWithSenderSignalStatic" [ptrT $ constT $ objT c_QObject, refT $ constT $ objT c_QMetaMethod, ptrT $ constT $ objT c_QObject, refT $ constT $ objT c_QMetaMethod] $ objT c_Connection
  , just $ mkStaticMethod' "connect" "connectWithSenderSignalTypeStatic" [ptrT $ constT $ objT c_QObject, refT $ constT $ objT c_QMetaMethod, ptrT $ constT $ objT c_QObject, refT $ constT $ objT c_QMetaMethod, enumT e_ConnectionType] $ objT c_Connection


  , just $ mkMethod "deleteLater" [] voidT

  , just $ mkConstMethod' "disconnect" "disconnect" [] boolT
  , just $ mkConstMethod' "disconnect" "disconnectWithReceiver" [ptrT $ constT $ objT c_QObject] boolT
  , just $ mkConstMethod' "disconnect" "disconnectWithMethod" [ptrT $ constT $ objT c_QObject, ptrT $ constT charT] boolT
  , just $ mkConstMethod' "disconnect" "disconnectWithSignal" [ptrT $ constT charT] boolT
  , just $ mkConstMethod' "disconnect" "disconnectWithSignalReceiver" [ptrT $ constT charT, ptrT $ constT $ objT c_QObject] boolT
  , just $ mkConstMethod' "disconnect" "disconnectWithSignalReceiverMethod" [ptrT $ constT charT, ptrT $ constT $ objT c_QObject, ptrT $ constT charT] boolT



  , just $ mkMethod "dumpObjectInfo" [] voidT
  , just $ mkMethod "dumpObjectTree" [] voidT
  , just $ mkConstMethod "dynamicPropertyNames" [] $ objT c_QListQByteArray
  , just $ mkMethod "event" [ptrT $ objT c_QEvent] boolT
  , just $ mkMethod "eventFilter" [ptrT $ objT c_QObject, ptrT $ objT c_QEvent] boolT
    -- TODO findChild
    -- TODO findChildren
  , just $ makeFnMethod (ident2 "qtah" "qobject" "inherits") "inherits" MConst Nonpure
    [objT c_QObject, objT c_QString] boolT
  , just $ mkMethod "installEventFilter" [ptrT $ objT c_QObject] voidT
  , just $ mkConstMethod "isWidgetType" [] boolT
  , -- This is a guess on the version bound.
    test (qtVersion >= [5, 0]) $ mkConstMethod "isWindowType" [] boolT
  , just $ mkMethod "killTimer" [intT] voidT
  , just $ mkConstMethod "metaObject" [] $ ptrT $ constT $ objT c_QMetaObject
  , just $ mkMethod "moveToThread" [ptrT $ objT c_QThread] voidT
  , just $ mkProp "objectName" $ objT c_QString
  , just $ mkProp "parent" $ ptrT $ objT c_QObject
  , just $ makeFnMethod (ident2 "qtah" "qobject" "property") "property" MConst Nonpure
    [objT c_QObject, objT c_QString] $ objT c_QVariant
  , just $ mkMethod "removeEventFilter" [ptrT $ objT c_QObject] voidT
  , just $ makeFnMethod (ident2 "qtah" "qobject" "setProperty") "setProperty" MNormal Nonpure
    [refT $ objT c_QObject, objT c_QString, objT c_QVariant] voidT
  , just $ mkConstMethod "signalsBlocked" [] boolT
  , just $ mkMethod "startTimer" [intT] intT
  , just $ mkConstMethod "thread" [] $ ptrT $ objT c_QThread
  ]

signals =
  [ makeSignal c_QObject "destroyed" c_ListenerPtrQObject
  , makeSignal c_QObject "objectNameChanged" c_ListenerQString
  ]

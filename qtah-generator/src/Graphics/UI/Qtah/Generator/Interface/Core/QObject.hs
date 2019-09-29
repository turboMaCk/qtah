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
  (~:),
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
  mkCtor,
  mkMethod,
  mkProp,
  np,
  )
import Foreign.Hoppy.Generator.Types (boolT, constT, intT, objT, ptrT, refT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QList (
  c_QListQByteArray,
  c_QListQObject,
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QEvent (c_QEvent)
import Graphics.UI.Qtah.Generator.Interface.Core.QMetaObject (c_QMetaObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QThread (c_QThread)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QVariant (c_QVariant)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  listenerPtrQObject,
  listenerQString,
  )
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QObject"] $
  (qtExport c_QObject) :
  map QtExportSignal signals

c_QObject =
  addReqIncludes [ includeStd "QObject"
                 , includeLocal "wrap_qobject.hpp"
                 ] $
  classSetEntityPrefix "" $
  makeClass (ident "QObject") Nothing [] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" ["parent" ~: ptrT $ objT c_QObject]
  , just $ mkMethod "blockSignals" ["block" ~: boolT] boolT
  , just $ mkMethod "children" np $ objT c_QListQObject
    -- TODO connect
  , just $ mkMethod "deleteLater" np voidT
    -- TODO disconnect
  , just $ mkMethod "dumpObjectInfo" np voidT
  , just $ mkMethod "dumpObjectTree" np voidT
  , just $ mkConstMethod "dynamicPropertyNames" np $ objT c_QListQByteArray
  , just $ mkMethod "event" ["event" ~: ptrT $ objT c_QEvent] boolT
  , just $ mkMethod "eventFilter"
    ["watched" ~: ptrT $ objT c_QObject, "event" ~: ptrT $ objT c_QEvent] boolT
    -- TODO findChild
    -- TODO findChildren
  , just $ makeFnMethod (ident2 "qtah" "qobject" "inherits") "inherits" MConst Nonpure
    ["" ~: objT c_QObject, "className" ~: objT c_QString] boolT
  , just $ mkMethod "installEventFilter" ["filterObj" ~: ptrT $ objT c_QObject] voidT
  , just $ mkConstMethod "isWidgetType" np boolT
  , -- This is a guess on the version bound.
    test (qtVersion >= [5, 0]) $ mkConstMethod "isWindowType" np boolT
  , just $ mkMethod "killTimer" ["id" ~: intT] voidT
  , just $ mkConstMethod "metaObject" np $ ptrT $ constT $ objT c_QMetaObject
  , just $ mkMethod "moveToThread" ["targetThread" ~: ptrT $ objT c_QThread] voidT
  , just $ mkProp "objectName" $ objT c_QString
  , just $ mkProp "parent" $ ptrT $ objT c_QObject
  , just $ makeFnMethod (ident2 "qtah" "qobject" "property") "property" MConst Nonpure
    ["" ~: objT c_QObject, "name" ~: objT c_QString] $ objT c_QVariant
  , just $ mkMethod "removeEventFilter" ["obj" ~: ptrT $ objT c_QObject] voidT
  , just $ makeFnMethod (ident2 "qtah" "qobject" "setProperty") "setProperty" MNormal Nonpure
    ["" ~: refT $ objT c_QObject, "name" ~: objT c_QString, "value" ~: objT c_QVariant] voidT
  , just $ mkConstMethod "signalsBlocked" np boolT
  , just $ mkMethod "startTimer" ["interval" ~: intT] intT
  , just $ mkConstMethod "thread" np $ ptrT $ objT c_QThread
  ]

signals =
  [ makeSignal c_QObject "destroyed" listenerPtrQObject
  , makeSignal c_QObject "objectNameChanged" listenerQString
  ]

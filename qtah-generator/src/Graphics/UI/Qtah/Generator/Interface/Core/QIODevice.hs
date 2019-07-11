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

module Graphics.UI.Qtah.Generator.Interface.Core.QIODevice (
  aModule,
  c_QIODevice,
  e_OpenModeFlag,
  bs_OpenMode,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportEnum, ExportBitspace),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkMethod',
  )
import Data.Bits ((.|.))  
import Foreign.Hoppy.Generator.Types (boolT, bitspaceT, constT, intT, objT, ptrT, refT, ptrT, voidT, charT, enumT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (c_Listener, c_ListenerQlonglong, c_ListenerIntQlonglong, c_ListenerInt)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qlonglong)

{-# ANN module "HLint: ignore Use camelCase" #-}


aModule =
  AQtModule $
  makeQtModule ["Core", "QIODevice"] $
  QtExport (ExportClass c_QIODevice) :
  map QtExportSignal signals ++
  [ QtExport $ ExportEnum e_OpenModeFlag
  , QtExport $ ExportBitspace bs_OpenMode
  ]

c_QIODevice =
  addReqIncludes [ includeStd "QIODevice" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QIODevice") Nothing [c_QObject] $
  collect
  [ --just $ mkCtor "new" []
   -- just $ mkCtor "newWithParent" [ptrT $ objT c_QObject]
    just $ mkConstMethod "atEnd" [] boolT
  , just $ mkConstMethod "bytesAvailable" [] qlonglong
  , just $ mkConstMethod "bytesToWrite" [] qlonglong
  , just $ mkConstMethod "canReadLine" [] boolT
  , just $ mkMethod "close" [] voidT
  , test (qtVersion >= [5, 7]) $ mkMethod "commitTransaction" [] voidT
  , test (qtVersion >= [5, 7]) $ mkConstMethod "currentReadChannel" [] intT
  , test (qtVersion >= [5, 7]) $ mkConstMethod "currentWriteChannel" [] intT
  , just $ mkConstMethod "errorString" [] $ objT c_QString
  , just $ mkMethod "getChar" [ptrT charT] boolT
  , just $ mkConstMethod "isOpen" [] boolT
  , just $ mkConstMethod "isReadable" [] boolT
  , just $ mkConstMethod "isSequential" [] boolT
  , just $ mkConstMethod "isTextModeEnabled" [] boolT
  , test (qtVersion >= [5, 7]) $ mkConstMethod "isTransactionStarted" [] boolT
  , just $ mkConstMethod "isWritable" [] boolT
  , just $ mkMethod "open" [bitspaceT bs_OpenMode] boolT
  , just $ mkConstMethod "openMode" [] $ bitspaceT bs_OpenMode
  , test (qtVersion >= [4, 1]) $ mkMethod' "peek" "peek" [ptrT charT, qlonglong] qlonglong
  , test (qtVersion >= [4, 1]) $ mkMethod' "peek" "peekWithBytearray" [qlonglong] $ objT c_QByteArray
  , just $ mkConstMethod "pos" [] qlonglong
  , just $ mkMethod "putChar" [charT] boolT
  , just $ mkMethod' "read" "read" [ptrT charT, qlonglong] qlonglong
  , just $ mkMethod' "read" "readWithBytearray" [qlonglong] $ objT c_QByteArray
  , just $ mkMethod "readAll" [] $ objT c_QByteArray
  , test (qtVersion >= [5, 7]) $ mkConstMethod "readChannelCount" [] intT
  , just $ mkMethod' "readLine" "readLineWithText" [ptrT charT, qlonglong] qlonglong
  , just $ mkMethod' "readLine" "readLine" [] $ objT c_QByteArray
  , just $ mkMethod' "readLine" "readLineWithBytearray" [qlonglong] $ objT c_QByteArray
  , just $ mkMethod "reset" [] boolT
  , test (qtVersion >= [5, 7]) $ mkMethod "rollbackTransaction" [] voidT
  , just $ mkMethod "seek" [qlonglong] boolT
  , test (qtVersion >= [5, 7]) $ mkMethod "setCurrentReadChannel" [intT] voidT
  , test (qtVersion >= [5, 7]) $ mkMethod "setCurrentWriteChannel" [intT] voidT
  , just $ mkMethod "setTextModeEnabled" [boolT] voidT
  , just $ mkConstMethod "size" [] qlonglong
  , test (qtVersion >= [5, 10]) $ mkMethod "skip" [qlonglong] qlonglong
  , test (qtVersion >= [5, 7]) $ mkMethod "startTransaction" [] voidT
  , just $ mkMethod "ungetChar" [charT] voidT
  , just $ mkMethod "waitForBytesWritten" [intT] boolT
  , just $ mkMethod "waitForReadyRead" [intT] boolT
  , just $ mkMethod' "write" "write" [ptrT $ constT charT, qlonglong] qlonglong
  , just $ mkMethod' "write" "writeWithText" [ptrT $ constT charT] qlonglong
  , just $ mkMethod' "write" "writeWithBytearray" [refT $ constT $ objT c_QByteArray] qlonglong
  , test (qtVersion >= [5, 7]) $ mkConstMethod "writeChannelCount" [] intT
  ]


(e_OpenModeFlag, bs_OpenMode) =
  makeQtEnumBitspace (ident1 "QIODevice" "OpenModeFlag") "OpenMode" [includeStd "QIODevice"] $
  let notOpen = 0x0000
      readOnly = 0x0001
      writeOnly = 0x0002
      append = 0x0004
      truncate = 0x0008
      text = 0x0010
      unbuffered = 0x0020
      newOnly = 0x0040
      existingOnly = 0x0080
      readWrite = readOnly .|. writeOnly
  in [ (notOpen, ["not", "open"])
      , (readOnly, ["read", "only"])
      , (writeOnly, ["write", "only"])
      , (readWrite, ["read", "write"])
      , (append, ["append"])
      , (truncate, ["truncate"])
      , (text, ["text"])
      , (unbuffered, ["unbuffered"])
      , (newOnly, ["new", "only"])
      , (existingOnly, ["existing", "only"])
     ]


signals =
  collect $
  [ just $ makeSignal c_QIODevice "aboutToClose" c_Listener
  , just $ makeSignal c_QIODevice "bytesWritten" c_ListenerQlonglong
  , test (qtVersion >= [5, 7]) $ makeSignal c_QIODevice "channelBytesWritten" c_ListenerIntQlonglong
  , test (qtVersion >= [5, 7]) $ makeSignal c_QIODevice "channelReadyRead" c_ListenerInt
  , test (qtVersion >= [4, 4]) $ makeSignal c_QIODevice "readChannelFinished" c_Listener
  , just $ makeSignal c_QIODevice "readyRead" c_Listener
  ]

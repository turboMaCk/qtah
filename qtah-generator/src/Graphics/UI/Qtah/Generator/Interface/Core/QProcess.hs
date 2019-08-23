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

module Graphics.UI.Qtah.Generator.Interface.Core.QProcess (
  aModule,
  c_QProcess,
  e_ExitStatus,
  e_InputChannelMode,
  e_ProcessChannel,
  e_ProcessChannelMode,
  e_ProcessError,
  e_ProcessState,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportEnum, ExportClass),
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
  mkStaticMethod,
  mkStaticMethod',
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, boolT, intT, longT, objT, refT, voidT, enumT, charT, constT, ptrT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QChar (c_QChar)
import Graphics.UI.Qtah.Generator.Interface.Core.QIODevice (c_QIODevice, bs_OpenMode)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QProcessEnvironment (c_QProcessEnvironment)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Internal.Listener (c_Listener, c_ListenerProcessError, c_ListenerIntExitStatus, c_ListenerProcessState)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qlonglong)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QProcess"] $
  QtExport (ExportClass c_QProcess) :
  map QtExportSignal signals ++
  [ QtExport $ ExportEnum e_ExitStatus
  , QtExport $ ExportEnum e_InputChannelMode
  , QtExport $ ExportEnum e_ProcessChannel
  , QtExport $ ExportEnum e_ProcessChannelMode
  , QtExport $ ExportEnum e_ProcessError
  , QtExport $ ExportEnum e_ProcessState
  ]

c_QProcess =
  addReqIncludes [includeStd "QProcess"] $
  classSetEntityPrefix "" $
  makeClass (ident "QProcess") Nothing [c_QIODevice] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QObject]
  , test (qtVersion >= [5, 0]) $ mkConstMethod "arguments" [] $ objT c_QStringList
  , just $ mkConstMethod "atEnd" [] boolT
  , just $ mkConstMethod "bytesAvailable" [] qlonglong
  , just $ mkConstMethod "bytesToWrite" [] qlonglong
  , just $ mkConstMethod "canReadLine" [] boolT
  , just $ mkMethod "close" [] voidT
  , just $ mkMethod "closeReadChannel" [enumT e_ProcessChannel] voidT
  , just $ mkMethod "closeWriteChannel" [] voidT
  -- TODO QProcess::CreateProcessArgumentModifier QProcess::createProcessArgumentsModifier() const
  , just $ mkConstMethod "error" [] $ enumT e_ProcessError
  , just $ mkStaticMethod' "execute" "execute" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QStringList] intT
  , just $ mkStaticMethod' "execute" "executeWithCommand" [refT $ constT $ objT c_QString] intT
  , just $ mkConstMethod "exitCode" [] intT
  , test (qtVersion >= [4, 1]) $ mkConstMethod "exitStatus" [] $ enumT e_ExitStatus
  , test (qtVersion >= [5, 2]) $ mkConstMethod "inputChannelMode" [] $ enumT e_InputChannelMode
  , just $ mkConstMethod "isSequential" [] boolT
 -- , test (qtVersion >= [4, 7]) $ mkConstMethod "nativeArguments" [] $ objT c_QString
  , test (qtVersion >= [5, 2]) $ mkStaticMethod "nullDevice" [] $ objT c_QString
  , just $ mkMethod' "open" "open" [] boolT
  , just $ mkMethod' "open" "openWithMode" [bitspaceT bs_OpenMode] boolT
  , test (qtVersion >= [4, 2]) $ mkConstMethod "processChannelMode" [] $ enumT e_ProcessChannelMode
  , test (qtVersion >= [4, 2]) $ mkConstMethod "processEnvironment" [] $ objT c_QProcessEnvironment
  , test (qtVersion >= [5, 3]) $ mkConstMethod "processId" [] qlonglong
  , test (qtVersion >= [5, 0]) $ mkConstMethod "program" [] $ objT c_QString
  , just $ mkMethod "readAllStandardError" [] $ objT c_QByteArray
  , just $ mkMethod "readAllStandardOutput" [] $ objT c_QByteArray
  , just $ mkConstMethod "readChannel" [] $ enumT e_ProcessChannel
  , test (qtVersion >= [5, 1]) $ mkMethod "setArguments" [refT $ constT $ objT c_QStringList] voidT
  --TODO void QProcess::setCreateProcessArgumentsModifier(QProcess::CreateProcessArgumentModifier modifier)
  , test (qtVersion >= [5, 2]) $ mkMethod "setInputChannelMode" [enumT e_InputChannelMode] voidT
 -- , test (qtVersion >= [4, 7]) $ mkMethod "setNativeArguments" [refT $ constT $ objT c_QString] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod "setProcessChannelMode" [enumT e_ProcessChannelMode] voidT
  , test (qtVersion >= [4, 6]) $ mkMethod "setProcessEnvironment" [refT $ constT $ objT c_QProcessEnvironment] voidT
  , test (qtVersion >= [5, 1]) $ mkMethod "setProgram" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod "setReadChannel" [enumT e_ProcessChannel] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod' "setStandardErrorFile" "setStandardErrorFile" [refT $ constT $ objT c_QString] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod' "setStandardErrorFile" "setStandardErrorFileWithMode" [refT $ constT $ objT c_QString, bitspaceT bs_OpenMode] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod "setStandardInputFile" [refT $ constT $ objT c_QString] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod' "setStandardOutputFile" "setStandardOutputFile" [refT $ constT $ objT c_QString] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod' "setStandardOutputFile" "setStandardOutputFileWithMode" [refT $ constT $ objT c_QString, bitspaceT bs_OpenMode] voidT
  , test (qtVersion >= [4, 2]) $ mkMethod "setStandardOutputProcess" [ptrT $ objT c_QProcess] voidT
  , just $ mkMethod "setWorkingDirectory" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "start" "start" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QStringList] voidT
  , just $ mkMethod' "start" "startWithProgramArgMode" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QStringList, bitspaceT bs_OpenMode] voidT
  , just $ mkMethod' "start" "startWithCommand" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "start" "startWithCommandMode" [refT $ constT $ objT c_QString, bitspaceT bs_OpenMode] voidT
  , test (qtVersion >= [5, 1]) $ mkMethod' "start" "startWithMode" [bitspaceT bs_OpenMode] voidT
  , test (qtVersion >= [5, 10]) $ mkMethod' "startDetached" "startDetached" [] boolT
  --, test (qtVersion >= [5, 10]) $ mkMethod' "startDetached" "startDetachedWithPid" [ptrT qlonglong] boolT
  , just $ mkStaticMethod' "startDetached" "startDetachedStatic" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QStringList] boolT
  , just $ mkStaticMethod' "startDetached" "startDetachedStaticDir" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QStringList, refT $ constT $ objT c_QString] boolT
  --, just $ mkStaticMethod' "startDetached" "startDetachedStaticDirPid" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QStringList, refT $ constT $ objT c_QString, ptrT qlonglong] boolT
  , just $ mkStaticMethod' "startDetached" "startDetachedStaticCommand" [refT $ constT $ objT c_QString] boolT
  , just $ mkConstMethod "state" [] $ enumT e_ProcessState
  , test (qtVersion >= [4, 1]) $ mkMethod "systemEnvironment" [] $ objT c_QStringList
  , just $ mkMethod' "waitForBytesWritten" "waitForBytesWritten" [] boolT
  , just $ mkMethod' "waitForBytesWritten" "waitForBytesWrittenMsecs" [intT] boolT
  , just $ mkMethod' "waitForFinished" "waitForFinished" [] boolT
  , just $ mkMethod' "waitForFinished" "waitForFinishedMsecs" [intT] boolT
  , just $ mkMethod' "waitForReadyRead" "waitForReadyRead" [] boolT
  , just $ mkMethod' "waitForReadyRead" "waitForReadyReadMsecs" [intT] boolT
  , just $ mkMethod' "waitForStarted" "waitForStarted" [] boolT
  , just $ mkMethod' "waitForStarted" "waitForStartedMsecs" [intT] boolT
  , just $ mkConstMethod "workingDirectory" [] $ objT c_QString

  , just $ mkMethod "kill" [] voidT
  , just $ mkMethod "terminate" [] voidT
  ]

e_ExitStatus =
    makeQtEnum (ident1 "QProcess" "ExitStatus") [includeStd "QProcess"]
    [ (0, ["normal", "exit"])
    , (1, ["crash", "exit"])
    ]

e_InputChannelMode =
    makeQtEnum (ident1 "QProcess" "InputChannelMode") [includeStd "QProcess"]
    [ (0, ["managed", "input", "channel"])
    , (1, ["forwarded", "input", "channel"])
    ]

e_ProcessChannel =
    makeQtEnum (ident1 "QProcess" "ProcessChannel") [includeStd "QProcess"]
    [ (0, ["standard", "output"])
    , (1, ["standard", "error"])
    ]

e_ProcessChannelMode =
    makeQtEnum (ident1 "QProcess" "ProcessChannelMode") [includeStd "QProcess"] $
    collect $
    [ just $ (0, ["separate", "channels"])
    , just $ (1, ["merged", "channels"])
    , just $ (2, ["forwarded", "channels"])
    , test (qtVersion >= [5, 2]) $ (4, ["forwarded", "error", "channel"])
    , test (qtVersion >= [5, 2]) $ (3, ["forwarded", "output", "channel"])
    ]

e_ProcessError =
    makeQtEnum (ident1 "QProcess" "ProcessError") [includeStd "QProcess"]
    [ (0, ["failed", "to", "start"])
    , (1, ["crashed"])
    , (2, ["timedout"])
    , (4, ["write", "error"])
    , (3, ["read", "error"])
    , (5, ["unknown", "error"])
    ]

e_ProcessState =
    makeQtEnum (ident1 "QProcess" "ProcessState") [includeStd "QProcess"]
    [ (0, ["not", "running"])
    , (1, ["starting"])
    , (2, ["running"])
    ]
    
signals =
  collect $
  [ test (qtVersion >= [5, 6]) $ makeSignal c_QProcess "errorOccurred" c_ListenerProcessError
  , just $ makeSignal c_QProcess "finished" c_ListenerIntExitStatus
  , just $ makeSignal c_QProcess "readyReadStandardError" c_Listener
  , just $ makeSignal c_QProcess "readyReadStandardOutput" c_Listener
  , just $ makeSignal c_QProcess "started" c_Listener
  , just $ makeSignal c_QProcess "stateChanged" c_ListenerProcessState
  ]

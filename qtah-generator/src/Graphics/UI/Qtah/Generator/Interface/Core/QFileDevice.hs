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

module Graphics.UI.Qtah.Generator.Interface.Core.QFileDevice (
  aModule,
  c_QFileDevice,
  e_FileError,
  e_FileHandleFlag,
  bs_FileHandleFlags,
  e_FileTime,
  e_MemoryMapFlags,
  e_Permission,
  bs_Permissions,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportEnum, ExportBitspace, ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkMethod,
  mkMethod',
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, ucharT, boolT, intT, objT, voidT, enumT, ptrT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QDateTime (c_QDateTime)
import Graphics.UI.Qtah.Generator.Interface.Core.QIODevice (c_QIODevice)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qlonglong)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QFileDevice"] [5, 0] $
  collect $
  [ just $ QtExport $ ExportClass c_QFileDevice
  , test (qtVersion >= [5, 10]) $ QtExport $ ExportEnum e_FileTime
  , test (qtVersion >= [4, 4]) $ QtExport $ ExportEnum e_MemoryMapFlags
  , just $ QtExport $ ExportEnum e_FileError
  , just $ QtExport $ ExportEnum e_FileHandleFlag
  , just $ QtExport $ ExportBitspace bs_FileHandleFlags
  , just $ QtExport $ ExportEnum e_Permission
  , just $ QtExport $ ExportBitspace bs_Permissions
  ]

c_QFileDevice =
  addReqIncludes [includeStd "QFileDevice"] $
  classSetEntityPrefix "" $
  makeClass (ident "QFileDevice") Nothing [c_QIODevice] $
  collect
  [ just $ mkConstMethod "error" [] $ enumT e_FileError
  , just $ mkConstMethod "fileName" [] $ objT c_QString
  , test (qtVersion >= [5, 10]) $ mkConstMethod "fileTime" [enumT e_FileTime] $ objT c_QDateTime
  , just $ mkMethod "flush" [] boolT
  , just $ mkConstMethod "handle" [] intT
  , just $ mkMethod' "map" "map" [qlonglong, qlonglong] $ ptrT ucharT
  , just $ mkMethod' "map" "mapWithFlags" [qlonglong, qlonglong, enumT e_MemoryMapFlags] $ ptrT ucharT
  , just $ mkConstMethod "permissions" [] $ bitspaceT bs_Permissions
  , just $ mkMethod "resize" [qlonglong] boolT
  , test (qtVersion >= [5, 10]) $
    mkMethod "setFileTime" [objT c_QDateTime, enumT e_FileTime] boolT
  , just $ mkMethod "setPermissions" [bitspaceT bs_Permissions] boolT
  , just $ mkMethod "unmap" [ptrT ucharT] boolT
  , just $ mkMethod "unsetError" [] voidT
  ]

e_FileError =
  makeQtEnum (ident1 "QFileDevice" "FileError") [includeStd "QFileDevice"]
  [ (0, ["no", "error"])
  , (1, ["read", "error"])
  , (2, ["write", "error"])
  , (3, ["fatal", "error"])
  , (4, ["resource", "error"])
  , (5, ["open", "error"])
  , (6, ["abort", "error"])
  , (7, ["time", "out", "error"])
  , (8, ["unspecified", "error"])
  , (9, ["remove", "error"])
  , (10, ["rename", "error"])
  , (11, ["position", "error"])
  , (12, ["resize", "error"])
  , (13, ["permissions", "error"])
  , (14, ["copy", "error"])
  ]

(e_FileHandleFlag, bs_FileHandleFlags) =
  makeQtEnumBitspace (ident1 "QFileDevice" "FileHandleFlag") "FileHandleFlags" [includeStd "QFileDevice"]
  [ (0, ["dont", "close", "handle"])
  , (0x0001, ["auto", "close", "handle"])
  ]

e_FileTime =
  makeQtEnum (ident1 "QFileDevice" "FileTime") [includeStd "QFileDevice"]
  [ (0, ["file", "access", "time"])
  , (1, ["file", "birth", "time"])
  , (2, ["file", "metadata", "change", "time"])
  , (3, ["file", "modification", "time"])
  ]

e_MemoryMapFlags =
  makeQtEnum (ident1 "QFileDevice" "MemoryMapFlags") [includeStd "QFileDevice"] $
  collect
  [ just (0, ["no", "options"])
  , test (qtVersion >= [5, 4]) (0x0001, ["standard", "error"])
  ]

(e_Permission, bs_Permissions) =
  makeQtEnumBitspace (ident1 "QFileDevice" "Permission") "Permissions" [includeStd "QFileDevice"]
  [ (0x4000, ["read", "owner"])
  , (0x2000, ["write", "owner"])
  , (0x1000, ["exe", "owner"])
  , (0x0400, ["read", "user"])
  , (0x0200, ["write", "user"])
  , (0x0100, ["exe", "user"])
  , (0x0040, ["read", "group"])
  , (0x0020, ["write", "group"])
  , (0x0010, ["exe", "group"])
  , (0x0004, ["read", "other"])
  , (0x0002, ["write", "other"])
  , (0x0001, ["exe", "other"])
  ]

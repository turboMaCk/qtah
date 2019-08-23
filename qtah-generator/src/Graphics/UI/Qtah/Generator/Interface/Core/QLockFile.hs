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

module Graphics.UI.Qtah.Generator.Interface.Core.QLockFile (
  aModule,
  c_QLockFile,
  e_LockError,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportEnum),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
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
  mkMethod',
  mkMethod
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Foreign.Hoppy.Generator.Types (boolT, intT, voidT, enumT, bitspaceT, constT, objT, ptrT, refT, llongT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qint64)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QLockFile"] [5, 1] $
  collect
  [ just $ QtExport $ ExportClass c_QLockFile
  , just $ QtExport $ ExportEnum e_LockError
  ]

c_QLockFile =
  addReqIncludes [ includeStd "QLockFile" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QLockFile") Nothing [] $
  collect
  [ just $ mkCtor "new" [refT $ constT $ objT c_QString]
  , just $ mkConstMethod "error" [] $ enumT e_LockError
  , just $ mkConstMethod "getLockInfo" [ptrT $ llongT, ptrT $ objT c_QString, ptrT $ objT c_QString] boolT
  , just $ mkConstMethod "isLocked" [] boolT
  , just $ mkMethod "lock" [] boolT
  , just $ mkMethod "removeStaleLockFile" [] boolT
  , just $ mkMethod "setStaleLockTime" [intT] voidT
  , just $ mkConstMethod "staleLockTime" [] intT
  , just $ mkMethod' "tryLock" "tryLock" [] boolT
  , just $ mkMethod' "tryLock" "tryLockWithTimeout" [intT] boolT
  , just $ mkMethod "unlock" [] voidT
  ]

e_LockError =
  makeQtEnum (ident1 "QLockFile" "LockError") [includeStd "QLockFile"]
  [ (0, ["no", "error"])
  , (1, ["lock", "failed", "error"])
  , (2, ["permission", "error"])
  , (3, ["unknown", "error"])
  ]

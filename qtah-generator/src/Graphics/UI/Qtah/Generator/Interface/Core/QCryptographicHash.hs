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

module Graphics.UI.Qtah.Generator.Interface.Core.QCryptographicHash (
  aModule,
  c_QCryptographicHash,
  e_Algorithm,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportEnum),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkStaticMethod,
  mkCtor,
  mkMethod',
  mkMethod
  )
import Foreign.Hoppy.Generator.Types (boolT, charT, intT, voidT, enumT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QIODevice (c_QIODevice)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QCryptographicHash"] [4, 3] $
  collect
  [ just $ QtExport $ ExportClass c_QCryptographicHash
  , just $ QtExport $ ExportEnum e_Algorithm
  ]

c_QCryptographicHash =
  addReqIncludes [ includeStd "QCryptographicHash" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QCryptographicHash") Nothing [] $
  collect
  [ just $ mkCtor "new" [enumT e_Algorithm]
  , just $ mkMethod' "addData" "addDataRaw" [ptrT $ constT charT, intT] voidT
  , just $ mkMethod' "addData" "addDataByteArray" [refT $ constT $ objT c_QByteArray] voidT
  , test (qtVersion >= [5, 0]) $ mkMethod' "addData" "addDataIODevice" [ptrT $ objT c_QIODevice] boolT
  , just $ mkStaticMethod "hash" [refT $ constT $ objT c_QByteArray, enumT e_Algorithm] $ objT c_QByteArray
  , test (qtVersion >= [5, 12]) $ mkStaticMethod "hashLength" [enumT e_Algorithm] intT
  , just $ mkMethod "reset" [] $ voidT
  , just $ mkConstMethod "result" [] $ objT c_QByteArray
  ]

e_Algorithm =
  makeQtEnum (ident1 "QCryptographicHash" "Algorithm") [includeStd "QCryptographicHash"] $
  collect
  [ just $ (0, ["md4"])
  , just $ (1, ["md5"])
  , just $ (2, ["sha1"])
  , test (qtVersion >= [5, 0]) $ (3, ["sha224"])
  , test (qtVersion >= [5, 0]) $ (4, ["sha256"])
  , test (qtVersion >= [5, 0]) $ (5, ["sha384"])
  , test (qtVersion >= [5, 0]) $ (6, ["sha512"])
  , test (qtVersion >= [5, 9, 2]) $ (7, ["keccak_224"])
  , test (qtVersion >= [5, 9, 2]) $ (8, ["keccak_256"])
  , test (qtVersion >= [5, 9, 2]) $ (9, ["keccak_384"])
  , test (qtVersion >= [5, 9, 2]) $ (10, ["keccak_512"])
  ]

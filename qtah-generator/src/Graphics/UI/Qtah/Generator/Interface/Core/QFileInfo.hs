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

module Graphics.UI.Qtah.Generator.Interface.Core.QFileInfo (
  aModule,
  c_QFileInfo,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  classSetConversionToGc,
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
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Copyable, Assignable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (boolT, uintT, voidT, enumT, bitspaceT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QDir (c_QDir)
import Graphics.UI.Qtah.Generator.Interface.Core.QFile (c_QFile)
import Graphics.UI.Qtah.Generator.Interface.Core.QFileDevice (e_FileTime, bs_Permissions)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QDateTime (c_QDateTime)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qint64)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QFileInfo"] $
  [QtExport $ ExportClass c_QFileInfo]

c_QFileInfo =
  addReqIncludes [ includeStd "QFileInfo" ] $
  classSetConversionToGc $
  classAddFeatures [Copyable, Assignable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QFileInfo") Nothing [] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithStr" [refT $ constT $ objT c_QString]
  , just $ mkCtor "newWithFile" [refT $ constT $ objT c_QFile]
  , just $ mkCtor "newWithDirStr" [refT $ constT $ objT c_QDir, refT $ constT $ objT c_QString]
  , just $ mkConstMethod "absoluteDir" [] $ objT c_QDir
  , just $ mkConstMethod "absoluteFilePath" [] $ objT c_QString
  , just $ mkConstMethod "absolutePath" [] $ objT c_QString
  , just $ mkConstMethod "baseName" [] $ objT c_QString
  , test (qtVersion >= [5, 10]) $ mkConstMethod "birthTime" [] $ objT c_QDateTime
  , test (qtVersion >= [4, 3]) $ mkConstMethod "bundleName" [] $ objT c_QString
  , just $ mkConstMethod "caching" [] $ boolT
  , just $ mkConstMethod "canonicalFilePath" [] $ objT c_QString
  , just $ mkConstMethod "canonicalPath" [] $ objT c_QString
  , just $ mkConstMethod "completeBaseName" [] $ objT c_QString
  , just $ mkConstMethod "completeSuffix" [] $ objT c_QString
  , just $ mkConstMethod "dir" [] $ objT c_QDir
  , just $ mkConstMethod' "exists" "exists" [] boolT
  , test (qtVersion >= [5, 2]) $ mkStaticMethod' "exists" "existsWithFile" [refT $ constT $ objT c_QString] boolT
  , just $ mkConstMethod "fileName" [] $ objT c_QString
  , just $ mkConstMethod "filePath" [] $ objT c_QString
  , test (qtVersion >= [5, 10]) $ mkConstMethod "fileTime" [enumT e_FileTime] $ objT c_QDateTime
  , just $ mkConstMethod "group" [] $ objT c_QString
  , just $ mkConstMethod "groupId" [] uintT
  , just $ mkConstMethod "isAbsolute" [] boolT
  , test (qtVersion >= [4, 3]) $ mkConstMethod "isBundle" [] boolT
  , just $ mkConstMethod "isDir" [] boolT
  , just $ mkConstMethod "isExecutable" [] boolT
  , just $ mkConstMethod "isFile" [] boolT
  , just $ mkConstMethod "isHidden" [] boolT
  , test (qtVersion >= [5, 10]) $ mkConstMethod "isNativePath" [] boolT
  , just $ mkConstMethod "isReadable" [] boolT
  , just $ mkConstMethod "isRelative" [] boolT
  , just $ mkConstMethod "isRoot" [] boolT
  , just $ mkConstMethod "isSymLink" [] boolT
  , just $ mkConstMethod "isWritable" [] boolT
  , just $ mkConstMethod "lastModified" [] $ objT c_QDateTime
  , just $ mkConstMethod "lastRead" [] $ objT c_QDateTime
  , just $ mkMethod "makeAbsolute" [] boolT
  , test (qtVersion >= [5, 10]) $ mkConstMethod "metadataChangeTime" [] $ objT c_QDateTime
  , just $ mkConstMethod "owner" [] $ objT c_QString
  , just $ mkConstMethod "ownerId" [] uintT
  , just $ mkConstMethod "path" [] $ objT c_QString
  , just $ mkConstMethod "permission" [bitspaceT bs_Permissions] boolT
  , just $ mkConstMethod "permissions" [] $ bitspaceT bs_Permissions
  , just $ mkMethod "refresh" [] voidT
  , just $ mkMethod "setCaching" [boolT] voidT
  , just $ mkMethod' "setFile" "setFileWithStr" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "setFile" "setFile" [refT $ constT $ objT c_QFile] voidT
  , just $ mkMethod' "setFile" "setFileWithDirStr" [refT $ constT $ objT c_QDir, refT $ constT $ objT c_QString] voidT
  , just $ mkConstMethod "size" [] $ qint64
  , just $ mkConstMethod "suffix" [] $ objT c_QString
  , test (qtVersion >= [5, 10]) $ mkMethod "swap" [refT $ objT c_QFileInfo] voidT
  , test (qtVersion >= [4, 2]) $ mkConstMethod "symLinkTarget" [] $ objT c_QString
  ]

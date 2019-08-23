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

module Graphics.UI.Qtah.Generator.Interface.Core.QMessageLogger (
  aModule,
  c_QMessageLogger,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod',
  mkCtor,
  )
import Foreign.Hoppy.Generator.Types (charT, intT, voidT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QDebug (c_QDebug)
import Graphics.UI.Qtah.Generator.Interface.Core.QLoggingCategory (c_QLoggingCategory)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QMessageLogger"] [5, 0] $
  [QtExport $ ExportClass c_QMessageLogger]

c_QMessageLogger =
  addReqIncludes [ includeStd "QMessageLogger" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QMessageLogger") Nothing [] $
  collect
  [ just $ mkCtor "newFull" [ptrT $ constT charT, intT, ptrT $ constT charT, ptrT $ constT charT]
  , just $ mkCtor "newShort" [ptrT $ constT $ charT, intT, ptrT $ constT $ charT]
  , just $ mkCtor "new" []
  , just $ mkConstMethod' "critical" "critical" [ptrT $ constT charT] voidT
  , test (qtVersion >= [5, 3]) $ mkConstMethod' "critical" "criticalWithQLoggingChar" [refT $ constT $ objT c_QLoggingCategory, ptrT $ constT charT] voidT
  --, test (qtVersion >= [5, 3]) $ mkConstMethod' "critical" "criticalWithCategoryFunc" [categoryFunction, ptrT $ constT charT] voidT
  , just $ mkConstMethod' "critical" "criticalQDebug" [] $ objT c_QDebug
  , test (qtVersion >= [5, 3]) $ mkConstMethod' "critical" "criticalWithQLogging" [refT $ constT $ objT c_QLoggingCategory] $ objT c_QDebug
  --, test (qtVersion >= [5, 3]) $ mkConstMethod' "critical" "criticalWithCategoryDebug" [categoryFunction] $ objT c_QDebug
  , just $ mkConstMethod' "debug" "debugWithChar" [ptrT $ constT charT] voidT
  , test (qtVersion >= [5, 3]) $ mkConstMethod' "debug" "debugWithQLoggingChar" [refT $ constT $ objT c_QLoggingCategory, ptrT $ constT charT] voidT
  --, test (qtVersion >= [5, 3]) $ mkConstMethod' "debug" "debugWithCategoryChar" [categoryFunction, ptrT $ constT charT] voidT
  , just $ mkConstMethod' "debug" "debug" [] $ objT c_QDebug
  , just $ mkConstMethod' "fatal" "fatalWithChar" [ptrT $ constT charT] voidT
  , test (qtVersion >= [5, 5]) $ mkConstMethod' "info" "infoWithChar" [ptrT $ constT charT] voidT
  , test (qtVersion >= [5, 5]) $ mkConstMethod' "info" "infoWithQLoggingChar" [refT $ constT $ objT c_QLoggingCategory, ptrT $ constT charT] voidT
  --, test (qtVersion >= [5, 5]) $ mkConstMethod' "info" "infoWithCategory" [categoryFunction, ptrT $ constT charT] voidT
  , test (qtVersion >= [5, 5]) $ mkConstMethod' "info" "info" [] $ objT c_QDebug
  , just $ mkConstMethod' "warning" "warningWithChar" [ptrT $ constT charT] voidT
  , test (qtVersion >= [5, 3]) $ mkConstMethod' "warning" "warningWithQLoggingChar" [refT $ constT $ objT c_QLoggingCategory, ptrT $ constT charT] voidT
  --, test (qtVersion >= [5, 3]) $ mkConstMethod' "warning" "warningWithCategory" [categoryFunction, ptrT $ constT charT] voidT
  , test (qtVersion >= [5, 3]) $ mkConstMethod' "warning" "warning" [] $ objT c_QDebug
  ]

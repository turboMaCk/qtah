-- This file is part of Qtah.
--
-- Copyright 2018-2019 The Qtah Authors.
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

module Graphics.UI.Qtah.Generator.Interface.Core.QDate (
  aModule,
  c_QDate,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportEnum),
  addReqIncludes,
  classSetConversionToGc,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkStaticMethod,
  mkStaticMethod',
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable, Comparable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (
  boolT, intT, int64T, objT, ptrT, enumT, voidT
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_DateFormat)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QDate"] $
  collect
  [ just $ QtExport $ ExportClass c_QDate
  , test (qtVersion >= [4, 5]) $ QtExport $ ExportEnum e_MonthNameType
  ]

c_QDate =
  addReqIncludes [includeStd "QDate"] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Copyable, Equatable, Comparable] $
  classSetEntityPrefix "" $
  makeClass (ident "QDate") Nothing [] $
  collect
  [
  -- Public Functions
    just $ mkCtor "new" []
  , just $ mkCtor "newWithYmd" [intT, intT, intT]
  , just $ mkConstMethod "addDays" [int64T] (objT c_QDate)
  , just $ mkConstMethod "addMonths" [intT] (objT c_QDate)
  , just $ mkConstMethod "addYears" [intT] (objT c_QDate)
  , just $ mkConstMethod "day" [] intT
  , just $ mkConstMethod "dayOfWeek" [] intT
  , just $ mkConstMethod "dayOfYear" [] intT
  , just $ mkConstMethod "daysInMonth" [] intT
  , just $ mkConstMethod "daysInYear" [] intT
  , just $ mkConstMethod "daysTo" [objT c_QDate] int64T
  , test (qtVersion >= [4, 5]) $ mkConstMethod "getDate" [ptrT $ intT, ptrT $ intT, ptrT $ intT] voidT
  , just $ mkConstMethod "isNull" [] boolT
  , just $ mkConstMethod "isValid" [] boolT
  , just $ mkConstMethod "month" [] intT
  , test (qtVersion >= [4, 2]) $ mkMethod "setDate" [intT, intT, intT] boolT
  , just $ mkConstMethod "toJulianDay" [] int64T
  , just $ mkConstMethod' "toString" "toStringWithStringFormat" [objT c_QString] (objT c_QString)
  , just $ mkConstMethod' "toString" "toStringWithDateFormat" [enumT e_DateFormat] (objT c_QString)
  , just $ mkConstMethod "toString" [] (objT c_QString)
  -- TODO just $ mkConstMethod' "toString" "toStringWithStringViewFormat"
  --      [objT c_QStringView] (objT c_QString)
  , just $ mkConstMethod "weekNumber" [] intT
  , just $ mkConstMethod' "weekNumber" "weekNumberWithYearNumber" [ptrT $ intT] intT
  , just $ mkConstMethod "year" [] intT

  -- Static Public Members
  , just $ mkStaticMethod "currentDate" [] (objT c_QDate)
  , just $ mkStaticMethod "fromJulianDay" [int64T] (objT c_QDate)
  , just $ mkStaticMethod "fromString" [objT c_QString] (objT c_QDate)
  , just $ mkStaticMethod' "fromString" "fromStringWithDateFormat" [objT c_QString, enumT e_DateFormat] (objT c_QDate)
  , just $ mkStaticMethod' "fromString" "fromStringWithStringFormat"
    [objT c_QString, objT c_QString] (objT c_QDate)
  , just $ mkStaticMethod "isLeapYear" [intT] boolT
  , just $ mkStaticMethod' "isValid" "isValidYmd" [intT, intT, intT] boolT
  ]

e_MonthNameType =
  makeQtEnum (ident1 "QDate" "MonthNameType") [includeStd "QDate"]
  [ (0, ["date", "format"])
  , (1, ["standalone", "format"])
  ]
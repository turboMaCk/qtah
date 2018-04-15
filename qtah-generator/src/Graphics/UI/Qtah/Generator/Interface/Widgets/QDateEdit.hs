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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QDateEdit (
  aModule,
  c_QDateEdit,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkCtor,
  np,
  )
import Foreign.Hoppy.Generator.Types (objT, ptrT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Interface.Core.QDate (c_QDate)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QDateTimeEdit (
  c_QDateTimeEdit,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QDateEdit"]
  [ qtExport c_QDateEdit ]

c_QDateEdit =
  addReqIncludes [includeStd "QDateEdit"] $
  classSetEntityPrefix "" $
  makeClass (ident "QDateEdit") Nothing [c_QDateTimeEdit] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , just $ mkCtor "newWithDate" [objT c_QDate]
  , just $ mkCtor "newWithDateAndParent" [objT c_QDate, ptrT $ objT c_QWidget]
  ]

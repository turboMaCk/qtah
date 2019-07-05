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

module Graphics.UI.Qtah.Generator.Interface.Core.QTranslator (
  aModule,
  c_QTranslator,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetConversionToGc,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkMethod',
  )
import Foreign.Hoppy.Generator.Types (
  boolT,
  constT,
  intT,
  objT,
  ptrT,
  refT,
  ucharT,
  charT,
  uintT,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QTranslator"]
  [ QtExport $ ExportClass c_QTranslator ]

c_QTranslator =
  addReqIncludes [ includeStd "QTranslator" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QTranslator") Nothing [c_QObject] $
  collect
  [
    just $ mkCtor "new" []
  , just $ mkCtor "newWithObject" [ptrT $ objT c_QObject]
  , just $ mkConstMethod "isEmpty" [] boolT

  , just $ mkMethod' "load" "loadFromString" [objT c_QString] boolT
  , just $ mkMethod' "load" "loadFromStringDir" [objT c_QString, objT c_QString] boolT
  , just $ mkMethod' "load" "loadFromStringDirSearch" [objT c_QString, objT c_QString, objT c_QString] boolT
  , just $ mkMethod' "load" "loadFromStringDirSearchSuff" [objT c_QString, objT c_QString, objT c_QString, objT c_QString] boolT
  
  -- TODO bool QTranslator::load(const QLocale &locale, const QString &filename, const QString &prefix = QString(), const QString &directory = QString(), const QString &suffix = QString())
  
  , just $ mkMethod' "load" "loadFromUChar" [ptrT $ constT ucharT, intT] boolT
  , just $ mkMethod' "load" "loadFromUCharDir" [ptrT $ constT ucharT, intT, objT c_QString] boolT

  , just $ mkConstMethod' "translate" "translate" [ptrT $ constT charT, ptrT $ constT charT] $ objT c_QString
  , just $ mkConstMethod' "translate" "translateWithDis" [ptrT $ constT charT, ptrT $ constT charT, ptrT $ constT charT] $ objT c_QString
  , just $ mkConstMethod' "translate" "translateWithDisNumb" [ptrT $ constT charT, ptrT $ constT charT, ptrT $ constT charT, intT] $ objT c_QString
  ]

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

module Graphics.UI.Qtah.Generator.Interface.Core.QDebug (
  aModule,
  c_QDebug,
  e_VerbosityLevel,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportEnum),
  Operator (OpShl),
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
  mkMethod',
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (
  boolT,
  constT,
  enumT,
  intT,
  objT,
  ptrT,
  refT,
  uintT,
  shortT,
  ushortT,
  doubleT,
  floatT,
  longT,
  ulongT,
  charT,
  voidT,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QChar (c_QChar)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_QtMsgType, qlonglong, qulonglong)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QDebug"] $
  collect
  [ just $ QtExport $ ExportClass c_QDebug
  , test (qtVersion >= [5, 13]) $ QtExport $ ExportEnum e_VerbosityLevel
  ]

c_QDebug =
  addReqIncludes [ includeStd "QDebug", includeStd "QtGlobal" ] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Copyable] $
  classSetEntityPrefix "" $
  makeClass (ident "QDebug") Nothing [] $
  collect
  [
    just $ mkCtor "newWithMsgType" [enumT e_QtMsgType]
  , just $ mkCtor "newWithString" [ptrT $ objT c_QString]
  -- TODO QDebug(QIODevice *device)
  , test (qtVersion >= [5, 0]) $ mkConstMethod "autoInsertSpaces" [] boolT
  , test (qtVersion >= [5, 4]) $ mkMethod' "maybeQuote" "maybeQuote" [] $ refT $ objT c_QDebug
  , test (qtVersion >= [5, 4]) $ mkMethod' "maybeQuote" "maybeQuoteWithChar" [charT] $ refT $ objT c_QDebug
  , just $ mkMethod "maybeSpace" [] $ refT $ objT c_QDebug
  , test (qtVersion >= [5, 4]) $ mkMethod "noquote" [] $ refT $ objT c_QDebug
  , just $ mkMethod "nospace" [] $ refT $ objT c_QDebug
  , test (qtVersion >= [5, 4]) $ mkMethod "quote" [] $ refT $ objT c_QDebug
  , test (qtVersion >= [5, 4]) $ mkMethod "resetFormat" [] $ refT $ objT c_QDebug
  , test (qtVersion >= [5, 0]) $ mkMethod "setAutoInsertSpaces" [boolT] voidT
  , test (qtVersion >= [5, 6]) $ mkMethod "setVerbosity" [intT] voidT
  , just $ mkMethod "space" [] $ refT $ objT c_QDebug
  , just $ mkMethod "swap" [refT $ objT c_QDebug] voidT
  , test (qtVersion >= [5, 13]) $ mkMethod' "verbosity" "verbosityWithLevel" [intT] $ refT $ objT c_QDebug
  , test (qtVersion >= [5, 6]) $ mkConstMethod' "verbosity" "verbosity" [] intT

  , just $ mkMethod' OpShl "qcharToStream" [objT c_QChar] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl "boolToStream" [boolT] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl "charToStream" [charT] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl "shortToStream" [shortT] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl "ushortToStream" [ushortT] $ refT $ objT c_QDebug
  --, just $ mkMethod OpShl [char16_t t] $ refT $ objT c_QDebug
  --, just $ mkMethod OpShl [char32_t t] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl "intToStream" [intT] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl "uintToStream" [uintT] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl "longToStream" [longT] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl "ulongToStream" [ulongT] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl "qlonglongToStream" [qlonglong] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl "qulonglongToStream" [qulonglong] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl "floatToStream" [floatT] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl "doubleToStream" [doubleT] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl "constCharToStream" [ptrT $ constT charT] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl "qstringToStream" [refT $ constT $ objT c_QString] $ refT $ objT c_QDebug
  --, just $ mkMethod OpShl [refT $ constT $ objT c_QStringRef] $ refT $ objT c_QDebug
  --, test (qtVersion >= [5, 10]) $ mkMethod OpShl [refT $ constT $ objT c_QStringView] $ refT $ objT c_QDebug
  --, just $ mkMethod OpShl [refT $ constT $ objT c_QLatin1String] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl "qbyteArrayToStream" [refT $ constT $ objT c_QByteArray] $ refT $ objT c_QDebug
  , just $ mkMethod' OpShl "voidToStream" [ptrT $ constT voidT] $ refT $ objT c_QDebug
  ]

e_VerbosityLevel =
  makeQtEnum (ident1 "QDebug" "VerbosityLevel") [includeStd "QDebug"]
  [ (0, ["minimum", "verbosity"])
  , (2, ["default", "verbosity"])
  , (7, ["maximum", "verbosity"])
  ]

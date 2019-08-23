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

module Graphics.UI.Qtah.Generator.Interface.Core.QString (
  aModule,
  c_QString,
  e_SectionFlag,
  bs_SectionFlags,
  e_NormalizationForm,
  e_SplitBehavior,
  ) where

import Foreign.Hoppy.Generator.Language.Haskell (
  addImports,
  sayLn,
  )
import Foreign.Hoppy.Generator.Spec (
  ClassHaskellConversion (
    ClassHaskellConversion,
    classHaskellConversionFromCppFn,
    classHaskellConversionToCppFn,
    classHaskellConversionType
  ),
  Export (ExportClass, ExportBitspace, ExportEnum),
  MethodApplicability (MNormal),
  Operator (OpArray),
  Purity (Nonpure),
  addReqIncludes,
  classSetEntityPrefix,
  classSetHaskellConversion,
  ident,
  ident1,
  ident2,
  includeLocal,
  includeStd,
  makeClass,
  makeFnMethod,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod',
  mkMethod
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Comparable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Std.String (c_string)
import Foreign.Hoppy.Generator.Types (toGcT, doubleT, charT, constT, intT, objT, ptrT, refT, voidT, longT, ulongT, uintT, shortT, ushortT, boolT, floatT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QChar (c_QChar)
import Graphics.UI.Qtah.Generator.Interface.Imports
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qulonglong, qlonglong)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QVector (c_QVectorUInt)
import Graphics.UI.Qtah.Generator.Types
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QString"]
  [ QtExport $ ExportClass c_QString
  , QtExport $ ExportEnum e_SectionFlag
  , QtExport $ ExportBitspace bs_SectionFlags
  , QtExport $ ExportEnum e_NormalizationForm
  , QtExport $ ExportEnum e_SplitBehavior
  ]

c_QString =
  addReqIncludes [includeStd "QString",
                  includeLocal "wrap_qstring.hpp"] $
  classAddFeatures [Assignable, Copyable, Comparable, Equatable] $
  classSetHaskellConversion
    ClassHaskellConversion
    { classHaskellConversionType = Just $ do
      addImports importForPrelude
      return $ HsTyCon $ UnQual $ HsIdent "QtahP.String"
    , classHaskellConversionToCppFn = Just $ do
      addImports $ mconcat [importForForeignC, importForPrelude]
      sayLn "QtahP.flip QtahFC.withCString newFromCString"
    , classHaskellConversionFromCppFn = Just $ sayLn "toStdString"
    } $
  classSetEntityPrefix "" $
  makeClass (ident "QString") Nothing [] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newFromByteArray" [objT c_QByteArray]
  , just $ mkCtor "newFromCString" [ptrT $ constT charT]
  , just $ mkCtor "newFromSizeChar" [intT, objT c_QChar]
  , just $ mkCtor "newFromChar" [ptrT $ objT c_QChar]
  --, just $ mkCtor "newFromCharSize" [ptrT $ objT c_QChar, intT]
  --, test (qtVersion >= [5, 2]) $ mkCtor "newFromRef" [refT $ objT c_QString]

  , just $ makeFnMethod (ident2 "qtah" "qstring" "set") "set" MNormal Nonpure
    [refT $ objT c_QString, intT, objT c_QChar] voidT

    , just $ mkMethod' "append" "append" [objT c_QString] $ refT $ objT c_QString
--, test (qtVersion >= [5, 0]) $ mkMethod' "append" "appendWithCharInt" [ptrT $ constT charT, intT] $ refT $ objT c_QString
  , just $ mkMethod' "append" "appendWithQChar" [objT c_QChar] $ refT $ objT c_QString
  -- TODO QString &QString::append(const QStringRef &reference)
  -- TODO QString &QString::append(QLatin1String str)
  , just $ mkMethod' "append" "appendWithChar" [ptrT $ constT charT] $ refT $ objT c_QString
  , just $ mkMethod' "append" "appendWithQByteArray" [objT c_QByteArray] $ refT $ objT c_QString

  , just $ mkConstMethod' "arg" "arg" [objT c_QString] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithStringSize" [objT c_QString, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithStringSizeFill" [objT c_QString, intT, objT c_QChar] $ objT c_QString

  , just $ mkConstMethod' "arg" "argWithULongLong" [qulonglong] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithULongLongField" [qulonglong, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithULongLongFieldBase" [qulonglong, intT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithULongLongFieldBaseFill" [qulonglong, intT, intT, objT c_QChar] $ objT c_QString

  , just $ mkConstMethod' "arg" "argWithLongLong" [qlonglong] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithLongLongField" [qlonglong, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithLongLongFieldBase" [qlonglong, intT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithLongLongFieldBaseFill" [qlonglong, intT, intT, objT c_QChar] $ objT c_QString

  , just $ mkConstMethod' "arg" "argWithLong" [longT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithLongField" [longT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithLongFieldBase" [longT, intT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithLongFieldBaseFill" [longT, intT, intT, objT c_QChar] $ objT c_QString

  , just $ mkConstMethod' "arg" "argWithULong" [ulongT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithULongField" [ulongT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithULongFieldBase" [ulongT, intT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithULongFieldBaseFill" [ulongT, intT, intT, objT c_QChar] $ objT c_QString

  , just $ mkConstMethod' "arg" "argWithInt" [intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithIntField" [intT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithIntFieldBase" [intT, intT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithIntFieldBaseFill" [intT, intT, intT, objT c_QChar] $ objT c_QString

  , just $ mkConstMethod' "arg" "argWithUInt" [uintT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithUIntField" [uintT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithUIntFieldBase" [uintT, intT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithUIntFieldBaseFill" [uintT, intT, intT, objT c_QChar] $ objT c_QString

  , just $ mkConstMethod' "arg" "argWithShort" [shortT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithShortField" [shortT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithShortFieldBase" [shortT, intT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithShortFieldBaseFill" [shortT, intT, intT, objT c_QChar] $ objT c_QString

  , just $ mkConstMethod' "arg" "argWithUShort" [ushortT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithUShortField" [ushortT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithUShortFieldBase" [ushortT, intT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithUShortFieldBaseFill" [ushortT, intT, intT, objT c_QChar] $ objT c_QString

  , just $ mkConstMethod' "arg" "argWithDouble" [doubleT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithDoubleField" [doubleT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithDoubleFieldFormat" [doubleT, intT, charT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithDoubleFieldFormatPrecision" [doubleT, intT, charT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithDoubleFieldFormatPrecisionFill" [doubleT, intT, charT, intT, objT c_QChar] $ objT c_QString

  , just $ mkConstMethod' "arg" "argWithChar" [charT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithCharField" [charT, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithCharFieldFill" [charT, intT, objT c_QChar] $ objT c_QString

  , just $ mkConstMethod' "arg" "argWithQChar" [objT c_QChar] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithQCharField" [objT c_QChar, intT] $ objT c_QString
  , just $ mkConstMethod' "arg" "argWithQCharFieldFill" [objT c_QChar, intT, objT c_QChar] $ objT c_QString

  -- TODO QString QString::arg(QStringView a, int fieldWidth = 0, QChar fillChar = QLatin1Char(' ')) const
  -- TODO QString QString::arg(QLatin1String a, int fieldWidth = 0, QChar fillChar = QLatin1Char(' ')) const

  , just $ mkConstMethod' "arg" "argTwoQStrings" [objT c_QString, objT c_QString] $ objT c_QString
  , just $ mkConstMethod' "arg" "argThreeQStrings" [objT c_QString, objT c_QString, objT c_QString] $ objT c_QString
  , just $ mkConstMethod' "arg" "argFourQStrings" [objT c_QString, objT c_QString, objT c_QString, objT c_QString] $ objT c_QString
  , just $ mkConstMethod' "arg" "argFiveQStrings" [objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString] $ objT c_QString
  , just $ mkConstMethod' "arg" "argSixQStrings" [objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString] $ objT c_QString
  , just $ mkConstMethod' "arg" "argSevenQStrings" [objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString] $ objT c_QString
  , just $ mkConstMethod' "arg" "argEightQStrings" [objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString] $ objT c_QString
  , just $ mkConstMethod' "arg" "argNineQStrings" [objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString, objT c_QString] $ objT c_QString

  , just $ mkConstMethod' OpArray "at" [intT] $ objT c_QChar
  , test (qtVersion >= [5, 10]) $ mkConstMethod "back" [] $ objT c_QChar
  -- TODO QCharRef QString::back()
  -- TODO QString::iterator QString::begin()
  -- TODO QString::const_iterator QString::begin() const

  , just $ mkConstMethod "capacity" [] intT

  -- TODO QString::const_iterator QString::cbegin() const
  -- TODO QString::const_iterator QString::cend() const

  , just $ mkMethod "chop" [intT] voidT
  , test (qtVersion >= [5, 10]) $ mkConstMethod "chopped" [intT] $ objT c_QString
  , just $ mkMethod "clear" [] voidT

  -- TODO compare methods
  -- TODO QString::const_iterator QString::constBegin() const
  -- TODO const QChar *QString::constData() const
  -- TODO QString::const_iterator QString::constEnd() const

  , test (qtVersion >= [5, 0]) $ mkConstMethod "toHtmlEscaped" [] $ objT c_QString
  , just $ mkConstMethod "toLatin1" [] $ objT c_QByteArray
  , just $ mkConstMethod "toLocal8Bit" [] $ objT c_QByteArray
  , just $ mkConstMethod "toStdString" [] $ objT c_string
  , just $ mkConstMethod "toUtf8" [] $ objT c_QByteArray
  -- TODO CFStringRef QString::toCFString() const
  , just $ mkConstMethod "toCaseFolded" [] $ objT c_QString

  , just $ mkConstMethod' "toDouble" "toDouble" [] doubleT
  , just $ mkConstMethod' "toDouble" "toDoubleOk" [ptrT boolT] doubleT

  , just $ mkConstMethod' "toFloat" "toFloat" [] floatT
  , just $ mkConstMethod' "toFloat" "toFloatOk" [ptrT boolT] floatT

  , just $ mkConstMethod' "toInt" "toInt" [] intT
  , just $ mkConstMethod' "toInt" "toIntOk" [ptrT boolT] intT
  , just $ mkConstMethod' "toInt" "toIntOkBase" [ptrT boolT, intT] intT

  , just $ mkConstMethod' "toLong" "toLong" [] longT
  , just $ mkConstMethod' "toLong" "toLongOk" [ptrT boolT] longT
  , just $ mkConstMethod' "toLong" "toLongOkBase" [ptrT boolT, intT] longT

  , just $ mkConstMethod' "toLongLong" "toLongLong" [] qlonglong
  , just $ mkConstMethod' "toLongLong" "toLongLongOk" [ptrT boolT] qlonglong
  , just $ mkConstMethod' "toLongLong" "toLongLongOkBase" [ptrT boolT, intT] qlonglong

  , just $ mkConstMethod "toLower" [] $ objT c_QString
  -- TODO NSString *QString::toNSString() const

  , just $ mkConstMethod' "toShort" "toShort" [] shortT
  , just $ mkConstMethod' "toShort" "toShortOk" [ptrT boolT] shortT
  , just $ mkConstMethod' "toShort" "toShortOkBase" [ptrT boolT, intT] shortT

  , just $ mkConstMethod' "toUInt" "toUInt" [] uintT
  , just $ mkConstMethod' "toUInt" "toUIntOk" [ptrT boolT] uintT
  , just $ mkConstMethod' "toUInt" "toUIntOkBase" [ptrT boolT, intT] uintT

  , just $ mkConstMethod' "toULong" "toULong" [] ulongT
  , just $ mkConstMethod' "toULong" "toULongOk" [ptrT boolT] ulongT
  , just $ mkConstMethod' "toULong" "toULongOkBase" [ptrT boolT, intT] ulongT

  , just $ mkConstMethod' "toULongLong" "toULongLong" [] qulonglong
  , just $ mkConstMethod' "toULongLong" "toULongLongOk" [ptrT boolT] qulonglong
  , just $ mkConstMethod' "toULongLong" "toULongLongOkBase" [ptrT boolT, intT] qulonglong

  , just $ mkConstMethod' "toUShort" "toUShort" [] ushortT
  , just $ mkConstMethod' "toUShort" "toUShortOk" [ptrT boolT] ushortT
  , just $ mkConstMethod' "toUShort" "toUShortOkBase" [ptrT boolT, intT] ushortT

  --, test (qtVersion >= [4, 2]) $ mkConstMethod "toUcs4" [] $ toGcT $ objT c_QVectorUInt

  , just $ mkConstMethod "toUpper" [] $ objT c_QString

  -- int QString::toWCharArray(wchar_t *array) const

  , just $ mkConstMethod "trimmed" [] $ objT c_QString
  , just $ mkMethod "truncate" [intT] voidT

  --, just $ mkConstMethod "unicode" [] $ objT c_QChar
  --, just $ mkConstMethod "utf16" [] $ ptrT ushortT
    -- TODO Lots more method here.
  ]

e_NormalizationForm =
    makeQtEnum (ident1 "QString" "NormalizationForm") [includeStd "QString"]
    [ (0, ["normalization", "form", "_", "d"])
    , (1, ["normalization", "form", "_", "c"])
    , (2, ["normalization", "form", "_", "k", "d"])
    , (3, ["normalization", "form", "_", "k", "c"])
    ]

(e_SectionFlag, bs_SectionFlags) =
    makeQtEnumBitspace (ident1 "QString" "SectionFlag") "SectionFlags" [includeStd "QString"]
    [ (0x00, ["section", "default"])
    , (0x01, ["section", "skip", "empty"])
    , (0x02, ["section", "include", "leading", "sep"])
    , (0x04, ["section", "include", "trailing", "sep"])
    , (0x08, ["section", "case", "insensitive", "seps"])
    ]

e_SplitBehavior =
    makeQtEnum (ident1 "QString" "SplitBehavior") [includeStd "QString"]
    [ (0, ["keep", "empty", "parts"])
    , (1, ["skip", "empty", "parts"])
    ]
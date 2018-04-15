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

module Graphics.UI.Qtah.Generator.Interface.Core.QChar (
  aModule,
  c_QChar,
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
  addReqIncludes,
  classSetEntityPrefix,
  classSetHaskellConversion,
  hsImport1,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod',
  mkStaticMethod,
  mkStaticMethod',
  np,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Comparable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (boolT, charT, intT, enumT, objT, refT, ucharT, ushortT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Config (qtVersion)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Imports
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QChar"] $
  collect
  [ just $ qtExport c_QChar
  , just $ qtExport e_Category
  , just $ qtExport e_Decomposition
  , test (qtVersion < [5, 3]) $ qtExport e_Joining
  , test (qtVersion >= [5, 3]) $ qtExport e_JoiningType
  , just $ qtExport e_Direction
  , just $ qtExport e_SpecialCharacter
  , just $ qtExport e_UnicodeVersion
  ]

-- TODO Add more QChar methods.
c_QChar =
  addReqIncludes [includeStd "QChar"] $
  classAddFeatures [Assignable, Copyable, Comparable, Equatable] $
  classSetHaskellConversion
    ClassHaskellConversion
    { classHaskellConversionType = Just $ do
      addImports importForPrelude
      return $ HsTyCon $ UnQual $ HsIdent "QtahP.Char"
    , classHaskellConversionToCppFn = Just $ do
      addImports $ mconcat [hsImport1 "Prelude" "(.)", importForChar, importForRuntime]
      sayLn "newFromInt . QtahFHR.coerceIntegral . QtahDC.ord"
    , classHaskellConversionFromCppFn = Just $ do
      addImports $ mconcat [hsImport1 "Prelude" "(.)", importForChar, importForPrelude,
                            importForRuntime]
      sayLn "QtahP.fmap (QtahDC.chr . QtahFHR.coerceIntegral) . unicode"
    } $
  classSetEntityPrefix "" $
  makeClass (ident "QChar") Nothing [] $
  collect
  [ just $ mkCtor "new" np
  , just $ mkCtor "newFromCellRow" [ucharT, ucharT]
  , just $ mkCtor "newFromInt" [intT]
  , just $ mkCtor "newFromSpecialCharacter" [enumT e_SpecialCharacter]
  , test (qtVersion < [5]) $ mkStaticMethod' "fromAscii" "newFromAscii" [charT] $ objT c_QChar
  , just $ mkStaticMethod' "fromLatin1" "newFromLatin1" [charT] $ objT c_QChar
  , just $ mkConstMethod "category" np $ enumT e_Category
  , just $ mkConstMethod "cell" np ucharT
  , just $ mkConstMethod "combiningClass" np ucharT
  , just $ mkStaticMethod "currentUnicodeVersion" np $ enumT e_UnicodeVersion
  , just $ mkConstMethod "decomposition" np $ objT c_QString
  , just $ mkConstMethod "decompositionTag" np $ enumT e_Decomposition
  , just $ mkConstMethod "digitValue" np intT
  , just $ mkConstMethod "direction" np $ enumT e_Direction
  , just $ mkConstMethod "hasMirrored" np boolT
  , just $ mkConstMethod "isDigit" np boolT
  , just $ mkConstMethod "isHighSurrogate" np boolT
  , just $ mkConstMethod "isLetter" np boolT
  , just $ mkConstMethod "isLetterOrNumber" np boolT
  , just $ mkConstMethod "isLowSurrogate" np boolT
  , just $ mkConstMethod "isLower" np boolT
  , just $ mkConstMethod "isMark" np boolT
  , just $ mkConstMethod "isNull" np boolT
  , just $ mkConstMethod "isNumber" np boolT
  , just $ mkConstMethod "isPrint" np boolT
  , just $ mkConstMethod "isPunct" np boolT
  , just $ mkConstMethod "isSpace" np boolT
  , just $ mkConstMethod "isSymbol" np boolT
  , just $ mkConstMethod "isTitleCase" np boolT
  , just $ mkConstMethod "isUpper" np boolT
  , test (qtVersion < [5, 3]) $ mkConstMethod "joining" np $ enumT e_Joining
  , test (qtVersion >= [5, 3]) $ mkConstMethod "joiningType" np $ enumT e_JoiningType
  , just $ mkConstMethod "mirroredChar" np $ objT c_QChar
  , just $ mkConstMethod "row" np ucharT
    -- TODO script (>=5.1)
  , test (qtVersion < [5]) $ mkConstMethod "toAscii" np charT
  , just $ mkConstMethod "toCaseFolded" np $ objT c_QChar
  , just $ mkConstMethod "toLatin1" np charT
  , just $ mkConstMethod "toLower" np $ objT c_QChar
  , just $ mkConstMethod "toTitleCase" np $ objT c_QChar
  , just $ mkConstMethod "toUpper" np $ objT c_QChar
  , just $ mkConstMethod' "unicode" "unicode" np ushortT
  , just $ mkMethod' "unicode" "unicodeRef" np $ refT ushortT
  , just $ mkConstMethod "unicodeVersion" np $ enumT e_UnicodeVersion
  ]

e_Category =
  makeQtEnum (ident1 "QChar" "Category") [includeStd "QChar"] $
  collect
  [ -- Normative.
    just "Mark_NonSpacing"
  , just "Mark_SpacingCombining"
  , just "Mark_Enclosing"
  , just "Number_DecimalDigit"
  , just "Number_Letter"
  , just "Number_Other"
  , just "Separator_Space"
  , just "Separator_Line"
  , just "Separator_Paragraph"
  , just "Other_Control"
  , just "Other_Format"
  , just "Other_Surrogate"
  , just "Other_PrivateUse"
  , just "Other_NotAssigned"
    -- Informative.
  , just "Letter_Uppercase"
  , just "Letter_Lowercase"
  , just "Letter_Titlecase"
  , just "Letter_Modifier"
  , just "Letter_Other"
  , just "Punctuation_Connector"
  , just "Punctuation_Dash"
  , just "Punctuation_Open"
  , just "Punctuation_Close"
  , just "Punctuation_InitialQuote"
  , just "Punctuation_FinalQuote"
  , just "Punctuation_Other"
  , just "Symbol_Math"
  , just "Symbol_Currency"
  , just "Symbol_Modifier"
  , just "Symbol_Other"
  , test (qtVersion <= [5, 0]) "NoCategory"
  ]

e_Decomposition =
  makeQtEnum (ident1 "QChar" "Decomposition") [includeStd "QChar"]
  [ "NoDecomposition"
  , "Canonical"
  , "Circle"
  , "Compat"
  , "Final"
  , "Font"
  , "Fraction"
  , "Initial"
  , "Isolated"
  , "Medial"
  , "Narrow"
  , "NoBreak"
  , "Small"
  , "Square"
  , "Sub"
  , "Super"
  , "Vertical"
  , "Wide"
  ]

e_Direction =
  makeQtEnum (ident1 "QChar" "Direction") [includeStd "QChar"]
  [ "DirAL"
  , "DirAN"
  , "DirB"
  , "DirBN"
  , "DirCS"
  , "DirEN"
  , "DirES"
  , "DirET"
  , "DirL"
  , "DirLRE"
  , "DirLRO"
  , "DirNSM"
  , "DirON"
  , "DirPDF"
  , "DirR"
  , "DirRLE"
  , "DirRLO"
  , "DirS"
  , "DirWS"
  ]

-- | Removed in Qt 5.3.0.
e_Joining =
  makeQtEnum (ident1 "QChar" "Joining") [includeStd "QChar"]
  [ "Center"
  , "Dual"
  , "OtherJoining"
  , "Right"
  ]

-- | Since Qt 5.3.0.
e_JoiningType =
  makeQtEnum (ident1 "QChar" "JoiningType") [includeStd "QChar"]
  [ "Joining_None"
  , "Joining_Causing"
  , "Joining_Dual"
  , "Joining_Right"
  , "Joining_Left"
  , "Joining_Transparent"
  ]

e_SpecialCharacter =
  makeQtEnum (ident1 "QChar" "SpecialCharacter") [includeStd "QChar"] $
  collect
  [ just "Null"
  , test (qtVersion >= [5, 0]) "Tabulation"
  , test (qtVersion >= [5, 0]) "LineFeed"
  , test (qtVersion >= [5, 0]) "CarriageReturn"
  , test (qtVersion >= [5, 0]) "Space"
  , just "Nbsp"
  , test (qtVersion >= [5, 0]) "SoftHyphen"
  , just "ReplacementCharacter"
  , just "ObjectReplacementCharacter"
  , just "ByteOrderMark"
  , just "ByteOrderSwapped"
  , just "ParagraphSeparator"
  , just "LineSeparator"
  , test (qtVersion >= [5, 0]) "LastValidCodePoint"
  ]

e_UnicodeVersion =
  makeQtEnum (ident1 "QChar" "UnicodeVersion") [includeStd "QChar"] $
  collect
  [ just "Unicode_1_1"
  , just "Unicode_2_0"
  , just "Unicode_2_1_2"
  , just "Unicode_3_0"
  , just "Unicode_3_1"
  , just "Unicode_3_2"
  , just "Unicode_4_0"
  , just "Unicode_4_1"
  , just "Unicode_5_0"
  , just "Unicode_5_1"
  , just "Unicode_5_2"
  , just "Unicode_6_0"
  , just "Unicode_6_1"
  , just "Unicode_6_2"
  , test (qtVersion >= [5, 3]) "Unicode_6_3"
  , test (qtVersion >= [5, 5]) "Unicode_7_0"
  , test (qtVersion >= [5, 6]) "Unicode_8_0"
  , test (qtVersion >= [5, 11]) "Unicode_9_0"
  , test (qtVersion >= [5, 11]) "Unicode_10_0"
  , just "Unicode_Unassigned"
  ]

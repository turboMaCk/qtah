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
  Export (ExportClass, ExportEnum),
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
  mkMethod,
  mkStaticMethod,
  mkStaticMethod',
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Comparable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (boolT, charT, intT, enumT, objT, refT, ucharT, ushortT, uintT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
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
  [ just $ QtExport $ ExportClass c_QChar
  , just $ QtExport $ ExportEnum e_Category
  , just $ QtExport $ ExportEnum e_Decomposition
  , test (qtVersion < [5, 3]) $ QtExport $ ExportEnum e_Joining
  , test (qtVersion >= [5, 3]) $ QtExport $ ExportEnum e_JoiningType
  , test (qtVersion >= [5, 1]) $ QtExport $ ExportEnum e_Script
  , just $ QtExport $ ExportEnum e_Direction
  , just $ QtExport $ ExportEnum e_SpecialCharacter
  , just $ QtExport $ ExportEnum e_UnicodeVersion
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
  [ just $ mkCtor "new" []
  , just $ mkCtor "newFromCellRow" [ucharT, ucharT]
  , just $ mkCtor "newFromInt" [intT]
  , just $ mkCtor "newFromSpecialCharacter" [enumT e_SpecialCharacter]
  , test (qtVersion < [5]) $ mkStaticMethod' "fromAscii" "newFromAscii" [charT] $ objT c_QChar
  , just $ mkConstMethod "category" [] $ enumT e_Category
  , just $ mkConstMethod "cell" [] ucharT
  , just $ mkConstMethod "combiningClass" [] ucharT
  , just $ mkConstMethod "decomposition" [] $ objT c_QString
  , just $ mkConstMethod "decompositionTag" [] $ enumT e_Decomposition
  , just $ mkConstMethod "digitValue" [] intT
  , just $ mkConstMethod "direction" [] $ enumT e_Direction
  , just $ mkConstMethod "hasMirrored" [] boolT
  , just $ mkConstMethod "isDigit" [] boolT
  , just $ mkConstMethod "isHighSurrogate" [] boolT
  , just $ mkConstMethod "isLetter" [] boolT
  , just $ mkConstMethod "isLetterOrNumber" [] boolT
  , just $ mkConstMethod "isLowSurrogate" [] boolT
  , just $ mkConstMethod "isLower" [] boolT
  , just $ mkConstMethod "isMark" [] boolT
  , just $ mkConstMethod "isNull" [] boolT
  , just $ mkConstMethod "isNumber" [] boolT
  , just $ mkConstMethod "isPrint" [] boolT
  , just $ mkConstMethod "isPunct" [] boolT
  , just $ mkConstMethod "isSpace" [] boolT
  , just $ mkConstMethod "isSymbol" [] boolT
  , just $ mkConstMethod "isTitleCase" [] boolT
  , just $ mkConstMethod "isUpper" [] boolT
  , test (qtVersion < [5, 3]) $ mkConstMethod "joining" [] $ enumT e_Joining
  , test (qtVersion >= [5, 3]) $ mkConstMethod "joiningType" [] $ enumT e_JoiningType
  , just $ mkConstMethod "mirroredChar" [] $ objT c_QChar
  , just $ mkConstMethod "row" [] ucharT
  , test (qtVersion >= [5, 1]) $ mkMethod "script" [uintT] $ enumT e_Script
  , test (qtVersion < [5]) $ mkConstMethod "toAscii" [] charT
  , just $ mkConstMethod "toCaseFolded" [] $ objT c_QChar
  , just $ mkConstMethod "toLatin1" [] charT
  , just $ mkConstMethod "toLower" [] $ objT c_QChar
  , just $ mkConstMethod "toTitleCase" [] $ objT c_QChar
  , just $ mkConstMethod "toUpper" [] $ objT c_QChar
  , just $ mkConstMethod' "unicode" "unicode" [] ushortT
  , just $ mkMethod' "unicode" "unicodeRef" [] $ refT ushortT
  , just $ mkConstMethod "unicodeVersion" [] $ enumT e_UnicodeVersion

  , just $ mkStaticMethod' "fromLatin1" "newFromLatin1" [charT] $ objT c_QChar
  , just $ mkStaticMethod "currentUnicodeVersion" [] $ enumT e_UnicodeVersion
  , just $ mkStaticMethod' "category" "categoryStatic" [uintT] $ enumT e_Category
  , just $ mkStaticMethod' "combiningClass" "combiningClassStatic" [uintT] ucharT
  , just $ mkStaticMethod' "decomposition" "decompositionStatic" [uintT] $ objT c_QString
  , just $ mkStaticMethod' "decompositionTag" "decompositionTagStatic" [uintT] $ enumT e_Decomposition
  , just $ mkStaticMethod' "digitValue" "digitValueStatic" [uintT] intT
  , just $ mkStaticMethod' "direction" "directionStatic" [uintT] $ enumT e_Direction
  , test (qtVersion >= [5]) $ mkStaticMethod' "hasMirrored" "hasMirroredStatic" [uintT] boolT
  , just $ mkStaticMethod "highSurrogate" [uintT] boolT
  , test (qtVersion >= [5]) $ mkStaticMethod' "isDigit" "isDigitStatic" [uintT] boolT
  , just $ mkStaticMethod' "isHighSurrogate" "isHighSurrogateStatic" [uintT] boolT
  , test (qtVersion >= [5]) $ mkStaticMethod' "isLetter" "isLetterStatic" [uintT] boolT
  , test (qtVersion >= [5]) $ mkStaticMethod' "isLetterOrNumber" "isLetterOrNumberStatic" [uintT] boolT
  , just $ mkStaticMethod' "isLowSurrogate" "isLowSurrogateStatic" [uintT] boolT
  , test (qtVersion >= [5]) $ mkStaticMethod' "isLower" "isLowerStatic" [uintT] boolT
  , test (qtVersion >= [5]) $ mkStaticMethod' "isMark" "isMarkStatic" [uintT] boolT
  , test (qtVersion >= [5]) $ mkStaticMethod "isNonCharacter" [uintT] boolT
  , test (qtVersion >= [5]) $ mkStaticMethod' "isNumber" "isNumberStatic" [uintT] boolT
  , test (qtVersion >= [5]) $ mkStaticMethod' "isPrint" "isPrintStatic" [uintT] boolT
  , test (qtVersion >= [5]) $ mkStaticMethod' "isPunct" "isPunctStatic" [uintT] boolT
  , test (qtVersion >= [5]) $ mkStaticMethod' "isSpace" "isSpaceStatic" [uintT] boolT
  , test (qtVersion >= [5]) $ mkStaticMethod "isSurrogate" [uintT] boolT
  , test (qtVersion >= [5]) $ mkStaticMethod' "isSymbol" "isSymbolStatic" [uintT] boolT
  , test (qtVersion >= [5]) $ mkStaticMethod' "isTitleCase" "isTitleCaseStatic" [uintT] boolT
  , test (qtVersion >= [5]) $ mkStaticMethod' "isUpper" "isUpperStatic" [uintT] boolT
  , test (qtVersion >= [5, 3]) $ mkStaticMethod' "joiningType" "joiningTypeStatic" [uintT] $ enumT e_JoiningType
  , just $ mkStaticMethod "lowSurrogate" [uintT] ushortT
  , just $ mkStaticMethod' "mirroredChar" "mirroredCharStatic" [uintT] uintT
  , just $ mkStaticMethod "requiresSurrogates" [uintT] boolT
  , test (qtVersion >= [5, 1]) $ mkStaticMethod' "script" "scriptStatic" [uintT] $ enumT e_Script
  , just $ mkStaticMethod' "surrogateToUcs4" "surrogateToUcs4" [ushortT, ushortT] uintT
  , just $ mkStaticMethod' "surrogateToUcs4" "surrogateToUcs4WithQChar" [objT c_QChar, objT c_QChar] uintT
  , just $ mkStaticMethod' "toCaseFolded" "toCaseFoldedStatic" [uintT] uintT
  , just $ mkStaticMethod' "toLower" "toLowerStatic" [uintT] uintT
  , just $ mkStaticMethod' "toTitleCase" "toTitleCaseStatic" [uintT] uintT
  , just $ mkStaticMethod' "toUpper" "toUpperStatic" [uintT] uintT
  , just $ mkStaticMethod' "unicodeVersion" "unicodeVersionStatic" [uintT] $ enumT e_UnicodeVersion
  ]

e_Category =
  makeQtEnum (ident1 "QChar" "Category") [includeStd "QChar"]
  [ -- Normative.
    (1, ["mark", "non", "spacing"])
  , (2, ["mark", "spacing", "combining"])
  , (3, ["mark", "enclosing"])
  , (4, ["number", "decimal", "digit"])
  , (5, ["number", "letter"])
  , (6, ["number", "other"])
  , (7, ["separator", "space"])
  , (8, ["separator", "line"])
  , (9, ["separator", "paragraph"])
  , (10, ["other", "control"])
  , (11, ["other", "format"])
  , (12, ["other", "surrogate"])
  , (13, ["other", "private", "use"])
  , (14, ["other", "not", "assigned"])
    -- Informative.
  , (15, ["letter", "uppercase"])
  , (16, ["letter", "lowercase"])
  , (17, ["letter", "titlecase"])
  , (18, ["letter", "modifier"])
  , (19, ["letter", "other"])
  , (20, ["punctuation", "connector"])
  , (21, ["punctuation", "dash"])
  , (22, ["punctuation", "open"])
  , (23, ["punctuation", "close"])
  , (24, ["punctuation", "initial", "quote"])
  , (25, ["punctuation", "final", "quote"])
  , (26, ["punctuation", "other"])
  , (27, ["symbol", "math"])
  , (28, ["symbol", "currency"])
  , (29, ["symbol", "modifier"])
  , (30, ["symbol", "other"])
  , (0, ["no", "category"])
  ]

e_Decomposition =
  makeQtEnum (ident1 "QChar" "Decomposition") [includeStd "QChar"]
  [ (0, ["no", "decomposition"])
  , (1, ["canonical"])
  , (8, ["circle"])
  , (16, ["compat"])
  , (6, ["final"])
  , (2, ["font"])
  , (17, ["fraction"])
  , (4, ["initial"])
  , (7, ["isolated"])
  , (5, ["medial"])
  , (13, ["narrow"])
  , (3, ["no", "break"])
  , (14, ["small"])
  , (15, ["square"])
  , (10, ["sub"])
  , (9, ["super"])
  , (11, ["vertical"])
  , (12, ["wide"])
  ]

e_Direction =
  makeQtEnum (ident1 "QChar" "Direction") [includeStd "QChar"]
  [ (13, ["dir", "al"])
  , (5, ["dir", "an"])
  , (7, ["dir", "b"])
  , (18, ["dir", "bn"])
  , (6, ["dir", "cs"])
  , (2, ["dir", "en"])
  , (3, ["dir", "es"])
  , (4, ["dir", "et"])
  , (0, ["dir", "l"])
  , (11, ["dir", "lre"])
  , (12, ["dir", "lro"])
  , (17, ["dir", "nsm"])
  , (10, ["dir", "on"])
  , (16, ["dir", "pdf"])
  , (1, ["dir", "r"])
  , (14, ["dir", "rle"])
  , (15, ["dir", "rlo"])
  , (8, ["dir", "s"])
  , (9, ["dir", "ws"])
  ]

-- | Removed in Qt 5.3.0.
e_Joining =
  makeQtEnum (ident1 "QChar" "Joining") [includeStd "QChar"]
  [ (3, ["center"])
  , (1, ["dual"])
  , (0, ["other", "joining"])
  , (2, ["right"])
  ]

-- | Since Qt 5.3.0.
e_JoiningType =
  makeQtEnum (ident1 "QChar" "JoiningType") [includeStd "QChar"]
  [ (0, ["joining", "none"])
  , (1, ["joining", "causing"])
  , (2, ["joining", "dual"])
  , (3, ["joining", "right"])
  , (4, ["joining", "left"])
  , (5, ["joining", "transparent"])
  ]

e_SpecialCharacter =
  makeQtEnum (ident1 "QChar" "SpecialCharacter") [includeStd "QChar"]
  [ (0x0000, ["null"])
  , (0x00a0, ["nbsp"])
  , (0x2028, ["line", "separator"])
  , (0x2029, ["paragraph", "separator"])
  , (0xfffc, ["object", "replacement", "character"])
  , (0xfffd, ["replacement", "character"])
  , (0xfeff, ["byte", "order", "mark"])
  , (0xfffe, ["byte", "order", "swapped"])
  ]

e_UnicodeVersion =
  makeQtEnum (ident1 "QChar" "UnicodeVersion") [includeStd "QChar"]
  [ (1, ["unicode", "1_1"])
  , (2, ["unicode", "2_0"])
  , (3, ["unicode", "2_1_2"])
  , (4, ["unicode", "3_0"])
  , (5, ["unicode", "3_1"])
  , (6, ["unicode", "3_2"])
  , (7, ["unicode", "4_0"])
  , (8, ["unicode", "4_1"])
  , (9, ["unicode", "5_0"])
  , (0, ["unicode", "unassigned"])
  ]

e_Script =
    makeQtEnum (ident1 "QChar" "Script") [includeStd "QChar"]
    [ (0, ["script_", "unknown"])
    , (1, ["script_", "inherited"])
    , (2, ["script_", "common"])
    , (3, ["script_", "latin"])
    , (4, ["script_", "greek"])
    , (5, ["script_", "cyrillic"])
    , (6, ["script_", "armenian"])
    , (7, ["script_", "hebrasshew"])
    , (8, ["script_", "arabic"])
    , (9, ["script_", "syriac"])
    , (10, ["script_", "thaana"])
    , (11, ["script_", "devanagari"])
    , (12, ["script_", "bengali"])
    , (13, ["script_", "gurmukhi"])
    , (14, ["script_", "gujarati"])
    , (15, ["script_", "oriya"])
    , (16, ["script_", "tamil"])
    , (17, ["script_", "telugu"])
    , (18, ["script_", "kannada"])
    , (19, ["script_", "malayalam"])
    , (20, ["script_", "sinhala"])
    , (21, ["script_", "thai"])
    , (22, ["script_", "lao"])
    , (23, ["script_", "tibetan"])
    , (24, ["script_", "myanmar"])
    , (25, ["script_", "georgian"])
    , (26, ["script_", "hangul"])
    , (27, ["script_", "ethiopic"])
    , (28, ["script_", "cherokee"])
    , (29, ["script_", "canadian", "aboriginal"])
    , (30, ["script_", "ogham"])
    , (31, ["script_", "runic"])
    , (32, ["script_", "khmer"])
    , (33, ["script_", "mongolian"])
    , (34, ["script_", "hiragana"])
    , (35, ["script_", "katakana"])
    , (36, ["script_", "bopomofo"])
    , (37, ["script_", "han"])
    , (38, ["script_", "yi"])
    , (39, ["script_", "old", "italic"])
    , (40, ["script_", "gothic"])
    , (41, ["script_", "deseret"])
    , (42, ["script_", "tagalog"])
    , (43, ["script_", "hanunoo"])
    , (44, ["script_", "buhid"])
    , (45, ["script_", "tagbanwa"])
    , (46, ["script_", "coptic"])
    , (47, ["script_", "limbu"])
    , (48, ["script_", "tai", "le"])
    , (49, ["script_", "linear", "b"])
    , (50, ["script_", "ugaritic"])
    , (51, ["script_", "shavian"])
    , (52, ["script_", "osmanya"])
    , (53, ["script_", "cypriot"])
    , (54, ["script_", "braille"])
    , (55, ["script_", "buginese"])
    , (56, ["script_", "new", "tai", "lue"])
    , (57, ["script_", "glagolitic"])
    , (58, ["script_", "tifinagh"])
    , (59, ["script_", "syloti", "nagri"])
    , (60, ["script_", "old", "persian"])
    , (61, ["script_", "kharoshthi"])
    , (62, ["script_", "balinese"])
    , (63, ["script_", "cuneiform"])
    , (64, ["script_", "phoenician"])
    , (65, ["script_", "phags", "pa"])
    , (66, ["script_", "nko"])
    , (67, ["script_", "sundanese"])
    , (68, ["script_", "lepcha"])
    , (69, ["script_", "ol", "chiki"])
    , (70, ["script_", "vai"])
    , (71, ["script_", "saurashtra"])
    , (72, ["script_", "kayah", "li"])
    , (73, ["script_", "rejang"])
    , (74, ["script_", "lycian"])
    , (75, ["script_", "carian"])
    , (76, ["script_", "lydian"])
    , (77, ["script_", "cham"])
    , (78, ["script_", "tai", "tham"])
    , (79, ["script_", "tai", "viet"])
    , (80, ["script_", "avestan"])
    , (81, ["script_", "egyptian", "hieroglyphs"])
    , (82, ["script_", "samaritan"])
    , (83, ["script_", "lisu"])
    , (84, ["script_", "bamum"])
    , (85, ["script_", "javanese"])
    , (86, ["script_", "meetei", "mayek"])
    , (87, ["script_", "imperial", "aramaic"])
    , (88, ["script_", "old", "south", "arabian"])
    , (89, ["script_", "inscriptional", "parthian"])
    , (90, ["script_", "inscriptional", "pahlavi"])
    , (91, ["script_", "old", "turkic"])
    , (92, ["script_", "kaithi"])
    , (93, ["script_", "batak"])
    , (94, ["script_", "brahmi"])
    , (95, ["script_", "mandaic"])
    , (96, ["script_", "chakma"])
    , (97, ["script_", "meroitic", "cursive"])
    , (98, ["script_", "meroitic", "hieroglyphs"])
    , (99, ["script_", "miao"])
    , (100, ["script_", "sharada"])
    , (101, ["script_", "sora", "sompeng"])
    , (102, ["script_", "takri"])
    , (103, ["script_", "caucasian", "albanian"])
    , (104, ["script_", "bassa", "vah"])
    , (105, ["script_", "duployan"])
    , (106, ["script_", "elbasan"])
    , (107, ["script_", "grantha"])
    , (108, ["script_", "pahawh", "hmong"])
    , (109, ["script_", "khojki"])
    , (110, ["script_", "linear", "a"])
    , (111, ["script_", "mahajani"])
    , (112, ["script_", "manichaean"])
    , (113, ["script_", "mende", "kikakui"])
    , (114, ["script_", "modi"])
    , (115, ["script_", "mro"])
    , (116, ["script_", "old", "north", "arabian"])
    , (117, ["script_", "nabataean"])
    , (118, ["script_", "palmyrene"])
    , (119, ["script_", "pau", "cin", "hau"])
    , (120, ["script_", "old", "permic"])
    , (121, ["script_", "psalter", "pahlavi"])
    , (122, ["script_", "siddham"])
    , (123, ["script_", "khudawadi"])
    , (124, ["script_", "tirhuta"])
    , (125, ["script_", "warang", "citi"])
    , (126, ["script_", "ahom"])
    , (127, ["script_", "anatolian", "hieroglyphs"])
    , (128, ["script_", "hatran"])
    , (129, ["script_", "multani"])
    , (130, ["script_", "old", "hungarian"])
    , (131, ["script_", "sign", "writing"])
    , (132, ["script_", "adlam"])
    , (133, ["script_", "bhaiksuki"])
    , (134, ["script_", "marchen"])
    , (135, ["script_", "newa"])
    , (136, ["script_", "osage"])
    , (137, ["script_", "tangut"])
    , (138, ["script_", "masaram", "gondi"])
    , (139, ["script_", "nushu"])
    , (140, ["script_", "soyombo"])
    , (141, ["script_", "zanabazar", "square"])
    ]
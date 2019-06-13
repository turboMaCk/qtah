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

module Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (
  aModule,
  c_QByteArray,
  e_Base64Option,
  bs_Base64Options,
  ) where

import Foreign.Hoppy.Generator.Language.Haskell (
  addImports,
  indent,
  ln,
  sayLn,
  )
import Foreign.Hoppy.Generator.Spec (
  Class,
  ClassHaskellConversion (
    ClassHaskellConversion,
    classHaskellConversionFromCppFn,
    classHaskellConversionToCppFn,
    classHaskellConversionType
  ),
  Export (ExportClass, ExportEnum, ExportBitspace),
  addAddendumHaskell,
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
  mkMethod,
  mkMethod'
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Comparable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (ushortT, shortT, uintT, floatT, doubleT, boolT, charT, constT, intT, ptrT, voidT, refT, objT, enumT)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qulonglong, qlonglong)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Imports
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_CaseSensitivity)
import Graphics.UI.Qtah.Generator.Types
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsType (HsTyCon),
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule :: AModule
aModule =
  AQtModule $
  makeQtModule ["Core", "QByteArray"] $
  collect
  [ just $ QtExport $ ExportClass c_QByteArray
  , test (qtVersion >= [5, 2]) $ QtExport $ ExportEnum e_Base64Option
  , test (qtVersion >= [5, 2]) $ QtExport $ ExportBitspace bs_Base64Options  
  ]

c_QByteArray :: Class
c_QByteArray =
  addReqIncludes [includeStd "QByteArray"] $
  addAddendum $
  classAddFeatures [Assignable, Copyable, Comparable, Equatable] $
  classSetHaskellConversion conversion $
  classSetEntityPrefix "" $
  makeClass (ident "QByteArray") Nothing [] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newFromData" [ptrT $ constT charT]
  , just $ mkCtor "newFromDataAndSize" [ptrT $ constT charT, intT]
  , just $ mkCtor "newFromRepeatedChar" [intT, charT]

  , just $ mkMethod' "append" "append" [objT c_QByteArray] $ refT $ objT c_QByteArray
  , test (qtVersion >= [5, 7]) $ mkMethod' "append" "appendWithIntChar" [intT, charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "append" "appendWithConstChar" [ptrT $ constT charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "append" "appendWithCharInt" [ptrT $ constT charT, intT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "append" "appendWithChar" [charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "append" "appendWithQString" [objT c_QString] $ refT $ objT c_QByteArray

  , just $ mkMethod' "insert" "insert" [intT, objT c_QByteArray] $ refT $ objT c_QByteArray
  , test (qtVersion >= [5, 7]) $ mkMethod' "insert" "insertWithIntCountChar" [intT, intT, charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "insert" "insertWithIntConstChar" [intT, ptrT $ constT charT] $ refT $ objT c_QByteArray
  , test (qtVersion >= [4, 6]) $  mkMethod' "insert" "insertWithIntConstCharLen" [intT, ptrT $ constT charT, intT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "insert" "insertWithIntChar" [intT, charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "insert" "insertWithIntQString" [intT, objT c_QString] $ refT $ objT c_QByteArray
  
  , just $ mkMethod' "prepend" "prepend" [objT c_QByteArray] $ refT $ objT c_QByteArray
  , test (qtVersion >= [5, 7]) $ mkMethod' "prepend" "prependWithIntChar" [intT, charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "prepend" "prependWithConstChar" [ptrT $ constT charT] $ refT $ objT c_QByteArray
  , test (qtVersion >= [4, 6]) $ mkMethod' "prepend" "prependWithConstCharInt" [ptrT $ constT charT, intT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "prepend" "prependWithChar" [charT] $ refT $ objT c_QByteArray
  
  , just $ mkMethod' "replace" "replace" [intT, intT, objT c_QByteArray] $ refT $ objT c_QByteArray
  , test (qtVersion >= [4, 7]) $ mkMethod' "replace" "replaceWithPosLenConstCharInt" [intT, intT, ptrT $ constT charT, intT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "replace" "replaceWithPosLenConstChar" [intT, intT, ptrT $ constT charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "replace" "replaceWithCharConstChar" [charT, ptrT $ constT charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "replace" "replaceWithCharQBytearray" [charT, objT c_QByteArray] $ refT $ objT c_QByteArray
  , just $ mkMethod' "replace" "replaceWithTwoConstChars" [ptrT $ constT charT, ptrT $ constT charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "replace" "replaceWithTwoConstCharsSizes" [ptrT $ constT charT, intT, ptrT $ constT charT, intT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "replace" "replaceWithTwoQBytearrays" [objT c_QByteArray, objT c_QByteArray] $ refT $ objT c_QByteArray
  , just $ mkMethod' "replace" "replaceWithQBytearrayConstChar" [objT c_QByteArray, ptrT $ constT charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "replace" "replaceWithConstCharQBytearray" [ptrT $ constT charT, objT c_QByteArray] $ refT $ objT c_QByteArray
  , just $ mkMethod' "replace" "replaceWithTwoChars" [charT, charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "replace" "replaceWithQStringConstChar" [objT c_QString, ptrT $ constT charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "replace" "replaceWithCharQString" [charT, objT c_QString] $ refT $ objT c_QByteArray
  , just $ mkMethod' "replace" "replaceWithQStringQBytearray" [objT c_QString, objT c_QByteArray] $ refT $ objT c_QByteArray
  
  , just $ mkConstMethod' "indexOf" "indexOf" [objT c_QByteArray] intT
  , just $ mkConstMethod' "indexOf" "indexOfFrom" [objT c_QByteArray, intT] intT
  , just $ mkConstMethod' "indexOf" "indexOfConstChar" [ptrT $ constT charT] intT
  , just $ mkConstMethod' "indexOf" "indexOfConstCharFrom" [ptrT $ constT charT, intT] intT
  , just $ mkConstMethod' "indexOf" "indexOfChar" [charT] intT
  , just $ mkConstMethod' "indexOf" "indexOfCharFrom" [charT, intT] intT
  , just $ mkConstMethod' "indexOf" "indexOfQString" [objT c_QString, intT] intT
  , just $ mkConstMethod' "indexOf" "indexOfQStringFrom" [objT c_QString, intT] intT

  , just $ mkConstMethod' "lastIndexOf" "lastIndexOf" [objT c_QByteArray] intT
  , just $ mkConstMethod' "lastIndexOf" "lastIndexOfFrom" [objT c_QByteArray, intT] intT
  , just $ mkConstMethod' "lastIndexOf" "lastIndexOfConstChar" [ptrT $ constT charT] intT
  , just $ mkConstMethod' "lastIndexOf" "lastIndexOfConstCharFrom" [ptrT $ constT charT, intT] intT
  , just $ mkConstMethod' "lastIndexOf" "lastIndexOfChar" [charT] intT
  , just $ mkConstMethod' "lastIndexOf" "lastIndexOfCharFrom" [charT, intT] intT
  , just $ mkConstMethod' "lastIndexOf" "lastIndexOfQString" [objT c_QString, intT] intT
  , just $ mkConstMethod' "lastIndexOf" "lastIndexOfQStringFrom" [objT c_QString, intT] intT

  , just $ mkMethod' "setNum" "setNumInt" [intT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "setNum" "setNumIntBase" [intT, intT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "setNum" "setNumUShort" [ushortT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "setNum" "setNumUShortBase" [ushortT, intT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "setNum" "setNumShort" [shortT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "setNum" "setNumShortBase" [shortT, intT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "setNum" "setNumUInt" [uintT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "setNum" "setNumUIntBase" [uintT, intT] $ refT $ objT c_QByteArray
  --, just $ mkMethod' "setNum" "setNumQLongLong" [qlonglong] $ refT $ objT c_QByteArray
  --, just $ mkMethod' "setNum" "setNumQLongLongBase" [qlonglong, intT] $ refT $ objT c_QByteArray
  --, just $ mkMethod' "setNum" "setNumQUlongLong" [qulonglong] $ refT $ objT c_QByteArray
  --, just $ mkMethod' "setNum" "setNumQUlongLongBase" [qulonglong, intT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "setNum" "setNumFloat" [floatT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "setNum" "setNumFloatChar" [floatT, charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "setNum" "setNumFloatCharInt" [floatT, charT, intT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "setNum" "setNumDouble" [doubleT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "setNum" "setNumDoubleChar" [doubleT, charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "setNum" "setNumDoubleCharInt" [doubleT, charT, intT] $ refT $ objT c_QByteArray

  , just $ mkConstMethod "at" [intT] charT
  , test (qtVersion >= [5, 10]) $ mkConstMethod "back" [] charT
  -- TODO QByteRef	back()
  -- TODO QByteArray::iterator	begin()
  -- TODO QByteArray::const_iterator	begin() const
  , just $ mkConstMethod "capacity" [] intT
  -- QByteArray::const_iterator	cbegin() const
  -- QByteArray::const_iterator	cend() const
  , just $ mkMethod "chop" [intT] voidT
  , test (qtVersion >= [5, 10]) $ mkConstMethod "chopped" [intT] $ objT c_QByteArray
  , test (qtVersion >= [5, 12]) $ mkConstMethod' "compare" "compare" [ptrT $ constT charT] intT
  , test (qtVersion >= [5, 12]) $ mkConstMethod' "compare" "compareWithCase" [ptrT $ constT charT, enumT e_CaseSensitivity] intT
  , test (qtVersion >= [5, 12]) $ mkConstMethod' "compare" "compareWithQBytearray" [objT c_QByteArray] intT
  , test (qtVersion >= [5, 12]) $ mkConstMethod' "compare" "compareWithQBytearrayCase" [objT c_QByteArray, enumT e_CaseSensitivity] intT
  -- TODO QByteArray::const_iterator	constBegin() const
  , just $ mkConstMethod "constData" [] $ ptrT $ constT charT
  -- TODO QByteArray::const_iterator	constEnd() const
  , just $ mkConstMethod' "contains" "contains" [objT c_QByteArray] boolT
  , just $ mkConstMethod' "contains" "containsWithConstChar" [ptrT $ constT charT] boolT
  , just $ mkConstMethod' "contains" "containsWithChar" [charT] boolT
  , just $ mkConstMethod' "count" "countWithQBytearray" [objT c_QByteArray] intT
  , just $ mkConstMethod' "count" "countWithConstChar" [ptrT $ constT charT] intT
  , just $ mkConstMethod' "count" "countWithChar" [charT] intT
  , just $ mkConstMethod' "count" "count" [] intT
  -- TODO QByteArray::const_reverse_iterator	crbegin() const
  -- TODO QByteArray::const_reverse_iterator	crend() const
  , just $ mkMethod "clear" [] voidT
  , just $ mkMethod' "data" "getData" [] $ ptrT charT
  , just $ mkConstMethod' "data" "getDataConst" [] $ ptrT $ constT charT
  -- TODO QByteArray::iterator	end()
  -- TODO QByteArray::const_iterator	end() const
  , just $ mkConstMethod' "endsWith" "endsWithQBytearray" [objT c_QByteArray] boolT
  , just $ mkConstMethod' "endsWith" "endsWithConstChar" [ptrT $ constT charT] boolT
  , just $ mkConstMethod' "endsWith" "endsWithChar" [charT] boolT
  , just $ mkMethod' "fill" "fill" [charT] $ refT $ objT c_QByteArray
  , just $ mkMethod' "fill" "fillWithSize" [charT, intT] $ refT $ objT c_QByteArray
  , test (qtVersion >= [5, 10]) $ mkConstMethod "front" [] charT
  -- TODO QByteRef	front()
  , just $ mkConstMethod "isEmpty" [] boolT
  , test (qtVersion >= [5, 12]) $ mkConstMethod "isLower" [] boolT
  , just $ mkConstMethod "isNull" [] boolT
  , test (qtVersion >= [5, 12]) $ mkConstMethod "isUpper" [] boolT
  , just $ mkConstMethod "left" [intT] $ objT c_QByteArray
  , just $ mkConstMethod' "leftJustified" "leftJustified" [intT] $ objT c_QByteArray
  , just $ mkConstMethod' "leftJustified" "leftJustifiedWithChar" [intT, charT] $ objT c_QByteArray
  , just $ mkConstMethod' "leftJustified" "leftJustifiedWithCharBool" [intT, charT, boolT] $ objT c_QByteArray
  , just $ mkConstMethod "length" [] intT
  , just $ mkConstMethod "size" [] intT
    -- TODO Lots more methods.
  ]

conversion :: ClassHaskellConversion
conversion =
  ClassHaskellConversion
  { classHaskellConversionType = Just $ do
    addImports importForByteString
    return $ HsTyCon $ UnQual $ HsIdent "QtahDBS.ByteString"
  , classHaskellConversionToCppFn = Just $ sayLn "convertToCpp"
  , classHaskellConversionFromCppFn = Just $ sayLn "convertFromCpp"
  }

addAddendum :: Class -> Class
addAddendum = addAddendumHaskell $ do
  addImports $ mconcat [hsImport1 "Prelude" "($)",
                        importForByteString,
                        importForByteStringUnsafe,
                        importForPrelude]
  ln
  sayLn "convertToCpp :: QtahDBS.ByteString -> QtahP.IO QByteArray"
  sayLn "convertToCpp ="
  indent $
    sayLn "QtahP.flip QtahDBSU.unsafeUseAsCStringLen $ QtahP.uncurry newFromDataAndSize"
  ln
  sayLn "convertFromCpp :: QByteArrayValue ba => ba -> QtahP.IO QtahDBS.ByteString"
  sayLn "convertFromCpp ba = do"
  indent $ do
    sayLn "d <- getDataConst ba"
    sayLn "len <- size ba"
    sayLn "QtahDBS.packCStringLen (d, len)"


(e_Base64Option, bs_Base64Options) =
  makeQtEnumBitspace (ident1 "QByteArray" "Base64Option") "Base64Options" [includeStd "QByteArray"]
  [ (0, ["base64", "encoding"])
  , (0, ["keep", "trailing", "equals"])
  , (1, ["base64", "url", "encoding"])
  , (2, ["omit", "trailing", "equals"])
  ]
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

module Graphics.UI.Qtah.Generator.Interface.Core.QXmlStreamWriter (
  aModule,
  c_QXmlStreamWriter,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod',
  mkMethod,
  mkProp
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QTextCodec (c_QTextCodec)
import Graphics.UI.Qtah.Generator.Interface.Core.QIODevice (c_QIODevice)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QXmlStreamReader (c_QXmlStreamReader)
import Graphics.UI.Qtah.Generator.Interface.Core.QXmlStreamAttributes (c_QXmlStreamAttributes)
import Graphics.UI.Qtah.Generator.Interface.Core.QXmlStreamAttribute (c_QXmlStreamAttribute)
import Foreign.Hoppy.Generator.Types (intT, boolT, charT, voidT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QXmlStreamWriter"] [4, 3] $
  [QtExport $ ExportClass c_QXmlStreamWriter]

c_QXmlStreamWriter =
  addReqIncludes [ includeStd "QXmlStreamWriter" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QXmlStreamWriter") Nothing [] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithQString" [ptrT $ objT c_QString]
  , just $ mkCtor "newWithBytearray" [ptrT $ objT c_QByteArray]
  , just $ mkCtor "newWithIODevice" [ptrT $ objT c_QIODevice]
  , test (qtVersion >= [4, 4]) $ mkProp "autoFormatting" boolT
  , test (qtVersion >= [4, 4]) $ mkProp "autoFormattingIndent" intT
  , just $ mkConstMethod "codec" [] $ ptrT $ objT c_QTextCodec
  , just $ mkConstMethod "device" [] $ ptrT $ objT c_QIODevice
  , just $ mkConstMethod "hasError" [] boolT
  , just $ mkMethod' "setCodec" "setCodecQText" [ptrT $ objT c_QTextCodec] voidT 
  , just $ mkMethod' "setCodec" "setCodecPtrchar" [ptrT $ constT charT] voidT
  , just $ mkMethod "setDevice" [ptrT $ objT c_QIODevice] voidT
  , just $ mkMethod' "writeAttribute" "writeAttributeUriNameValue" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "writeAttribute" "writeAttributeQualValue" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "writeAttribute" "writeAttribute" [refT $ constT $ objT c_QXmlStreamAttribute] voidT
  , just $ mkMethod "writeAttributes" [refT $ constT $ objT c_QXmlStreamAttributes] voidT
  , just $ mkMethod "writeCDATA" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod "writeCharacters" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod "writeComment" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod "writeCurrentToken" [refT $ constT $ objT c_QXmlStreamReader] voidT
  , just $ mkMethod "writeDTD" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod "writeDefaultNamespace" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "writeEmptyElement" "writeEmptyElementUriName" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "writeEmptyElement" "writeEmptyElement" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod "writeEndDocument" [] voidT
  , just $ mkMethod "writeEndElement" [] voidT
  , just $ mkMethod "writeEntityReference" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "writeNamespace" "writeNamespace" [refT $ constT $ objT c_QString ] voidT
  , just $ mkMethod' "writeNamespace" "writeNamespaceWithPref" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "writeProcessingInstruction" "writeProcessingInstruction" [refT $ constT $ objT c_QString ] voidT
  , just $ mkMethod' "writeProcessingInstruction" "writeProcessingInstructionWithData" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "writeStartDocument" "writeStartDocumentWithVer" [refT $ constT $ objT c_QString] voidT
  , test (qtVersion >= [4, 5]) $ mkMethod' "writeStartDocument" "writeStartDocument" [] voidT
  , test (qtVersion >= [4, 5]) $ mkMethod' "writeStartDocument" "writeStartDocumentWithVerStand" [refT $ constT $ objT c_QString, boolT] voidT
  , just $ mkMethod' "writeStartElement" "writeStartElementUriName" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "writeStartElement" "writeStartElementQual" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "writeTextElement" "writeTextElementUriNameText" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' "writeTextElement" "writeTextElementQualText" [refT $ constT $ objT c_QString, refT $ constT $ objT c_QString] voidT
  ]

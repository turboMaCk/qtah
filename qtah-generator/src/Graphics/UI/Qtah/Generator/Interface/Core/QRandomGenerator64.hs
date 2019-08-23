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

module Graphics.UI.Qtah.Generator.Interface.Core.QRandomGenerator64 (
  aModule,
  c_QRandomGenerator64,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Operator (OpCall),
  classSetConversionToGc,
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  ident2,
  includeLocal,
  includeStd,
  makeClass,
  makeFnMethod,
  mkConstMethod,
  mkConstMethod',
  mkStaticMethod,
  mkStaticMethod',
  mkCtor,
  mkMethod',
  mkMethod
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (doubleT, intT, uintT, ullongT, voidT, enumT, bitspaceT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (quint32, quint64, qsizetype)
import Graphics.UI.Qtah.Generator.Interface.Core.QRandomGenerator (c_QRandomGenerator)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QRandomGenerator64"] [5, 10] $
  [QtExport $ ExportClass c_QRandomGenerator64]

c_QRandomGenerator64 =
  addReqIncludes [ includeStd "QRandomGenerator64" ] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QRandomGenerator64") Nothing [] $
  collect
  [ just $ mkCtor "newWithQuints" [ptrT $ constT quint32, ptrT $ constT quint32]
  -- TODO QRandomGenerator::QRandomGenerator(std::seed_seq &sseq)
  , just $ mkCtor "newWithQuintQsizetype" [ptrT $ constT quint32, qsizetype]
  -- TODO QRandomGenerator::QRandomGenerator(const quint32 (&)[N] seedBuffer = ...)
  , just $ mkCtor "new" []
  , just $ mkCtor "newWithQuint" [quint32]
  , just $ mkMethod' "bounded" "boundedWithDouble" [doubleT] doubleT
  , just $ mkMethod' "bounded" "boundedWithQuint" [quint32] quint32
  , just $ mkMethod' "bounded" "boundedWirhQuints" [quint32, quint32] quint32
  , just $ mkMethod' "bounded" "boundedWithInt" [intT] intT
  , just $ mkMethod' "bounded" "boundedWithInts" [intT, intT] intT
  , just $ mkMethod "discard" [ullongT] $ voidT
  , just $ mkMethod "fillRange" [ptrT uintT, qsizetype] voidT
  -- TODO just $ mkMethod "fillRange" [ptrT $ uintT, qsizetype] $ voidT
  , just $ mkMethod "generate64" [] quint64
  , just $ mkMethod "generate" [] quint64
  -- TODO void QRandomGenerator::generate(ForwardIterator begin, ForwardIterator end)
  , just $ mkMethod "generateDouble" [] doubleT
  , just $ mkStaticMethod "global" [] $ ptrT $ objT c_QRandomGenerator
  , just $ mkStaticMethod "max" [] quint32
  , just $ mkStaticMethod "min" [] quint32
  , just $ mkStaticMethod "securelySeeded" [] $ objT c_QRandomGenerator
  , just $ mkMethod' "seed" "seed" [] voidT
  , just $ mkMethod' "seed" "seedWithQuint" [quint32] voidT
  -- TODO void QRandomGenerator::seed(std::seed_seq &seed)
  , just $ mkStaticMethod "system" [] $ ptrT $ objT c_QRandomGenerator
  , just $ mkMethod OpCall [] quint64
  ]

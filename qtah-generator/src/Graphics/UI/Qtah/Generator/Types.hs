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

module Graphics.UI.Qtah.Generator.Types (
  QtExport (..),
  qtExport,
  qtExportToExports,
  makeQtEnum,
  makeQtEnum',
  makeQtEnumAndFlags,
  makeQtEnumAndFlags',
  makeQtEnumAndFlagsWithOverrides,
  ListenerInfo (ListenerInfo),
  Signal, makeSignal, makeSignal',
  signalCName, signalHaskellName, signalClass, signalListenerClass, signalCallback,
  ) where

import qualified Data.Set as S
import Foreign.Hoppy.Generator.Spec (
  Callback,
  Class,
  CppEnum,
  Export (Export),
  Exportable,
  ForeignLanguage (Haskell),
  Function,
  Identifier,
  Include,
  addReqIncludes,
  enumAddEntryNameOverrides,
  enumSetHasBitOperations,
  enumSetUnknownValueEntry,
  enumSetValuePrefix,
  identifierParts,
  idPartBase,
  makeAutoEnum,
  toExtName,
  toExport,
  )
import Graphics.UI.Qtah.Generator.Flags (Flags, makeFlags)

data QtExport =
  QtExport Export
  | QtExportFnRenamed Function String
  | QtExportSignal Signal
  | QtExportEvent Class
  | QtExportSceneEvent Class
  | QtExportSpecials
    -- ^ This is a special value that is exported exactly once, and generates
    -- some bindings that need special logic.

qtExport :: Exportable a => a -> QtExport
qtExport = QtExport . toExport

qtExportToExports :: QtExport -> [Export]
qtExportToExports qtExport = case qtExport of
  QtExport export -> [export]
  QtExportFnRenamed fn _ -> [Export fn]
  QtExportSignal {} -> []
  QtExportEvent cls -> [Export cls]
  QtExportSceneEvent cls -> [Export cls]
  QtExportSpecials -> []

-- | Creates a 'CppEnum' whose 'ExtName' is the concatenation of all part of its
-- 'Identifier'.  This should be used for all Qt enums.
makeQtEnum :: Identifier -> [Include] -> [String] -> CppEnum
makeQtEnum identifier includes names =
  makeQtEnum' identifier
              False  -- Most Qt enums are unscoped.
              includes
              names

-- | Creates a 'CppEnum' like 'makeQtEnum' does, but also takes a boolean
-- parameter indicating whether the enum is scoped.
makeQtEnum' :: Identifier -> Bool -> [Include] -> [String] -> CppEnum
makeQtEnum' identifier scoped includes names =
  addReqIncludes includes $
  enumSetValuePrefix "" $
  enumSetUnknownValueEntry ("Unknown" ++ niceName) $
  enumSetHasBitOperations False $
  addEntryOverrides $
  makeAutoEnum identifier
               (Just $ toExtName niceName)
               scoped
               names
  where niceName = concatMap idPartBase $ identifierParts identifier
        addEntryOverrides = enumAddEntryNameOverrides Haskell applicableOverrides
        applicableOverrides = filter (\(from, _) -> S.member from nameSet) enumNameOverrides
        nameSet = S.fromList names

-- | Creates a 'CppEnum' and 'Flags' pair, with the same entries and related
-- names.
makeQtEnumAndFlags :: Identifier -> String -> [Include] -> [String] -> (CppEnum, Flags)
makeQtEnumAndFlags enumIdentifier flagsName includes names =
  let enum = makeQtEnum enumIdentifier includes names
      flags = makeFlags enum flagsName
  in (enum, flags)

-- | Creates a 'CppEnum' and 'Flags' pair like 'makeQtEnumAndFlags' does, but
-- also takes a boolean parameter indicating whether the enum is scoped.
makeQtEnumAndFlags' :: Identifier -> String -> Bool -> [Include] -> [String] -> (CppEnum, Flags)
makeQtEnumAndFlags' enumIdentifier flagsName scoped includes names =
  let enum = makeQtEnum' enumIdentifier scoped includes names
      flags = makeFlags enum flagsName
  in (enum, flags)

-- | This version of 'makeQtEnumAndFlags' accepts entry name overrides, which is
-- useful because flag bindings can conflict with method names (they're both
-- Haskell identifiers starting with lower-case letters).
makeQtEnumAndFlagsWithOverrides ::
  Identifier -> String -> [Include] -> [String] -> [(String, String)] -> (CppEnum, Flags)
makeQtEnumAndFlagsWithOverrides enumIdentifier flagsName includes names nameOverrides =
  let enum = enumAddEntryNameOverrides Haskell nameOverrides $
             makeQtEnum enumIdentifier includes names
      flags = makeFlags enum flagsName
  in (enum, flags)

-- | Global enum entry name overrides.  These are applied to all enum entries,
-- to handle the cases where they overlap with Haskell keywords.
--
-- TODO Fill these out based on enums we're defined so far.
enumNameOverrides :: [(String, String)]
enumNameOverrides =
  [ ("Type", "Typ")
  ]

-- | Specification for a signal in the Qt signals and slots framework.
data Signal = Signal
  { signalClass :: Class
    -- ^ The class to which the signal belongs.
  , signalCName :: String
    -- ^ The C name of the signal, without parameters, e.g. @"clicked"@.
  , signalHaskellName :: String
    -- ^ The base name of the Haskell binding for the signal.  Normally the same
    -- as the C name.
  , signalListenerClass :: Class
    -- ^ An appropriately typed listener class.
  , signalCallback :: Callback
    -- ^ The callback type used by the listener.
  }

data ListenerInfo = ListenerInfo Class Callback

makeSignal :: Class  -- ^ 'signalClass'
           -> String  -- ^ 'signalCName'
           -> ListenerInfo  -- ^ 'signalListenerClass' and 'signalCallback'.
           -> Signal
makeSignal cls cName (ListenerInfo listenerClass callback) =
  Signal cls cName cName listenerClass callback

makeSignal' :: Class  -- ^ 'signalClass'
            -> String  -- ^ 'signalCName'
            -> String  -- ^ 'signalHaskellName'
            -> ListenerInfo  -- ^ 'signalListenerClass' and 'signalCallback'.
            -> Signal
makeSignal' cls cName hsName (ListenerInfo listenerClass callback) =
  Signal cls cName hsName listenerClass callback

-- This file is part of Qtah.
--
-- Copyright 2015-2020 The Qtah Authors.
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

module Graphics.UI.Qtah.Generator.Interface.Gui.QValidator (
  aModule,
  c_QValidator,
  e_State,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  )
import Foreign.Hoppy.Generator.Types (enumT, intT, objT, refT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (listener)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QValidator"] $
  [ QtExportClassAndSignals c_QValidator signals
  , qtExport e_State
  ]

(c_QValidator, signals) =
  makeQtClassAndSignals signalGens $
  addReqIncludes [includeStd "QValidator"] $
  classSetEntityPrefix "" $
  makeClass (ident "QValidator") Nothing [c_QObject]
  [ mkConstMethod "fixup" [refT $ objT c_QString] voidT
    -- TODO locale
    -- TODO setLocale
  , mkConstMethod "validate" [refT $ objT c_QString, refT intT] $ enumT e_State
  ]

signalGens :: [SignalGen]
signalGens =
  [ makeSignal "changed" listener
  ]

e_State =
  makeQtEnum (ident1 "QValidator" "State") [includeStd "QValidator"]
  [ "Invalid"
  , "Intermediate"
  , "Acceptable"
  ]

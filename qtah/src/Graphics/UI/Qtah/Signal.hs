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

{-# LANGUAGE MultiParamTypeClasses
            ,FlexibleInstances
            ,FlexibleContexts
            ,TypeSynonymInstances
            ,UndecidableInstances
            ,ScopedTypeVariables
            ,TemplateHaskell
            ,ExistentialQuantification
            ,RankNTypes
            ,OverloadedStrings #-}

-- | General routines for managing Qt signals.
module Graphics.UI.Qtah.Signal (
  Signal (..),
  nullptr,
  connect_,
  disconnect_,
  disconnectWithConn_,
  ) where

import Control.Monad (unless)
import Graphics.UI.Qtah.Core.Connection (Connection)
import Graphics.UI.Qtah.Generated.Core.QObject (QObject) 
import {-# SOURCE #-} qualified Graphics.UI.Qtah.Object as Object


nullptr :: Bool -> IO ()
nullptr = \_ -> return ()

-- | Universal disconnection taking argument with QMetaObject::Connection type, which is always a result of all connections
disconnectWithConn_ :: Connection -> IO Bool
disconnectWithConn_ = Object.disconnectWithConn_


-- | A signal that can be connected to an instance of the @object@ (C++) class,
-- and when invoked will call a function of the given @handler@ type.
data Signal object handler = Signal
  { internalConnectSignal :: object -> handler -> IO Connection
  , internalDisconnectSignal :: object -> handler -> IO Bool
  , internalName :: String
  }

instance Show (Signal object handler) where
  show signal = concat ["<Signal ", internalName signal, ">"]


-- | Registers a handler function to listen to a signal an object emits, via
-- 'connect'. 
connect_ :: object -> Signal object handler -> handler -> IO Connection
connect_ object signal handler = internalConnectSignal signal object handler


-- | Registers a handler function to listen to a signal an object emits, via
-- 'disconnect'. 
disconnect_ :: object -> Signal object handler -> handler -> IO Bool
disconnect_ object signal handler = internalDisconnectSignal signal object handler

-- | Registers a handler function to listen to a signal an object emits, via
-- 'connect'.  If the connection fails, then the program aborts.
--disconnect_ :: Connection -> Signal object handler -> object -> handler -> IO Bool
--disconnect_ connection signal object handler = internalDisconnectSignal signal connection 

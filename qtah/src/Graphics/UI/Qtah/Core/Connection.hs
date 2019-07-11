{-# LANGUAGE FlexibleContexts, FlexibleInstances, ForeignFunctionInterface, GeneralizedNewtypeDeriving, MonoLocalBinds, MultiParamTypeClasses, ScopedTypeVariables, TypeSynonymInstances, UndecidableInstances #-}
---------- GENERATED FILE, EDITS WILL BE LOST ----------

{-# LANGUAGE NoMonomorphismRestriction #-}

module Graphics.UI.Qtah.Core.Connection (
  ConnectionValue (..),
  ConnectionConstPtr (..),
  ConnectionPtr (..),
  ConnectionConst,
  Connection,
  castConst,
  cast,
  downCastConst,
  downCast,
  decode,
  new,
  newCopy,
  aSSIGN,
  ) where

import qualified Foreign.Hoppy.Runtime as QtahFHR
import Graphics.UI.Qtah.Generated.Core.Connection
import qualified Graphics.UI.Qtah.Generated.Core.Connection as M44
import Prelude ()
import qualified Prelude as QtahP


castConst = toConnectionConst
cast = toConnection
downCastConst = downToConnectionConst
downCast = downToConnection

decode :: (ConnectionConstPtr this) => this -> QtahP.IO M44.Connection
decode = QtahFHR.decode QtahP.. toConnectionConst
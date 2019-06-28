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

-- Is it worse to allow bitwise operations for enum values in places where
-- they're not allowed, or to force the user to use separate identifiers for
-- flags values?

module Graphics.UI.Qtah.Flags (
  Flags (..),
  IsFlags (..),
  numToFlags,
  flagsToNum,
  ) where

import Foreign.Hoppy.Runtime (CppEnum, fromCppEnum, toCppEnum)

class CppEnum n e => Flags n e f | e -> f, f -> e, f -> n where
  enumToFlags :: e -> f

  flagsToEnum :: f -> e

numToFlags :: Flags n e f => n -> f
numToFlags = enumToFlags . toCppEnum

flagsToNum :: Flags n e f => f -> n
flagsToNum = fromCppEnum . flagsToEnum

-- | A class of types that can be convered to @QFlags@ instances.  This class is
-- used as a constraint in functions that expect Qt flags values as arguments.
class IsFlags f a | a -> f where
  toFlags :: a -> f

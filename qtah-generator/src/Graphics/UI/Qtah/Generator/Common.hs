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

-- | General routines.
module Graphics.UI.Qtah.Generator.Common (
  splitOn,
  butLast,
  replaceLast,
  fromMaybeM,
  maybeFail,
  firstM,
  lowerFirst,
  ) where

import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.Char (toLower)
import Data.Foldable (asum)
import Data.List (findIndex)

-- | Splits a list at elements for which a predicate returns true.  The matching
-- elements themselves are dropped.
splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen _ [] = []
splitWhen f xs = case findIndex f xs of
  Just index -> let (term, _:rest) = splitAt index xs
                in term : splitWhen f rest
  Nothing -> [xs]

-- | Splits a list on a specified element.
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x = splitWhen (== x)

-- | Drops the last item from the list, if non-empty.
butLast :: [a] -> [a]
butLast [] = []
butLast xs = take (length xs - 1) xs

-- | Replaces the last element in a list, if the list is non-empty.  Returns the
-- empty list when given it.
replaceLast :: a -> [a] -> [a]
replaceLast _ [] = []
replaceLast y [_] = [y]
replaceLast y (x:xs) = x:replaceLast y xs

-- | @fromMaybeM m x = maybe m return x@
fromMaybeM :: Monad m => m a -> Maybe a -> m a
fromMaybeM = flip maybe return

-- | @maybeFail s x = maybe (fail s) x@
maybeFail :: Monad m => String -> Maybe a -> m a
maybeFail = fromMaybeM . fail

-- | Runs a list of monadic actions until one returns a 'Just' value, then
-- returning that value.  Returns 'Nothing' if all actions return 'Nothing'.
firstM :: (Functor m, Monad m) => [m (Maybe a)] -> m (Maybe a)
firstM = runMaybeT . asum . map MaybeT

-- | Lower cases the first character of a string, if nonempty.
lowerFirst :: String -> String
lowerFirst "" = ""
lowerFirst (c:cs) = toLower c : cs

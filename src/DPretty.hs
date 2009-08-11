{-
    The Delve Programming Language
    Copyright 2009 John Morrice

    Distributed under the terms of the GNU General Public License v3, or ( at your option ) any later version.

    This file is part of Delve.

    Delve is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Delve is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Delve.  If not, see <http://www.gnu.org/licenses/>.
-}

-- Inside the DelveVM, some of the pretty printing is monadic, since objects must read IORefs to print their contents
-- So pretty printing is atm a little slapdash
-- This module holds some useful combinators
module DPretty where

import Data.List.Stream as L

mk_spaces :: Int -> ShowS
mk_spaces n =
   (if n > 0 then (:) '\n' else id ) . (L.++) ( L.concat $ L.replicate n "   " )

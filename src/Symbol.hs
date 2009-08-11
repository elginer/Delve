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

-- Symbols are used everywhere, so it's best to make it a module on its own
module Symbol
       ( Symbol 
       , render_path
       )
       where

import Data.ByteString.Char8 as B
import Data.List.Stream as L

-- A symbol is a packed string ( for speed )
type Symbol =
   ByteString

-- render a path, OO-style
render_path :: [ Symbol ] -> Symbol
render_path path = 
   L.foldl B.append B.empty ( L.intersperse ( B.singleton '.' ) path )


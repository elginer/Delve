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

-- random tools for operating on Haskell's AST
module HsAST 
   ( module Language.Haskell.Exts
   , a_src_loc
   , exp_name
   )
   where

import Symbol
import Data.ByteString.Char8 as B

import Language.Haskell.Exts

a_src_loc :: SrcLoc
a_src_loc = SrcLoc "" 0 0


exp_name :: Symbol -> Name
exp_name = name . B.unpack

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

-- this module contains some clumsy and inefficient combinators which are amusingly enough vital for the operation of the Compiler
module Util where

import Data.List.Stream as L

new_list :: a -> [ a ]
new_list = return 

drop_lst :: [ a ] -> [ a ]
drop_lst ( a : b@( _ : _ ) ) =
   a : drop_lst b
drop_lst _ =
   [ ]

uncons :: a -> [ a ] -> [ a ]
uncons = flip (L.++ ) . new_list

-- this very is inefficient
-- seperate the last 
sep :: [ a ] -> ( a , [ a ] )
sep lexec =
   let lst   = L.last lexec
       unlst = drop_lst lexec
   in  ( lst , unlst )
 

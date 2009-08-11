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

{-# OPTIONS -XTypeFamilies #-}
module Compiler 
   ( compile_error
   , Compiler ( .. )
   )
   where

import Data.List.Stream as L

compile_error msg =
   error $ "FATAL DUNE COMPILER ERROR\nThe possible happened!\nUnless you were hacking on the Delve compiler, then you should send this error message, with the code you were trying to compile as a bug report to spoon@killersmurf.com.  Cheers!\n\n" L.++ msg

-- The type class of compilers
class Compiler c where
   data Source c
   data Target c
   compile :: Source c -> Target c

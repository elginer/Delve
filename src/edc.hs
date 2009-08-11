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

-- The Elgin Delve Compiler
module Main ( extend , main , bytecode_compile ) where
import Phases

import Data.Maybe
import Data.List.Stream as L

import Control.Arrow

import System.Environment

-- Get the args, check if they're empty, if not we might be in business.
main = do
   args <- getArgs
   if L.null args
      then usage
      else obey_args args

-- For when folk are stuck
usage = do
   n <- getProgName
   putStrLn $
      "____Elgin Delve Compiler Help____\n" L.++
      "\n***To compile Delve source into dcode***\n" L.++
      '\n' : n L.++ " source_file+ edc_option*\n" L.++
      "\nBy default, the output file is named corresponding to the name of the last source_file given.\n" L.++
      "\nFor example, for the command:\n" L.++
      n L.++ " HTTP.delve httpd.delve\n" L.++
      "The output file would be called 'httpd.dcode'\n" L.++
      "\nAvailable edc_options:\n" L.++
      "\n-o output_file\n" L.++
      "-o allows the user to choose the name of the output file.\n" L.++
      "Arguments specified after the name of the output file are silently ignored. ( Because of laziness on the part of the author )\n" L.++
      "\n***To extend the virtual machine***\n" L.++
      '\n' : n L.++ " extend source_file+ [-g ghc_option*]\n" L.++
      "\nThe file 'edvm.hs' is expected to be in the current directory, as the virtual machine extension will be compiled against it.\n" L.++
      "\nExample:\n" L.++
      n L.++ " Object.edelve Bool.edelve Int.edelve -g -O2\n" L.++
      "Creates a virtual machine from the embedded delve files Object.edelve, Bool.edelve and Int.edelve, and optimizes with O2.\n" L.++
      meaning_of_symbols L.++
      "\n\nPlease be advised that this is an in development version of Delve: it doesn't have a type-system yet!\nBut you can still have a lot of fun :)"

-- Describing the meaning of symbols in usage
meaning_of_symbols = 
   "\nMeaning of symbols:\n" L.++
   "term+ denotes one or more of term\n" L.++
   "term* denotes zero or more of term\n" L.++
   "[ term ] denotes that the term is optional - the term occurs only once OR not at all."

-- Carry out the instructions given! promptly! no mucking about!
obey_args args =
   let cmd = L.head args
   in  case cmd of
          "extend" -> extend $ L.tail args 
          _        -> bytecode_compile args -- if 'extend' is not specified, then assume bytecode compilation
           
-- Compile to bytecode
bytecode_compile args =
   let ( fs , output_file_list ) = L.span ((/=) "-o" ) args 
   in  dcode fs $ listToMaybe $ L.drop 1 output_file_list

-- Extend the virtual machine with embedded delve
extend args =
   if L.null args 
      then do 
         putStrLn "ERROR:\n'extend' requires one or more filenames as arguments.\n"     
         usage
      else
         let ( fs , ghc_opts ) = second ( intercalate " " . L.drop 1 ) $ L.span ((/=) "-g" ) args
         in  do
           embedded ghc_opts fs
           return ( )
             


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

-- The Elgin Delve Virtual Machine
{-# OPTIONS -XBangPatterns #-}
import DelveVM ( run_delve )
import VirtualMachineExtension
import DelveFormats

import System.Environment

import Data.Binary
import Data.Binary.Get

import Data.List.Stream as L

import qualified Data.ByteString.Lazy.Char8 as Z

import System.IO

import Control.Exception

-- Get the arguments
-- Get the handle relating the name of the program to be executed
-- Get the dcode from the handle
-- Run delve with the dcode appended to the bytecode in the extended virtual machine
main = do
   as <- getArgs
   name <- get_prog_name as
   progbs <- read_bs name
   case from_dcode progbs of -- strictly - to make sure we have no decoding errors!
      !dcode ->
         run_delve $ bytecode L.++ dcode

-- Get the name of the program
get_prog_name :: [ String ] -> IO FilePath
get_prog_name as =
   if L.length as /= 1
      then usage
      else return $ L.head as

-- print a helpful message
usage :: IO a
usage = do
   n <- getProgName
   error $ "Usage:\n" L.++
           n L.++ " prog.dcode"
   

-- Get the contents of the file to be run
read_bs :: FilePath -> IO Z.ByteString
read_bs name =
   onException ( Z.readFile name )
               ( error $ "Error while reading '" L.++ name L.++ "'\nAre you sure it exists?" )

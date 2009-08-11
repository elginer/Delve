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

module Phases 
   ( 
      dcode 
   ,  dcodify
   ,  embedded
   )
   where

-- an interface to various compiler phases

import VarApp
import ResolveLocal
import SExp
import Compiler ( compile ) 
import VMCompiler
import Embed
import VMASTCompiler
import DelveFormats

import System.Cmd
import System.Exit

import qualified Control.Exception as X
import Control.Monad.Stream as C

import SerializeUtil
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as Z
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.List.Stream as L

import Language.Haskell.Exts

-- | Compile embedded haskell files
-- Open the given files
-- Parse each file into an sexpression
-- Join the sexpressions
-- Compile the code into a haskell file
-- Write the haskell file to a temporary file
-- Compile it with the virtual machine to create a virtual machine executable
embedded :: String -> [ FilePath ] -> IO ExitCode 
embedded ghc_opts files = do
   fs <- C.mapM read_s files
   let sexp                = parse_file_sexps files fs
       mname               = "VirtualMachineExtension"
       VarApp vuniq vcore      = compile $ NonVarApp 0 $ from_sexp sexp
       Resolved lcore syms = compile $ Unresolved vcore [ ]
       EmbedModule euniq m ec  = compile $ EmbeddedHaskell vuniq mname lcore
       VMAST vmuniq haskell       = compile $ EmbedCore euniq m ec
       tmp                 = mname L.++ ".hs"
       delve_interface      = "edvm.dui"
   catch ( Prelude.writeFile tmp $ prettyPrint haskell )
         ( const $ error $ "Error while writing temporary file '" L.++ tmp L.++ "'" )
   ghc <- system $ "ghc --make edvm " L.++ ghc_opts
   -- This has a bit of duplication with the build system -- if this ever gets cabalized, that should sort the problem
   case ghc of
      ExitSuccess -> do -- The vm was created
         X.onException ( Z.writeFile delve_interface $ to_dui syms vmuniq )
                       ( error $ "Error while writing Delve Interface file '" L.++ delve_interface L.++ "'" )
      _           -> return ( )
   putStrLn "\nBuild complete. It's a good idea to move 'edvm.dui' (the VM interface file for the compiler ) into /etc/delve/, so the compiler can find it while compiling things. Otherwise, it will have to be in your PWD every time you want to compile!"
   return ghc 

   
-- Read a file, trigger error on failure
read_s :: FilePath -> IO String
read_s fp =
   catch (Prelude.readFile fp)
         ( const $ error $ "Error while reading '" L.++ fp L.++ "'" )

-- | Compile a list of files to DCode and write it out
dcode :: [ FilePath ]-> ( Maybe FilePath ) -> IO ( )
dcode fs mo = do
   dus <- C.mapM read_s fs
   -- should refactor the catches
   duibs <- catch ( Z.readFile "edvm.dui" ) 
                  ( const $ catch ( Z.readFile "/etc/delve/edvm.dui" ) 
                                  ( const $ error $ "Error!\nCould not find interface file 'edvm.dui' or '/etc/delve/edvm.dui'\n" L.++
                                                    "Have you built the VM and installed the interface file?" ) )
   let ( dui , init_seed )  = from_dui duibs
       sexp = parse_file_sexps fs dus
       VarApp uniq vcore = compile $ NonVarApp init_seed $ from_sexp sexp
       Resolved lcore _  = compile $ Unresolved vcore dui
       VMCode d  = compile $ LocalCore uniq lcore
       o = fromMaybe ( dcodify $ L.last fs ) mo
   catch ( Z.writeFile o $ to_dcode d )
         ( const $ error $ "Error while writing dcode output '" L.++ o L.++ "'" )

-- | Transform a source file name into an appropriate dcode file name
dcodify :: FilePath -> FilePath
dcodify source =
   let name = strip_type source
   in  name L.++ ".dcode"
   where
   strip_type = L.takeWhile $ (/=) '.'

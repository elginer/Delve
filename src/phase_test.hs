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

import VarApp
import ResolveLocal
import SExp
import Compiler ( compile ) 
import VMCompiler
import Embed
import VMASTCompiler

embedded = do
   l <- lecore
   f <- Prelude.readFile "EmbedTest.delve"
   let VarApp o vcore     = compile $ NonVarApp 0 $ from_sexp $ to_sexp f
       Resolved lcore     = compile $ Unresolved vcore
       EmbedModule w m ec = compile $ EmbeddedHaskell o "EmbedTest" lcore
       VMAST haskell      = compile $ EmbedCore w m ec
   return haskell


lecore = do
   f <- Prelude.readFile "CoreTest.delve"
   let VarApp _ vcore = compile $ NonVarApp 0 $ from_sexp $ to_sexp f
       Resolved lcore = compile $ Unresolved vcore
   return lcore

dcode = do
   f <- Prelude.readFile "CoreTest.delve"
   let VarApp uniq vcore = compile $ NonVarApp 0 $ from_sexp $ to_sexp f
       Resolved lcore    = compile $ Unresolved vcore
       VMCode d          = compile $ LocalCore uniq lcore
   return d
   

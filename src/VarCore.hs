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

module VarCore where

import Core

-- A simplified version of core where only simple expressions can be applied to functions

type VCore =
   [ VStmt ]

type VExecCore =
   [ VExecStmt ]  

data VStmt = 
   VSetHere      Symbol VExecStmt
   | VSetLocal   [ Symbol ] VExecStmt
   | VSetObject  [ Symbol ] VExecStmt
   | VStandalone VCoreExpr
   | VBegin VExecCore
   | VCoreSpecial Symbol String
   deriving Show

data VExecStmt =
   VStmt VStmt
   | VExecExpr VExecExpr
   deriving Show

data VCoreExpr =
   VApp VVar [ VVar ]
   | VCoreMatch VVar VAlternatives
   | VSimple VSimpleExpr
   deriving Show

data VVar =
   VLocalVar [ Symbol ]
   | VObjVar [ Symbol ]
   deriving Show

type VAlternatives =
   [ VCoreAlternative ]

type VCoreAlternative = 
   ( Symbol , VExecCore )

data VSimpleExpr =
   VLit Lit
   | VReserved  Reserved
   | VVar VVar 
   deriving Show

data VExecExpr =
   VFunction     [ Symbol ] VExecCore 
   | VMethod     [ Symbol ] VExecCore
   deriving Show

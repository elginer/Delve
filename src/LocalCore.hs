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

module LocalCore where

import Core

-- A simplified version of core where only simple expressions can be applied to functions and the scope used is well defined for local interactions

type LCore =
   [ LStmt ]

type LExecCore =
   [ LExecStmt ]

data LExecStmt =
   LStmt LStmt
   | LExecExpr LExecExpr
   deriving ( Show , Eq )

data LStmt = 
   LSetHere      Symbol LExecStmt
   | LSetLocal   Int [ Symbol ] LExecStmt
   | LSetObject  [ Symbol ] LExecStmt
   | LStandalone LCoreExpr
   | LBegin LExecCore
   | LCoreSpecial Symbol String
   deriving ( Show , Eq )

data LCoreExpr =
     LApp LVar [ LVar ]
   | LCoreMatch LVar LAlternatives
   | LSimple LSimpleExpr
   deriving ( Show , Eq )

data LVar =
   LLocalVar Int [ Symbol ]
   | LObjVar [ Symbol ]
   deriving ( Show , Eq )

type LAlternatives =
   [ LCoreAlternative ]

type LCoreAlternative = 
   ( Symbol , LExecCore )

data LSimpleExpr =
   LLit Lit
   | LReserved  Reserved
   | LVar LVar 
   deriving ( Show , Eq )

data LExecExpr =
   LFunction     [ Symbol ] LExecCore 
   | LMethod     [ Symbol ] LExecCore
   deriving ( Show , Eq )

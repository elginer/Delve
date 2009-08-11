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
module VarApp 
   ( module Core  
   , module VarCore
   , VarAppC ( .. )
   , Source ( .. )
   , Target ( .. )
   ) where


import Control.Monad.Stream as C
import Control.Arrow

import Data.List.Stream as L
import Data.Word

import Core
import VarCore
import Compiler
import UniqueVars
import Util

type VarAppM = UniqueVarsM Core VCore

data VarAppC =
   VarAppC

-- A compiler from plain core, into VarCore - similar to core, but with only simple expressions and variables allowed in applications and matches 
instance Compiler VarAppC where
   data Source VarAppC                = NonVarApp Word64 Core 
        deriving Show
   data Target VarAppC                = VarApp Word64 VCore
        deriving Show
   compile ( NonVarApp in_word core ) =
      let ( out_word , vcore ) = execRWS assign_variables core in_word
      in  VarApp out_word vcore

foldrM :: (Monad m) => (a -> b -> m b) -> b -> [ a ] -> m b
foldrM f z0 xs   = L.foldl f' return xs z0
  where 
  f' k x z = 
     f x z >>= k
 
-- assign var variables to a core program
assign_variables :: VarAppM ( )
assign_variables = do
   core <- ask
   vcore <- var_stmt_fold core
   tell vcore

-- fold over statements assigning a variable to each expression used in an application
var_stmt_fold :: Core -> VarAppM VCore
var_stmt_fold = 
   foldrM var_stmt [ ]

-- fold over exec-stmts assigning  a variable to each expression used in an application
var_exec_stmt_fold :: ExecCore -> VarAppM VExecCore
var_exec_stmt_fold =
   foldrM var_exec_stmt [ ]

-- create variables for the expressions used in an ExecStmt
-- return the new core
-- and the last statement generated
var_exec_stmt :: ExecStmt -> VExecCore -> VarAppM VExecCore
var_exec_stmt estmt core =
   case estmt of
      Stmt s     -> do
         stmts <- var_stmt s [ ]
         return $ L.map VStmt stmts L.++ core
      ExecExpr e -> do
         exec <- transform_exec e
         return $ VExecExpr exec : core 

-- Seperate VExecCore into its last statement and plain VCore
exec_sep :: VExecCore -> ( VExecStmt , VCore )
exec_sep =
   second ( L.map force_stmt ) . sep

-- forcing statements, very Stalinist
force_stmt :: VExecStmt -> VStmt
force_stmt exec =
   case exec of
      VStmt s -> s
      _       -> compile_error "VarApp::force_stmt\nWe have an enemy of the people!\nCould not force a statement."

-- create variables for the expressions used in the statements
var_stmt :: Stmt -> VCore -> VarAppM VCore
var_stmt ( SetHere path exp ) code   = do
   ( vexp , exps ) <- fmap exec_sep $ var_exec_stmt exp [ ]
   return $ uncons ( VSetHere path vexp ) exps L.++ code

var_stmt ( SetLocal path exp ) code  = do
   ( vexp , exps ) <- fmap exec_sep $ var_exec_stmt exp [ ]
   return $ uncons ( VSetLocal path vexp ) exps L.++ code

var_stmt ( SetObject path exp ) code = do
   ( vexp , exps ) <- fmap exec_sep $ var_exec_stmt exp [ ]
   return $ uncons ( VSetObject path vexp ) exps L.++ code

var_stmt ( Standalone ce ) code      = do
   var_cexpr ce code

var_stmt ( Begin co ) code = do
   stmts <- var_exec_stmt_fold co
   return $ VBegin stmts : code

var_stmt ( CoreSpecial n c ) code =
   return $ VCoreSpecial n c : code

-- transform an ExecExpr into a VExecExpr
transform_exec :: ExecExpr -> VarAppM VExecExpr
transform_exec exec =
   case exec of
      Function args core -> do
         stmts <- var_exec_stmt_fold core
         return $ VFunction args stmts
      Method args core   -> do
         stmts <- var_exec_stmt_fold core
         return $ VMethod args stmts

-- transform a SimpleExpr into it's VarCore equivalent
var_simple_expr sexp = 
   case sexp of
      LocalVar s  ->
         VVar $ VLocalVar s
      ObjectVar s ->
         VVar $ VObjVar s
      Lit l       ->
         VLit l
      Reserved r  ->
         VReserved r 

to_var :: ExecStmt -> Maybe VVar
to_var exec =
   case exec of 
      Stmt ( Standalone ( Simple s ) ) ->
         case s of
            LocalVar s  ->
               Just $ VLocalVar s
            ObjectVar s ->
               Just $ VObjVar s
            _           -> Nothing
      _                                -> Nothing

opt_execs :: ExecCore -> VarAppM [ ( VVar , VCore ) ]
opt_execs =
   C.mapM opt_exec

opt_exec :: ExecStmt -> VarAppM ( VVar , VCore )
opt_exec exec =
   let mvar = to_var exec
   in  maybe ( do my_execs <- var_exec_stmt exec [ ]
                  let ( lst , rest ) = exec_sep my_execs
                  my_name  <- new_name
                  return ( VLocalVar [ my_name ] , uncons ( VSetHere my_name lst ) rest )
             ) 
             ( return . flip (,) [ ] ) mvar



-- create new var variables for each argument of the CoreExpr
-- create a new application expression which is only applied to variables ( rather than ExecExprs )
-- and generate code which calls the expression
var_cexpr :: CoreExpr -> VCore -> VarAppM VCore
var_cexpr ( App exec args ) code           = do
   ( vars , assign_lists ) <- fmap L.unzip $ opt_execs $ exec : args
   let assignments = L.concat assign_lists
       call_var    = L.head vars
       arg_vars    = L.tail vars
   return $ uncons ( VStandalone $ VApp call_var arg_vars )  assignments L.++ code

-- it's slooow because of the uncons
var_cexpr ( CoreMatch exec core_alts ) code  = do
   ( var , assignment ) <- opt_exec exec
   match <- fmap ( VCoreMatch var ) $ C.mapM var_alts core_alts
   return $ uncons ( VStandalone match ) assignment L.++ code

var_cexpr ( Simple s ) code = 
   return $ VStandalone ( VSimple $ var_simple_expr s ) : code

-- apply the uniquification to alternative computations
var_alts :: CoreAlternative -> VarAppM VCoreAlternative
var_alts ca = do
   let ( s , ma ) = second var_exec_stmt_fold ca
   ma >>= return . (,) s



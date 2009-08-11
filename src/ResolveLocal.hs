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
-- Find the scope to which each local variable belongs, and generate code which captures this information.
module ResolveLocal where

import Control.Arrow
import Control.Monad.State
import Control.Monad.Stream as C

import Data.List as L
import Data.Maybe

import Core
import Symbol
import VarCore
import LocalCore
import Compiler

-- The monad the resolution is gonna run in
type ResolveM = State ResSt

-- the state of the resolution monad
data ResSt = ResSt {
      scopes :: [ [ Symbol ] ]
   ,  vcore  :: VExecCore
   }

{- Algorithm:
   Create a new table.
   For each local function or method def in the current scope, and before any 'calls'
   check if it declares a new variable,
   if it does, then add this variable to the current table, and generate code.
   If it sets a member of a local variable,
   Find the scope index of the variable, and generate the code.

   Find the scope index for all variables called after the def-block,
   generate code.

   Recurse into the bodies of each definition.
   Pop the current table
-} 

-- compiler instance for resolving this shiz
-- ResolveLocal is used as an index in the data families involved
data ResolveLocal =
   ResolveLocal

instance Compiler ResolveLocal where
   data Source ResolveLocal = Unresolved VCore [ Symbol ]
   data Target ResolveLocal = Resolved LCore [ Symbol ]
   compile ( Unresolved vcore init_scp ) =
      uncurry Resolved $ second ( L.last . scopes ) $ first ( L.map force_statement ) $ runState resolve $ ResSt [ init_scp ] $ L.map VStmt vcore

-- This should be in a typeclass
force_statement :: LExecStmt -> LStmt
force_statement exec =
   case exec of
      LStmt s -> s
      _       -> compile_error "ResolveLocal::force_statement:\nUnexpected ExecExpr"
 
-- resolve all the local interactions of VExecCore into LExecCore
resolve :: ResolveM LExecCore
resolve =
   resolve' [ ]

resolve' :: [ LExecCore ] -> ResolveM LExecCore
resolve' acc = do
   vc <- fmap vcore get 
   if L.null vc
      then return $ L.concat $ L.reverse acc 
      else do
         defs      <- pop_defs
         s_cons    <- exec_stmt_cons defs
         let def_bodies  = get_def_bodies defs
             ldef_cons   = get_def_cons defs
         lcode       <- resolve_code
         ldef_bodies <- C.mapM ( \ ( formal_args , body ) -> do
                                  in_new_scope $ do
                                     C.mapM add_var formal_args 
                                     resolve_recurse body ) def_bodies
         resolve' $ ( L.zipWith ($) s_cons ( L.zipWith ($) ldef_cons ldef_bodies ) L.++ lcode ) : acc

-- Evaluate the computation in a new scope
in_new_scope :: ResolveM a -> ResolveM a
in_new_scope f = do
   st <- get
   put $ st { scopes = [ ] : scopes st }
   a <- f
   put st
   return a

-- Recurse by evaluating the LExecCore from the given VExecCore
resolve_recurse :: VExecCore -> ResolveM LExecCore
resolve_recurse vc = 
   do st <- get
      put $ st { vcore = vc }
      lcore <- resolve
      put st
      return lcore

resolve_code :: ResolveM LExecCore
resolve_code = do
   vcore   <- pop_while is_code
   ls_cons <- call_stmt_cons vcore
   let vstmts = get_inner_stmts vcore
   lstmts  <- resolve_stmts vstmts
   return $ L.map LStmt $ L.zipWith ($) ls_cons lstmts

resolve_stmts :: [ VStmt ] -> ResolveM [ LStmt ]
resolve_stmts = C.mapM resolve_stmt

-- a bit of duplication with call_stmt_con
resolve_stmt :: VStmt -> ResolveM LStmt
resolve_stmt stmt = do
   eeasy_stmt <- case stmt of
                    VSetHere a e     -> do
                       add_var a
                       return $ Left ( LSetHere a , e )
                    VSetLocal a e    -> do
                       i <- find_path_scope a
                       return $ Left ( LSetLocal i a , e )
                    VSetObject a e   -> return $ Left $ ( LSetObject a , e )
                    VCoreSpecial n d -> return $ Right $ LCoreSpecial n d
                    VBegin estmts    -> do
                       lestmts <- in_new_scope $ resolve_recurse estmts
                       return $ Right $ LBegin lestmts
                    VStandalone ce -> fmap ( Right . LStandalone ) $ resolve_cexp ce
   either ( \ ( lcons , vestmt ) ->
             do ( lstmt : [ ] ) <- resolve_recurse [ vestmt ]
                return $ lcons lstmt
          ) return eeasy_stmt

-- resolve alternatives
resolve_alt :: VCoreAlternative -> ResolveM LCoreAlternative
resolve_alt ( sym , vcore ) = do
   lcore <- in_new_scope $ resolve_recurse vcore
   return ( sym , lcore )

-- resolve a variable
resolve_var :: VVar -> ResolveM LVar
resolve_var var =
   case var of
      VLocalVar path -> do
         i <- find_path_scope path
         return $ LLocalVar i path
      VObjVar path   ->
         return $ LObjVar path

-- resolve a VCoreExpr
resolve_cexp :: VCoreExpr -> ResolveM LCoreExpr
resolve_cexp cexp =
   case cexp of
      VApp app_var vars       -> do
         lvars <- C.mapM resolve_var $ app_var : vars
         return $ LApp ( L.head lvars ) $ L.tail lvars
      VCoreMatch m_var alts -> do
         lvar <- resolve_var m_var
         lalts <- C.mapM resolve_alt alts
         return $ LCoreMatch lvar lalts
      VSimple simple      -> do
         fmap LSimple $ resolve_simple simple   

-- resolve a simple expression
resolve_simple :: VSimpleExpr -> ResolveM LSimpleExpr
resolve_simple simple =
   case simple of
      VLit l         ->
         return $ LLit l
      VReserved Self ->
         return $ LReserved Self
      VVar var       ->
         fmap LVar $ resolve_var var

call_stmt_cons :: [ VExecStmt ] -> ResolveM [ ( LStmt -> LStmt ) ]
call_stmt_cons = C.mapM call_stmt_con

call_stmt_con :: VExecStmt -> ResolveM ( LStmt -> LStmt )
call_stmt_con ex =
   case ex of
      VStmt s     ->
         case s of
            VSetHere a _ -> do
               add_var a
               return $ LSetHere a . LStmt
            VSetLocal as _  -> do
               i <- find_path_scope as
               return $ LSetLocal i as . LStmt
            VSetObject a _  ->
               return $ LSetObject a . LStmt
            _               -> return id
      _           -> compile_error "ResolveLocal::call_stmt_con:\nThe algorithm is fucked!\nUnexpected ExecExpr." 


get_inner_stmts :: [ VExecStmt ] -> [ VStmt ]
get_inner_stmts =
   L.map get_inner_stmt

get_inner_stmt :: VExecStmt -> VStmt
get_inner_stmt exec =
   let Just s = on_inner_stmt exec id
   in  s

is_code :: VExecStmt -> Bool
is_code exec =
   fromMaybe False $ on_inner_stmt exec $ const True

on_inner_stmt :: VExecStmt -> ( VStmt -> a ) -> Maybe a
on_inner_stmt ex f =
  case ex of
      VStmt s   ->
         case s of
            VSetHere _ ( VStmt s )   -> Just $ f s
            VSetLocal _ ( VStmt s )  -> Just $ f s
            VSetObject _ ( VStmt s ) -> Just $ f s
            VSetHere _ _             -> Nothing
            VSetLocal _ _            -> Nothing
            VSetObject _ _           -> Nothing
            s                 -> Just $ f s
      VExecExpr e -> Nothing

-- Transform VExec statements into constructors for LExecStmts
exec_stmt_cons :: [ VExecStmt ] -> ResolveM [ ( LExecExpr -> LExecStmt ) ]
exec_stmt_cons =
   C.mapM exec_stmt_con

-- Transform a VExec statement into the constructor for an LExecStmt
exec_stmt_con :: VExecStmt -> ResolveM ( LExecExpr -> LExecStmt )
exec_stmt_con ex =
   case ex of
      VStmt s     ->
         case s of
            VSetHere a _    -> do
               add_var a
               return $ LStmt . LSetHere a . LExecExpr
            VSetLocal as _  -> do
               i <- find_path_scope as
               return $ LStmt . LSetLocal i as . LExecExpr
            VSetObject a _  ->
               return $ LStmt . LSetObject a . LExecExpr
            _                 -> compile_error "ResolveLocal::exec_stmt_con:\nThe algorithm is fucked!\nUnexpected Stmt."
      VExecExpr _ -> return $ LExecExpr 

-- Get the bodies of functions from the statements given
get_def_bodies :: [ VExecStmt ] -> [ ( [ Symbol ] , VExecCore ) ]
get_def_bodies = L.map get_def_body

-- Get the body of a function from the statement
get_def_body :: VExecStmt -> ( [ Symbol ] , VExecCore )
get_def_body exec =
   let Just ex = on_fun exec $ \ exec ->
                    case exec of
                       VFunction a e -> ( a , e )
                       VMethod   a e -> ( a , e )
   in  ex

get_def_cons :: [ VExecStmt ] -> [ ( LExecCore -> LExecExpr ) ]
get_def_cons = L.map get_def_con

get_def_con :: VExecStmt -> ( LExecCore -> LExecExpr )
get_def_con def =
   let Just c = on_fun def $ \ exec ->
                   case exec of 
                     VFunction args _ -> LFunction args
                     VMethod args _   -> LMethod   args
   in  c
   
pop_defs :: ResolveM VExecCore
pop_defs =
   pop_while is_fun

is_fun :: VExecStmt -> Bool 
is_fun ex =
   fromMaybe False $ on_fun ex $ const True

on_fun :: VExecStmt -> ( VExecExpr -> a ) -> Maybe a
on_fun ex f =
  case ex of
      VStmt s   ->
         case s of
            VSetHere _ exec   -> on_fun exec f
            VSetLocal _ exec  -> on_fun exec f
            VSetObject _ exec -> on_fun exec f
            _                 -> Nothing
      VExecExpr e -> Just $ f e

pop_while :: ( VExecStmt -> Bool ) -> ResolveM VExecCore
pop_while =
   pop_while' [ ]

pop_while' :: [ VExecStmt ] -> ( VExecStmt -> Bool ) -> ResolveM VExecCore
pop_while' acc p = do
   vc <- fmap vcore get
   if L.null vc
      then end 
      else
         let exec = L.head vc
         in  if p exec
                then do
                   modify $ \ st -> st { vcore = L.tail vc }
                   pop_while' ( exec : acc ) p
                else
                   end
   where end = return $ L.reverse acc


-- find the relative difference between the current scope and the specified var's scope
find_var_scope :: Symbol -> ResolveM Int
find_var_scope sym = do
   scps <- fmap scopes get
   maybe ( compile_error $ "ResolveLocal::find_var_scope:\nElement " L.++ show sym L.++ "not present in local scope" )
         return 
         $ findIndex ( L.elem sym ) scps

-- like find_var_scope but for a path of symbols
find_path_scope :: [ Symbol ] -> ResolveM Int
find_path_scope = find_var_scope . L.head

-- add a new variable in the current scope
add_var :: Symbol -> ResolveM ( )
add_var sym = do
   scps <- fmap scopes get
   let scp = L.head scps
   modify $ \ res -> res { scopes = ( sym : scp ) : L.tail scps }

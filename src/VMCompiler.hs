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

{-# OPTIONS -XTypeFamilies 
            -XEmptyDataDecls 
            -XMultiParamTypeClasses 
            -XRankNTypes
            -XTypeSynonymInstances 
            -XFlexibleInstances 
            -XFlexibleContexts 
            -XUndecidableInstances #-}
-- A compiler from LocalCore to VMCode
-- A lot of the operations are very very inefficient
-- It would be better to pass around the continuation, than to add commands to the end of the program
module VMCompiler where

import Data.List.Stream as L
import Data.Word
import Data.Tuple.Select
import Data.Maybe

import Control.Monad.Stream as C
import Control.Applicative as A

import Backend
import Core
import Compiler
import LocalCore
import DMachineState as D
import UniqueVars hiding ( local )
import Util

-- TODO: Raise compiler errors when fed invalid code
-- TODO: Quantify extactly 'invalid code'
-- Any instruction with an empty path is invalid.
-- TODO: Run within VMVars -- need to assign and then immediately call 'Begin' blocks
--       This is best done at this stage, rather than earlier, as this is rather a large change of semantics, changing the meaning of the core languages ( which are similar )

-- The compiler instance for compiling VM code
data VMCompiler

instance Compiler VMCompiler where
   data Source VMCompiler = LocalCore Word64 LCore
   newtype Target VMCompiler = VMCode Code
   compile ( LocalCore uniq lcore ) =
      VMCode $ sel1 $ runRWS ( vm_core lcore ) ( ) uniq 

type VMVars = SimpleVars

class Optimization o Expr => VMCode a o where
   to_vm :: o -> a -> VMVars Code

-- instances for optimized LExecStmts
instance VMCode LStmt o => VMCode LExecStmt o where
   to_vm o lexec = 
      case lexec of
         LStmt stmt      -> to_vm o stmt
         LExecExpr lexec -> do
            execs <- vm_exec lexec 
            return $ execs L.++ oppop o

-- a new type representing a simple block of code
newtype SimpleBlock = SimpleBlock LExecCore

-- compile a simple block of code
instance VMCode LExecStmt o => VMCode SimpleBlock o where
   to_vm o ( SimpleBlock core ) = do
      stmts <- tail_analyze_block o core
      return $ PushLocal : stmts

-- popping, taking into account the optimization
instance Optimization None Expr where
   oppop _ = A.empty

instance Optimization Local Expr where
   oppop _ = A.pure PopLocal

instance Optimization Frame Expr where
   oppop _ = A.pure PopFrame

instance VMCode s o => Backend o s VMVars Expr where
   backend = to_vm

-- instances for optimized Stmts
-- These operations are highly inefficient
instance VMCode LStmt None where
   to_vm o stmt =
      let pops = [ ] in
      -- compile a LStmt to Code
      case stmt of
         LStandalone cexp      ->
            to_vm none cexp
         LSetHere sym eexp     -> do
            vm_code <- to_vm none eexp
            return $ vm_code L.++ ( AssignLocal 0 [ ] sym : pops )
         LSetLocal i path eexp -> do
            vm_code <- to_vm none eexp 
            return $ vm_code L.++ ( AssignLocal i ( drop_lst path ) ( L.last path ) : pops )
         LSetObject path eexp  -> do
            vm_code <- to_vm none eexp
            return $ vm_code L.++ ( AssignObject ( drop_lst path ) ( L.last path ) : pops )         
         LBegin lcore          ->
            to_vm local $ SimpleBlock lcore
         LCoreSpecial _ _      ->
            error "Attempted to compile special data into bytecode!\nSpecial data sections are reserved for embedding Haskell into Delve."

instance VMCode LStmt Local where
   to_vm o stmt =
      let pops = oppop o in
      -- compile a LStmt to Code
      case stmt of
         LStandalone cexp      ->
            to_vm local cexp
         LSetHere sym eexp     -> do
            vm_code <- to_vm none eexp
            return $ vm_code L.++ ( AssignLocal 0 [ ] sym : pops )
         LSetLocal i path eexp -> do
            vm_code <- to_vm none eexp 
            return $ vm_code L.++ ( AssignLocal i ( drop_lst path ) ( L.last path ) : pops )
         LSetObject path eexp  -> do
            vm_code <- to_vm none eexp
            return $ vm_code L.++ ( AssignObject ( drop_lst path ) ( L.last path ) : pops )
         LBegin lcore          -> do
            exps <- to_vm local $ SimpleBlock lcore
            return $ exps L.++ pops

instance VMCode LStmt Frame where
   to_vm o stmt =
      let pops = oppop o in
      -- compile a LStmt to Code
      case stmt of
         LStandalone cexp      ->
            to_vm frame cexp
         LSetHere sym eexp     -> do
            vm_code <- to_vm none eexp
            return $ vm_code L.++ ( AssignLocal 0 [ ] sym : pops )
         LSetLocal i path eexp -> do
            vm_code <- to_vm none eexp 
            return $ vm_code L.++ ( AssignLocal i ( drop_lst path ) ( L.last path ) : pops )
         LSetObject path eexp  -> do
            vm_code <- to_vm none eexp
            return $ vm_code L.++ ( AssignObject ( drop_lst path ) ( L.last path ) : pops )
         LBegin lcore          ->
            to_vm frame $ SimpleBlock lcore  

embed_core :: VMCode LExecStmt o => ( LVar -> Code ) -> o -> LCoreExpr -> VMVars Code
embed_core c o cexp =
      let pops = oppop o in
      case cexp of
         LApp e args  ->
            return $ push_args args L.++ c e
         LCoreMatch lvar alts  ->
            case lvar of
               LLocalVar i p ->
                  fmap ( return . MatchLocal i p ) $ vm_alts o alts
               LObjVar p     ->
                  fmap ( return . MatchObj p ) $ vm_alts o alts
         LSimple s             ->
            return $ vm_simple s : pops

instance VMCode LCoreExpr None where
   -- compile a LCoreExpr to Code
   to_vm o c =
      embed_core no_opt_call o c

instance VMCode LCoreExpr Local where
   -- compile a LCoreExpr to Code
   to_vm o c =
      embed_core local_tail_call o c

instance VMCode LCoreExpr Frame where
   -- compile a LCoreExpr to Code
   to_vm o c =
      embed_core frame_tail_call o c

-- compile LAlternatives into Alternatives
vm_alts :: VMCode LExecStmt o=> o -> LAlternatives -> VMVars Alternatives
vm_alts o =
   C.mapM $ vm_alt o

-- compile LCoreAlternative into Alternative
vm_alt :: VMCode LExecStmt o => o -> LCoreAlternative -> VMVars D.Alternative
vm_alt o ( sym , lcore ) = do
   dcore <- to_vm o $ SimpleBlock lcore
   return ( sym , dcore )

-- compile LCore to Code
vm_core :: LCore -> VMVars Code
vm_core = fmap L.concat . C.mapM ( to_vm none )

-- write the arguments to the argument stack
push_args :: [ LVar ] -> Code
push_args as = L.zipWith push_arg as [ 0 .. ]

push_arg :: LVar -> Word8 -> Expr 
push_arg v n =
   case v of
      LLocalVar i p ->
         PushLocalArg i p n
      LObjVar p     ->
         PushObjArg p n

-- There is duplication between a couple of backends here
-- But I have yet to seperate the generation of the call code from these functions ( no_opt_call, local_tail_call , frame_tail_call )
-- Is it really needed?
no_opt_call v =
   on_var v call_local call_object

local_tail_call v =
   on_var v local_tail_call_local local_tail_call_obj

frame_tail_call v =
   on_var v frame_tail_call_local frame_tail_call_obj

-- call a path from the local scope
call_local :: Int -> [ Symbol ] -> Code
call_local i path =
   case path of
      ( _ :_ : _ ) ->
         [ LoadLocal i ( drop_lst path ) , CallObj $ return $ L.last path , PopObject ]
      ( p : [ ] )          ->
         [ CallLocal i path ]

-- local tail call a path from the local scope
local_tail_call_local :: Int -> [ Symbol ] -> Code
local_tail_call_local i path =
   case path of
      ( _ : _ : _ ) ->
         [ LoadLocal i ( drop_lst path ) , LocalTailCallObj True $ return $ L.last path ]
      ( p : [ ] )          ->
         [ LocalTailCallLocal False i path ]

-- local tail call a path from the current object
local_tail_call_obj :: [ Symbol ] -> Code
local_tail_call_obj path =
   case path of
      ( _ : _ : _ ) ->
         [ LoadObj ( drop_lst path ) , LocalTailCallObj True $ return $ L.last path ]
      ( p : [ ] )          ->
         [ LocalTailCallObj False path ]

-- frame tail call a path from the local scope
frame_tail_call_local :: Int -> [ Symbol ] -> Code
frame_tail_call_local i path =
   case path of
      ( _ : _ : _ ) ->
         [ LoadLocal i ( drop_lst path ) , FrameTailCallObj $ return $ L.last path ]
      ( p : [ ] )          ->
         [ FrameTailCallLocal i path ]

-- frame tail call a path from the current object
frame_tail_call_obj :: [ Symbol ] -> Code
frame_tail_call_obj path =
   case path of
      ( _ : _ : _ ) ->
         [ LoadObj ( drop_lst path ) , FrameTailCallObj $ return $ L.last path ]
      ( p : [ ] )          ->
         [ FrameTailCallObj path ]

-- call from an object
call_object :: [ Symbol ] -> Code
call_object =
   return . CallObj

-- compile a LSimpleExpr to an Expr
vm_simple :: LSimpleExpr -> Expr
vm_simple ls =
   case ls of
      LReserved r ->
         vm_reserved r
      LLit l      ->
         vm_lit l
      LVar v      ->
         vm_var v

-- compile a lit into an Expr
vm_lit :: Lit -> Expr
vm_lit l =
   case l of
      CoreSymbol s ->
         NewSymbol s
      CorePrim p   ->
         NewPrim p

-- compile reserved into an Expr
vm_reserved :: Reserved -> Expr
vm_reserved Self =
   RefSelf

-- compile a var into an Expr
vm_var :: LVar -> Expr
vm_var v =
   case v of
      LLocalVar i path ->
         RefLocal i path
      LObjVar path     ->
         RefObj path

-- create a block for a bit of callable code ( Function or Method )
callable_block :: LExecCore -> [ Symbol ] -> VMVars Expr
callable_block lcore args =
   fmap ( NewBlock . (:) PushLocal . (L.++) ( write_args args ) ) $ tail_analyze_block frame lcore

-- compile an LExecExpr to Code
vm_exec :: LExecExpr -> VMVars Code
vm_exec exec =
   case exec of
      LFunction args lcore ->
         if not $ L.null lcore
            then do 
               n <- new_name
               cb <- callable_block lcore args 
               return $ cb : remember_whole_scope n
            else return [ NewBlock [ ] ]
      LMethod args lcore ->
         if not $ L.null lcore
            then do 
               n <- new_name
               cb <- callable_block lcore args 
               return $ cb : remember_local_scope n
            else return [ NewBlock [ ] ]


-- Write the arguments from the argument stack into the current local scope
write_args :: [ Symbol ] -> Code
write_args args = L.zipWith WriteArg [ 0 .. ] args 

-- Assign a bit of code to the variable n, and bind it to the local scope and the current object
remember_whole_scope :: Symbol -> Code
remember_whole_scope n =
   [ AssignLocal 0 [ ] n , RememberLocalLocal 0 [ n ] , RememberObjLocal 0 [ n ] ]
    
-- Assign a bit of code to the variable n, and bind it to only the local scope
remember_local_scope :: Symbol -> Code
remember_local_scope n =
   [ AssignLocal 0 [ ] n , RememberLocalLocal 0 [ n ] ]

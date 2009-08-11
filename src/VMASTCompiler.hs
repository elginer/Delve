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
            -XTypeSynonymInstances 
            -XFlexibleInstances 
            -XFlexibleContexts 
            -XUndecidableInstances #-}
-- A compiler from EmbeddedCore to the Haskell AST of Delve bytecode.
-- So Haskell can be embedded within Delve programs.
-- It is based on the VM Compiler -- some effort should go into uniting them into one general backend
---
-- However, a lot of the operations are very very inefficient
-- It would be better to pass around the continuation, than to add commands to the end of the program
module VMASTCompiler
   ( Source ( EmbedCore )
   , Target ( VMAST ) )
   where

import Data.List.Stream as L
import Data.Word
import Data.Tuple.Select
import Data.ByteString.Char8 as B hiding ( uncons )

import Control.Monad.Stream as C
import Control.Applicative as A

import Backend
import HsAST
import Core hiding ( Lit , App )
import qualified Core ( Lit )
import Compiler
import Embed
import LocalCore
import UniqueVars hiding ( local )
import Util

-- TODO: Raise compiler errors when fed invalid code
-- TODO: Quantify extactly 'invalid code'
-- Any instruction with an empty path is invalid.

-- The compiler instance for compiling EmbedCore to the Haskell represenetation of Delve ByteCode.
data VMASTCompiler

instance Compiler VMASTCompiler where
   data Source VMASTCompiler    = EmbedCore Word64 Module EmbedCore
   data Target VMASTCompiler = VMAST Word64 Module
   compile ( EmbedCore uniq mod lcore ) =
      uncurry VMAST $ sel1 $ runRWS ( vm_ast mod lcore ) ( ) uniq 

instance Optimization None Exp where
   oppop _ = A.empty

instance Optimization Local Exp where
   oppop _ = pure $ ast_var "PopLocal"

instance Optimization Frame Exp where
   oppop _ = pure $ ast_var "PopFrame"

instance VMAst a o => Backend o a ASTVars Exp where
   backend = to_ast

-- This compiler requires unique variables
type ASTVars = SimpleVars 

class Optimization o Exp => VMAst a o where
   to_ast :: o -> a -> ASTVars [ Exp ]

-- instances for optimized EmbedExecStmts
instance VMAst EmbedStmt o => VMAst EmbedExecStmt o where
   to_ast o lexec = 
      case lexec of
         EmbedStmt stmt      -> to_ast o stmt
         EmbedExecExpr lexec ->
            let pops = oppop o
            in  do
               execs <- vm_exec lexec
               return $ execs L.++ pops

-- a new type representing a simple block of code
newtype SimpleBlock = SimpleBlock EmbedExecCore

-- compile a simple block of code
instance VMAst EmbedExecStmt o => VMAst SimpleBlock o where
   to_ast o ( SimpleBlock core ) = do
      stmts <- tail_analyze_block o core
      return $ ast_var "PushLocal" : stmts

-- ast for a variable or constructor
ast_var :: String -> Exp
ast_var n = Con $ UnQual $ name n

-- ast for appllication
appl :: [ Exp ] -> Exp
appl exps = 
   if L.null exps
      then L.head exps
      else L.foldr App ( L.last exps ) $ drop_lst exps

-- instances for optimized Stmts
-- These operations are highly inefficient
instance VMAst EmbedStmt Frame where
   to_ast o stmt =
      let pops = oppop o in
      -- compile a EmbedStmt to Exp
      case stmt of
         EmbedStandalone cexp      ->
            to_ast frame cexp
         EmbedSetHere sym eexp     -> do
            vm_code <- to_ast none eexp
            return $ vm_code L.++ ( appl [ ast_var "AssignLocal" , Lit $ Int 0 , List [ ] , ast_symbol sym ] : pops )
         EmbedSetLocal i path eexp -> do
            vm_code <- to_ast none eexp 
            return $ vm_code L.++ ( appl [ ast_var "AssignLocal" , Lit $ Int $ fromIntegral i ,  ast_symbols $ drop_lst path , ast_symbol $ L.last path ] : pops )
         EmbedSetObject path eexp  -> do
            vm_code <- to_ast none eexp
            return $ vm_code L.++ ( appl [ ast_var "AssignObject" , ast_symbols $ drop_lst path , ast_symbol $ L.last path ] : pops )
         EmbedBegin lcore          ->
            to_ast frame $ SimpleBlock lcore
         EmbedHSFunc s             ->
            return $ app ( ast_var "NewBlock" ) ( List [ appl [ ast_var "MachineInstruction" , ast_var $ B.unpack s ] ] ) : pops

-- We must pop after the local block - it will not pop the frame itself, so we must pop after it
instance VMAst EmbedStmt Local where
   to_ast o stmt =
      let pops = oppop o in
      -- compile a EmbedStmt to Exp
      case stmt of
         EmbedStandalone cexp      -> do
            to_ast local cexp
         EmbedSetHere sym eexp     -> do
            vm_code <- to_ast none eexp
            return $ vm_code L.++ ( appl [ ast_var "AssignLocal" , Lit $ Int 0 , List [ ] , ast_symbol sym ] : pops )
         EmbedSetLocal i path eexp -> do
            vm_code <- to_ast none eexp 
            return $ vm_code L.++ ( appl [ ast_var "AssignLocal" , Lit $ Int $ fromIntegral i ,  ast_symbols $ drop_lst path , ast_symbol $ L.last path ] : pops )
         EmbedSetObject path eexp  -> do
            vm_code <- to_ast none eexp
            return $ vm_code L.++ ( appl [ ast_var "AssignObject" , ast_symbols $ drop_lst path , ast_symbol $ L.last path ] : pops )
         EmbedBegin lcore          -> do
            exps <- to_ast local ( SimpleBlock lcore  ) 
            return $ exps L.++ pops
         EmbedHSFunc s             ->
            return $ app ( ast_var "NewBlock" ) ( List [ appl [ ast_var "MachineInstruction" , ast_var $ B.unpack s ] ] ) : pops

instance VMAst EmbedStmt None where
   to_ast o stmt =
      let pops = oppop o in
      -- compile a EmbedStmt to Exp
      case stmt of
         EmbedStandalone cexp      -> do
            to_ast none cexp
         EmbedSetHere sym eexp     -> do
            vm_code <- to_ast none eexp
            return $ vm_code L.++ ( appl [ ast_var "AssignLocal" , Lit $ Int 0 , List [ ] , ast_symbol sym ] : pops )
         EmbedSetLocal i path eexp -> do
            vm_code <- to_ast none eexp 
            return $ vm_code L.++ ( appl [ ast_var "AssignLocal" , Lit $ Int $ fromIntegral i ,  ast_symbols $ drop_lst path , ast_symbol $ L.last path ] : pops )
         EmbedSetObject path eexp  -> do
            vm_code <- to_ast none eexp
            return $ vm_code L.++ ( appl [ ast_var "AssignObject" , ast_symbols $ drop_lst path , ast_symbol $ L.last path ] : pops )
         EmbedBegin lcore          ->
            to_ast local $ SimpleBlock lcore
         EmbedHSFunc s             ->
            return $ app ( ast_var "NewBlock" ) ( List [ appl [ ast_var "MachineInstruction" , ast_var $ B.unpack s ] ] ) : pops



ast_symbols :: [ Symbol ] -> Exp
ast_symbols =
   List . L.map ast_symbol   

ast_symbol :: Symbol -> Exp
ast_symbol s = Paren $ appl [ ast_var "pack" , Lit $ String $ B.unpack s ] 


instance VMAst EmbedCoreExpr None where
   -- compile a EmbedCoreExpr to [ Exp ]
   to_ast o cexp =
      embed_core no_opt_call o cexp

embed_core :: VMAst EmbedExecStmt o => ( LVar -> [ Exp ] ) -> o -> EmbedCoreExpr -> ASTVars [ Exp ]
embed_core c o cexp =
      let pops = oppop o in
      case cexp of
         EmbedApp e args  ->
            return $ push_args args L.++ c e
         EmbedCoreMatch lvar alts  ->
            case lvar of
               LLocalVar i p ->
                  fmap ( return . App ( appl [ ast_var "MatchLocal" , Lit $ Int $ fromIntegral i , ast_symbols p ] ) . List ) $ vm_alts o alts
               LObjVar p     ->
                  fmap ( return . App ( appl [ ast_var "MatchObj" , ast_symbols p ] ) . List ) $ vm_alts o alts
         EmbedSimple s             ->
            return $ vm_simple s : pops

instance VMAst EmbedCoreExpr Local where
   -- compile a EmbedCoreExpr to [ Exp ]
   to_ast o cexp =
      embed_core local_tail_call o cexp

instance VMAst EmbedCoreExpr Frame where
   -- compile a EmbedCoreExpr to [ Exp ]
   to_ast o cexp =
      embed_core frame_tail_call o cexp

instance VMAst a o => VMAst [ a ] o where
   to_ast o = do
      fmap L.concat . C.mapM ( to_ast o )

-- compile EmbedAlternatives into Alternatives
vm_alts :: VMAst EmbedExecStmt o => o -> EmbedAlternatives -> ASTVars [ Exp ]
vm_alts o =
   C.mapM $ vm_alt o

-- compile EmbedCoreAlternative into Alternative
vm_alt :: VMAst EmbedExecStmt o => o -> EmbedCoreAlternative -> ASTVars Exp
vm_alt o ( sym , lcore ) = do
   dcore <- to_ast o $ SimpleBlock lcore
   return $ Tuple [ ast_symbol sym , List dcore ]

-- compile EmbedCore to Module
vm_ast :: Module -> EmbedCore -> ASTVars ( Word64 , Module )
vm_ast ( Module s n ops w e im ds ) ecore = do
   exps <- to_ast none ecore
   let d = FunBind [ Match a_src_loc ( name "bytecode" ) [ ] Nothing ( UnGuardedRhs $ List exps ) $ BDecls [ ] ]
   seed <- get
   return $ ( seed , Module s n ops w e ( d_imports L.++ im ) $ d : ds )

-- imports needed to compile dcode
d_imports :: [ ImportDecl ]
d_imports =
   L.map simple_import [ "Data.ByteString.Char8" , "DMachineState" ] 

-- a simple import
simple_import :: String -> ImportDecl
simple_import str =
   ImportDecl a_src_loc ( ModuleName str ) False False Nothing Nothing Nothing

-- write the arguments to the argument stack
push_args :: [ LVar ] -> [ Exp ]
push_args as = L.zipWith push_arg as [ 0 .. ]

push_arg :: LVar -> Word8 -> Exp 
push_arg v n =
   case v of
      LLocalVar i p ->
         appl [ ast_var "PushLocalArg" , Lit $ Int $ fromIntegral i , ast_symbols p , Lit $ Int $ fromIntegral n ]
      LObjVar p     ->
         appl [ ast_var "PushObjArg" , ast_symbols p , Lit $ Int$ fromIntegral n ]

no_opt_call v =
   on_var v call_local call_object

local_tail_call v =
   on_var v local_tail_call_local local_tail_call_obj

frame_tail_call v =
   on_var v frame_tail_call_local frame_tail_call_obj

-- call a path from the current object
call_object :: [ Symbol ] -> [ Exp ]
call_object path =
   [ call "CallObj" path ]

-- call a path from the local scope
call_local :: Int -> [ Symbol ] -> [ Exp ]
call_local i path =
   case path of
      ( _ : _ : _ ) ->
         [ appl [ ast_var "LoadLocal" , Lit $ Int $ fromIntegral i , ast_symbols $ drop_lst path ] , call "CallObj" [ L.last path ] , ast_var "PopObject" ]
      ( p : [ ] )          ->
         [ appl [ ast_var "CallLocal" , Lit $ Int $ fromIntegral i , ast_symbols path ] ]

-- local tail call a path from the local scope
local_tail_call_local :: Int -> [ Symbol ] -> [ Exp ]
local_tail_call_local i path =
   case path of
      ( _ : _ : _ ) ->
         [ appl [ ast_var "LoadLocal" , Lit $ Int $ fromIntegral i , ast_symbols $ drop_lst path ] , appl [ ast_var "LocalTailCallObj" , ast_var "True" , ast_symbols [ L.last path ] ] ]
      ( p : [ ] )          ->
         [ appl [ ast_var "LocalTailCallLocal" , ast_var "False" , Lit $ Int $ fromIntegral i , ast_symbols path ] ]

-- local tail call a path from the current object
local_tail_call_obj :: [ Symbol ] -> [ Exp ]
local_tail_call_obj path =
   case path of
      ( _ : _ : _ ) ->
         [ appl [ ast_var "LoadObj" , ast_symbols $ drop_lst path ] , appl [ ast_var "LocalTailCallObj" , ast_var "True" , ast_symbols [ L.last path ] ] ]
      ( p : [ ] )          ->
         [ appl [ ast_var "LocalTailCallObj" , ast_var "False" , ast_symbols path ] ]

-- frame tail call a path from the local scope
frame_tail_call_local :: Int -> [ Symbol ] -> [ Exp ]
frame_tail_call_local i path =
   case path of
      ( _ : _ : _ ) ->
         [ appl [ ast_var "LoadLocal" , Lit $ Int $ fromIntegral i , ast_symbols $ drop_lst path ] , call "FrameTailCallObj" [ L.last path ] ]
      ( p : [ ] )          ->
         [ appl [ ast_var "FrameTailCallLocal" , Lit $ Int $ fromIntegral i , ast_symbols path ] ]

-- frame tail call a path from the current object
frame_tail_call_obj :: [ Symbol ] -> [ Exp ]
frame_tail_call_obj path =
   case path of
      ( _ : _ : _ ) ->
         [ appl [ ast_var "LoadObj" , ast_symbols $ drop_lst path ] , call "FrameTailCallObj" [ L.last path ] ]
      ( p : [ ] )          ->
         [ call "FrameTailCallObj" path ]

-- tail-call a path by using the given constructor
call :: String -> [ Symbol ] -> Exp
call c p =
   appl [ ast_var c ,  ast_symbols p ] 

-- compile a LSimpleExpr to an Exp
vm_simple :: LSimpleExpr -> Exp
vm_simple ls =
   case ls of
      LReserved r ->
         vm_reserved r
      LLit l      ->
         vm_lit l
      LVar v      ->
         vm_var v

-- compile a lit into an Exp
vm_lit :: Core.Lit -> Exp
vm_lit l =
   case l of
      CoreSymbol s ->
         appl [ ast_var "NewSymbol" , ast_symbol s ]
      CorePrim p   ->
         appl [ ast_var "NewPrim" , prim_ast p ]

-- compile a prim to HsAst
prim_ast :: Prim -> Exp
prim_ast p =
   case p of
      I i -> Paren $ appl [ ast_var "I" , Lit $ Int $ fromIntegral i ]

-- compile reserved into an Exp
vm_reserved :: Reserved -> Exp
vm_reserved Self =
   ast_var "RefSelf"

-- compile a var into an Exp
vm_var :: LVar -> Exp
vm_var v =
   case v of
      LLocalVar i path ->
         appl [ ast_var "RefLocal" , Lit $ Int $ fromIntegral i , ast_symbols path ]
      LObjVar path     ->
         appl [ ast_var "RefObj" , ast_symbols path ]

-- create a block for a bit of callable code ( Function or Method )
callable_block :: EmbedExecCore -> [ Symbol ] -> ASTVars Exp
callable_block lcore args =
   fmap ( appl . (:) ( ast_var "NewBlock" ) . new_list . List . (:) ( ast_var "PushLocal" ) . (L.++) ( write_args args ) ) $ tail_analyze_block frame lcore

-- compile an EmbedExecExpr to [ Exp ] 
vm_exec :: EmbedExecExpr -> ASTVars [ Exp ]
vm_exec exec =
   case exec of
      EmbedFunction args lcore ->
         if not $ L.null lcore
            then do 
               n <- new_name
               cb <- callable_block lcore args 
               return $ cb : remember_whole_scope n
            else return [ appl [ ast_var "NewBlock" , List [ ] ] ]
      EmbedMethod args lcore ->
         if not $ L.null lcore
            then do 
               n <- new_name
               cb <- callable_block lcore args 
               return $ cb : remember_local_scope n
            else return [ appl [ ast_var "NewBlock" , List [ ] ] ]


-- Write the arguments from the argument stack into the current local scope
write_args :: [ Symbol ] -> [ Exp ]
write_args args = L.zipWith ( \ i a -> appl [ ast_var "WriteArg" , Lit $ Int $ fromIntegral i , ast_symbol a ]  ) [ 0 .. ] args 

-- Assign a bit of code to the variable n, and bind it to the local scope and the current object
remember_whole_scope :: Symbol -> [ Exp ]
remember_whole_scope n =
   [ appl [ ast_var "AssignLocal" , Lit $ Int 0 , List [ ], ast_symbol n ] 
   , appl [ ast_var "RememberLocalLocal" , Lit $  Int 0 , List [ ast_symbol n ] ] 
   , appl [ ast_var "RememberObjLocal" , Lit $ Int 0 , List [ ast_symbol n ] ] ]
    
-- Assign a bit of code to the variable n, and bind it to only the local scope
remember_local_scope :: Symbol -> [ Exp ]
remember_local_scope n =
   [ appl [ ast_var "AssignLocal" , Lit $ Int 0 , List [ ] , ast_symbol n ] 
   , appl [ ast_var "RememberLocalLocal" , Lit $ Int 0 , List [ ast_symbol n ] ] ]

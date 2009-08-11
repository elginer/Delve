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

{-# OPTIONS -XTypeFamilies -XEmptyDataDecls -XTypeSynonymInstances -XMultiParamTypeClasses #-}
-- Transform local core into the AST for DMachineCode, replace Haskell expressions with the appropriate names of bindings
-- create haskell module with bindings and imports
module Embed 
   ( Target ( EmbedModule )
   , Source ( EmbeddedHaskell )
   , EmbedCore
   , EmbedStmt ( .. )
   , EmbedExecStmt ( .. )
   , EmbedExecExpr ( .. )
   , EmbedExecCore
   , EmbedCoreExpr ( .. )
   , EmbedAlternatives
   , EmbedCoreAlternative
   )
   where

import Data.Word
import Data.Tuple.Select
import Data.List.Stream as L
import Data.ByteString.Char8 as B
import Data.Maybe

import Control.Monad.Stream as C
import Control.Arrow

import HsAST
import UniqueVars
import LocalCore
import Core
import Compiler

type EmbedM = UniqueVarsM ( ) ( )

data Embed

instance Compiler Embed where
   data Source Embed = EmbeddedHaskell Word64 String LCore
   data Target Embed = EmbedModule Word64 Module EmbedCore
   compile ( EmbeddedHaskell w name lcore ) =
      sel1 $ runRWS ( create_module name lcore ) ( ) w

data EmbedHS =
   EmbedImport ImportDecl
   | EmbedDecl Decl

create_module :: String -> LCore -> EmbedM ( Target Embed )
create_module mname lcore = do
   ( embedded , memcore ) <- embed lcore
   let embedded_hsexps = embed_hss mname embedded
   w <- get
   return $ EmbedModule w embedded_hsexps $ not_special memcore

-- create a module from EmbedHs
embed_hss :: String -> [ EmbedHS ] -> Module
embed_hss mname = L.foldr embed_hs $ Module a_src_loc ( ModuleName mname ) [ ] Nothing Nothing [ ] [ ]

-- embed an EmbedHs into a module
embed_hs :: EmbedHS -> Module -> Module
embed_hs ehs ( Module srcl mname op w ed imds ds ) =
   case ehs of
      EmbedImport imd -> Module srcl mname op w ed ( imd : imds ) ds
      EmbedDecl d     -> Module srcl mname op w ed imds ( d : ds )


-- a class for data types which may hold embedded code, translating into an arbitrary data type as it goes
class Embedded e f where
   embed :: e -> EmbedM ( [ EmbedHS ] , Maybe f )

-- instances which take embedded code from delve while translating to embedded core
-- check the instance for LStmt - it is the most interesting
instance Embedded LExecStmt EmbedExecStmt where
   embed est =
      case est of
         LStmt stmt      -> do
            ( embedded , memstmt ) <- embed stmt
            return ( embedded , memstmt >>= Just . EmbedStmt )
         LExecExpr lexec -> do
            ( embedded , memstmt ) <- embed lexec
            return ( embedded , memstmt >>= Just . EmbedExecExpr )
        

instance Embedded LExecExpr EmbedExecExpr where
   embed eexp =
      case eexp of
         LFunction args lcore -> do
            ( embedded , memcore ) <- embed lcore
            return ( embedded , Just $ EmbedFunction args $ not_special memcore )
         LMethod args lcore   -> do
            ( embedded , memcore ) <- embed lcore
            return ( embedded , Just $ EmbedMethod args $ not_special memcore )
                 
-- the statement is where the embedded data really is
-- the rest is boilerplate
-- here we check if we have embedded hsimports
-- or embedded hsexpressions
instance Embedded LStmt EmbedStmt where
   embed stmt =
      case stmt of
         LCoreSpecial bs str ->
            if bs == B.pack "hsexp"
               then let pr = parseExp str
                    in case pr of
                       ParseOk exp     -> do
                          n <- new_name
                          return ( [ EmbedDecl $ FunBind [ Match a_src_loc ( exp_name n ) [ ] Nothing ( rhs_exp exp ) no_binds ] ] , Just $ EmbedHSFunc n )
                       _               -> exp_error $ show pr
               else 
                  if bs == B.pack "hsimports"
                     then
                        let imps = parseImports str
                        in  return ( L.map EmbedImport imps , Nothing )
                     else return ( [ ] , Nothing )
         LSetHere sym lexec    -> do
            ( hs , eexec ) <- embed lexec
            return ( hs , Just $ EmbedSetHere sym $ not_special eexec )
         LSetLocal i path lexec -> do
            ( hs , eexec ) <- embed lexec
            return ( hs , Just $ EmbedSetLocal i path $ not_special eexec )
         LSetObject path lexec -> do
            ( hs , eexec ) <- embed lexec
            return ( hs , Just $ EmbedSetObject path $ not_special eexec )
         LStandalone cexp      ->
            fmap ( second $ Just . EmbedStandalone . not_special ) $ embed cexp
         LBegin lexeccore      ->
            fmap ( second $ Just . EmbedBegin . not_special ) $ embed lexeccore

-- if the embedded declaration was an import or another special piece of data, raise an error because the user is attempting to assign it to use it in a computation
not_special :: Maybe a -> a
not_special = fromMaybe ( error "A 'hsimport' or other special piece of data was used in a computation." )

exp_error :: String -> a
exp_error s =
   error $ "Parse error in embedded Haskell expression\n.Error was:\n" L.++ s

import_error :: String -> a
import_error s =
   error $ "Parse error in embedded Haskell import\n.Error was:\n" L.++ s

parseImports :: String -> [ ImportDecl ]
parseImports s =
   case parseModule s of
      ParseOk ( Module _ _ _ _ _ is _ ) -> is
      pr                                -> import_error $ show pr


rhs_exp :: Exp -> Rhs
rhs_exp = UnGuardedRhs
 
no_binds :: Binds
no_binds = BDecls [ ]
         
instance Embedded a b => Embedded [ a ] [ b ] where
   embed ll = do
      ( embedded, meml ) <- fmap L.unzip $ C.mapM embed ll   
      return ( L.concat embedded , Just $ catMaybes meml ) 

instance Embedded LCoreExpr EmbedCoreExpr where
   embed cexp =
      case cexp of
         LCoreMatch lvar lalts -> do
            ( embeds , malts ) <- embed lalts
            return ( embeds, Just $ EmbedCoreMatch lvar $ not_special malts )
         LApp var args         -> return ( [ ] , Just $ EmbedApp var args )
         LSimple simple        -> return ( [ ] , Just $ EmbedSimple simple )

instance Embedded LCoreAlternative EmbedCoreAlternative where
   embed ( sym , lc ) = do
      ( emb , melc ) <- embed lc
      return ( emb , Just ( sym , not_special melc ) ) 

-- A simplified version of core where only simple expressions can be applied to functions and the scope used is well defined for local interactions, and Special statements have been replaced with the name of the appropriate haskell binding, where the binding was formed from the string contained in the special statement

type EmbedCore =
   [ EmbedStmt ]

type EmbedExecCore =
   [ EmbedExecStmt ]

data EmbedExecStmt =
   EmbedStmt EmbedStmt
   | EmbedExecExpr EmbedExecExpr
   deriving ( Show , Eq )

data EmbedStmt = 
   EmbedSetHere      Symbol EmbedExecStmt
   | EmbedSetLocal   Int [ Symbol ] EmbedExecStmt
   | EmbedSetObject  [ Symbol ] EmbedExecStmt
   | EmbedStandalone EmbedCoreExpr
   | EmbedBegin EmbedExecCore
   | EmbedHSFunc Symbol
   deriving ( Show , Eq )

data EmbedCoreExpr =
   EmbedApp LVar [ LVar ]
   | EmbedCoreMatch LVar EmbedAlternatives
   | EmbedSimple LSimpleExpr
   deriving ( Show , Eq )

type EmbedAlternatives =
   [ EmbedCoreAlternative ]

type EmbedCoreAlternative = 
   ( Symbol , EmbedExecCore )

data EmbedExecExpr =
   EmbedFunction     [ Symbol ] EmbedExecCore 
   | EmbedMethod     [ Symbol ] EmbedExecCore
   deriving ( Show , Eq )

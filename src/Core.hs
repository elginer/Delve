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

{-# OPTIONS -XTypeSynonymInstances #-}
-- This module is getting a little big
-- It could be doing with getting split up into a module for a parser, one for the instances, one for the actual AST involved
module Core 
       ( module DMachineState
       , module SExp
       , Core
       , ExecCore
       , Stmt       ( .. )
       , ExecStmt   ( .. )
       , CoreExpr   ( .. )
       , ExecExpr   ( .. )
       , Lit        ( .. )
       , Reserved   ( .. )
       , SimpleExpr ( .. )
       , CoreAlternative
       , CoreAlternatives
       , altsp
       , consume
       )
       where

-- The Delve core language.
-- A non-polymorpic, stripped down version of the delve language.
-- All function applications are saturated

import DMachineState
import SExp
import Symbol

import qualified Data.Attoparsec.Char8    as A
import Data.Attoparsec.Combinator
import Data.List.Stream                   as L
import Data.ByteString.Char8              as B
import Data.ByteString.Lazy.Char8         as Z

import qualified Text.ParserCombinators.Poly.State as P

-- a sexy parser
sex_parse :: SParser a -> [ SExp ] -> a
sex_parse p ts = simple_parse p ts ""

-- reserved words
reserved :: [ String ]
reserved = [
      "fu"
   ,  "me"
   ,  "do"
   ,  ":"
   ,  ":!"
   ,  "self"
   ,  "match"
   ,  "->"
   ]

reserved_sym  = L.map B.pack reserved
reserved_zsym = L.map Z.pack reserved

-- is the symbol legal?
is_legal_var :: Symbol -> Bool
is_legal_var v =
   not ( L.elem v reserved_sym ) && not ( B.elem '#' v ) && not ( B.elem '.' v )

-- gather not a reserved word
legal_var :: SParser Symbol
legal_var = do
   Token _ t <- legal_token
   return t

raise_err msg = do
   p <- P.stGet
   fail $ P.indent 3 $ '\n' : err_pos p L.++ '\n' : msg

legal_token :: SParser SExp
legal_token =
   P.satisfy $ \ s ->
      case s of
         Token _ t ->
            if is_legal_var t
               then True
               else False
         _         -> False

-- The core language can be represented by S-Expressions
instance SExpression Core where
   to_sexp stmts =
      SExp a_sp $ L.map to_sexp stmts
   from_sexp sexp =
      sex_parse ( rec_parse $ stmt_consume ) [ sexp ]

-- I need very specific behaviour from this function, so the general consume will not do
stmt_consume =
   stmt_consume' [ ]


stmt_consume' acc =
   two_left `P.onFail` one_left
   where
   two_left = do
      t1 <- P.next
      t2 <- P.next
      P.reparse [ t1 , t2 ]
      a <- stmtp
      stmt_consume' ( a : acc )
   one_left = with_err_bad "Expected statement" $ do
      a <- stmtp
      return $ L.reverse $ a : acc

-- TODO: move to a combinator module
-- | The parser must consume all remaining input
consume :: P.Parser st t a -> P.Parser st t [ a ]
consume =
   consume' [ ]

consume' :: [ a ] -> P.Parser st t a -> P.Parser st t [ a ]
consume' acc p =
   P.oneOf [ two_left , one_left ]
   where
   two_left = do
      t1 <- P.next
      t2 <- P.next
      P.reparse [ t1 , t2 ]
      a <- p
      consume' ( a : acc ) p
   one_left = P.commit $ do
      a <- p
      return $ L.reverse $ a : acc

instance SExpression ExecCore where
   to_sexp stmts =
      SExp a_sp $ L.map to_sexp stmts
   from_sexp sexp =
      sex_parse ( rec_parse $ exec_corep ) [ sexp ]


instance SExpression Stmt where
   to_sexp stmt =
      case stmt of
         SetHere sym exec_expr   ->
            SExp a_sp [ Token a_sp $ B.singleton ':' , Token a_sp sym , to_sexp exec_expr ]
         SetLocal path exec_expr  ->
            SExp a_sp [ Token a_sp $ B.pack ":!" , Token a_sp $ render_path path , to_sexp exec_expr ]
         SetObject path exec_expr ->
            SExp a_sp [ Token a_sp $ B.pack ":" , Token a_sp $ render_path $ B.pack "self" : path , to_sexp exec_expr ]
         Begin core               ->
            SExp a_sp $ Token a_sp ( B.pack "do" ) : L.map to_sexp core
         Standalone core_expr     ->
            to_sexp core_expr
         CoreSpecial n c          ->
            Special a_sp n c
   from_sexp sexp =
      sex_parse stmtp [ sexp ]

-- Parse a statement
stmtp :: SParser Stmt
stmtp = 
   P.oneOf' stmts
   where
   stmts = 
      [ ( "Update assignment" , rec_parse $ do 
            stoken $ B.pack ":!"
            with_err_bad "Malformed assignment" $ do
               path <- pathp
               exec <- exec_stmtp
               return $ SetLocal path exec )
      , ( "Declaration assignment" , rec_parse $ do 
            stoken $ B.pack ":"
            P.oneOf sets )
      , ( "Block" , rec_parse $ do 
            stoken $ B.pack "do"
            with_err_bad "Malformed block" $ do
               stmts <- exec_corep
               return $ Begin stmts )
      , ( "Expression" , fmap Standalone corep )
      , ( "Special" , do Special _ n c <- special
                         return $ CoreSpecial n c )
      ]
   sets = 
      [  do path <- object_ref
            with_err_bad "Malformed assingment" $ do 
               exec <- exec_stmtp
               return $ SetObject path exec
      ,  do sym <- legal_var
            with_err_bad "Malformed assignment" $ do
               exec <- exec_stmtp
               return $ SetHere sym exec
      ,  do p <- pathp
            with_err_bad "Malformed assignment" $ do
               exec <- exec_stmtp
               return $ SetLocal p exec ]

-- parse a path from the current token
pathp :: SParser [ Symbol ]
pathp = do
   Token _ bs <- any_stoken
   parse_path bs

-- parse a path from the given bytestring
parse_path :: B.ByteString -> SParser [ Symbol ]
parse_path bs = do
   let zbs = Z.fromChunks [ bs ]
       ( _ , sym_check ) = A.parse ( A.manyTill A.anyChar ( A.char '#' ) ) zbs
       ( s , pres )      = A.parse ( A.sepBy a_path_var $ A.char '.' ) zbs

   either ( const $ return ( ) )
          ( const $ raise_err $ "symbol in path\n   In " L.++ show bs L.++ "\n   There must not be any occurrence of '#'" )
          sym_check
   
   either raise_err 
          ( \ res -> 
             if L.null res
                then raise_err "DelveVM: the parser somehow broke.  How unfortunate!"
                else if Z.null s
                        then return $ L.map ( B.concat . Z.toChunks ) res
                        else raise_err $ "reserved word.\n   In " L.++ show bs L.++ "\n   You must remove any reserved words"
          )
          pres


a_path_var :: A.Parser Z.ByteString 
a_path_var = A.notEmpty $ do
   ( A.try $ do A.choice $ L.map A.string reserved_zsym
                return Z.empty )
   A.<|>
   A.takeWhile ( (/=) '.' )

-- read an object reference from the current token
object_ref :: SParser [ Symbol ]
object_ref = do
   Token _ bs <- any_stoken
   if B.isPrefixOf ( B.pack "self." ) bs
      then
         parse_path $ B.drop 5 bs
      else
         raise_err $ show bs L.++ "\nObject reference expects 'self' at beginning of object path"

-- a parser for ExecCore
exec_corep :: SParser ExecCore
exec_corep = do
   stmts <- P.many1 stmtp
   let estmts = L.map Stmt stmts
   ( do exec <- execp
        return $ ExecExpr exec : estmts) `P.onFail` ( return $ estmts )

-- a parser for ExecStmts
-- note: Should not be used to parse blocks: use exec_corep
exec_stmtp :: SParser ExecStmt
exec_stmtp =
   P.oneOf [
      fmap Stmt $  stmtp
   ,  fmap ExecExpr $  execp
   ]

-- a parser for core expressions
-- expects input to be a SExp ( rather than a token ) 
corep :: SParser CoreExpr
corep =
   P.oneOf [
         csimplep
      ,  rec_parse matchp
      ,  rec_parse appp
   ]

appp = do
   e <- exec_stmtp
   with_err_bad "Malformed application" $ do 
      as <- P.many exec_stmtp 
      return $ App e as
matchp = do
   stoken $ B.pack "match"
   with_err_bad "Malformed match" $ do
      e <- exec_stmtp
      alts <- P.many altsp
      return $ CoreMatch e alts
altsp = rec_parse $ do
   stoken $ B.pack "->"
   with_err_bad "Malformed alternative" $ do
      t    <- core_symbolp
      core <- rec_parse $ exec_corep
      return ( t , core )
csimplep =
   fmap Simple simplep

-- a parser for 'exec' expressions
execp :: SParser ExecExpr
execp =
   P.oneOf exec_exprsp
   where
   exec_exprsp :: [ SParser ExecExpr ]
   exec_exprsp = [
         rec_parse $ do 
            stoken $ B.pack "fu"
            with_err_bad "Malformed function" $ do
               fas  <- rec_parse $ P.many legal_var 
               core <- rec_parse $ exec_corep
               return $ Function fas core
      ,  rec_parse $ do
            stoken $ B.pack "me";
            with_err_bad "Malformed method" $ do
               fas  <- rec_parse $ P.many legal_var
               core <- rec_parse $ exec_corep
               return $ Method fas core
      ]

-- parser for simple expressions
simplep :: SParser SimpleExpr
simplep =
   P.oneOf [ fmap Lit litp , fmap Reserved reservedp , varp ]

-- parser for literal expressions
litp :: SParser Lit
litp =
    ( fmap CoreSymbol core_symbolp ) `P.onFail` fmap CorePrim primp
  
core_symbolp :: SParser Symbol
core_symbolp = do
   Token _ bs <- any_stoken
   if B.head bs == '#'
      then return $ B.drop 1 bs
      else raise_err $ show bs L.++ "\nExpected symbol"

-- parser for primative values
primp :: SParser Prim
primp = do
   Token _ bs <- any_stoken
   let mres = B.readInt bs
   maybe ( raise_err $ show bs L.++ "\nExpected integer" )
         ( \ ( i , rest ) ->
            if B.null rest
               then return $ I i
               else raise_err $ show bs L.++ "\nString at end of integer."
         ) mres

-- a parser for reserved words
reservedp :: SParser Reserved
reservedp = do
  Token _ bs <- any_stoken
  if bs == B.pack "self"
     then return Self
     else raise_err $ show bs L.++ "Expected \"self\""

-- a parser for variables
varp :: SParser SimpleExpr
varp =
   objvarp `P.onFail` localvarp
localvarp = do
   path <- pathp
   return $ LocalVar path
objvarp   = do
   path <- object_ref
   return $ ObjectVar path

instance SExpression CoreExpr where
   to_sexp ce =
      case ce of
         App e args       ->
            SExp a_sp $ to_sexp e  : L.map to_sexp args
         CoreMatch e alts ->
            SExp a_sp $ Token a_sp ( B.pack "match" ) : to_sexp e : L.map to_sexp alts
         Simple s                 ->
            to_sexp s
   from_sexp sexp =
      sex_parse corep [ sexp ]

instance SExpression CoreAlternative where
   to_sexp ( sym , alt ) =
      SExp a_sp $ [ Token a_sp $ B.pack "->" , Token a_sp sym , SExp a_sp $ L.map to_sexp alt ]
   from_sexp sexp =
      sex_parse altsp [ sexp ]

instance SExpression ExecStmt where
   to_sexp es =
      case es of
         ExecExpr ex -> to_sexp ex
         Stmt s      -> to_sexp s
   from_sexp sexp =
      sex_parse exec_stmtp [ sexp ]

instance SExpression ExecExpr where
   to_sexp ee =
      case ee of
         Function fargs core ->
            SExp a_sp [ Token a_sp $ B.pack "fu" , SExp a_sp $ L.map ( Token a_sp ) fargs , to_sexp core ]
         Method fargs core   ->
            SExp a_sp [ Token a_sp $ B.pack "me" , SExp a_sp $ L.map ( Token a_sp ) fargs , to_sexp core ]
   from_sexp sexp =
      sex_parse execp [ sexp ]

instance SExpression SimpleExpr where
   to_sexp se =
      case se of
         Lit       l   ->
            to_sexp l
         Reserved  r   ->
            to_sexp r
         LocalVar  path ->
            Token a_sp $ render_path path
         ObjectVar path ->
            Token a_sp $ render_path $ B.pack "self" : path 
   from_sexp sexp =
      sex_parse simplep [ sexp ]

instance SExpression Reserved where
   to_sexp Self =
      Token a_sp $ B.pack "self"
   from_sexp sexp =
      sex_parse reservedp [ sexp ]

instance SExpression Lit where
   to_sexp l =
      case l of
         CorePrim p   ->
            to_sexp p
         CoreSymbol s ->
            Token a_sp $ B.cons '#' s
   from_sexp sexp =
      sex_parse litp [ sexp ]

instance SExpression Prim where
   to_sexp p =
      case p of
         I i ->
            Token a_sp $ B.pack $ show i
   from_sexp sex =
      sex_parse primp [ sex ]

-- The core language is syntactically and semantically equivalent to full delve but simpler ( Delve is a superset of core )
type Core =
   [ Stmt ]

data Stmt = 
   SetHere Symbol ExecStmt
   | SetLocal [ Symbol ] ExecStmt
   | SetObject [ Symbol ] ExecStmt
   | Standalone CoreExpr
   | Begin ExecCore
   | CoreSpecial Symbol String
   deriving ( Show , Eq )

type ExecCore =
   [ ExecStmt ]

data ExecStmt =
   Stmt Stmt
   | ExecExpr ExecExpr
   deriving ( Show , Eq )

data CoreExpr =
   App ExecStmt [ ExecStmt ]
   | CoreMatch ExecStmt CoreAlternatives 
   | Simple SimpleExpr
   deriving ( Show , Eq )

type CoreAlternatives =
   [ CoreAlternative ]

type CoreAlternative =
   ( Symbol , ExecCore ) 

data ExecExpr =
   Function [ Symbol ] ExecCore 
   | Method [ Symbol ] ExecCore
   deriving ( Show , Eq )

data SimpleExpr =
   Lit Lit
   | Reserved  Reserved
   | LocalVar  [ Symbol ]
   | ObjectVar [ Symbol ]
   deriving ( Show , Eq )

data Lit =
   CorePrim Prim
   | CoreSymbol Symbol
   deriving ( Show , Eq )

data Reserved =
   Self
   deriving ( Show , Eq )

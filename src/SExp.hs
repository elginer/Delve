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

-- The syntax of the delve languae is made up of S-Expressions
-- I worry about the -XIncoherentInstances, but it makes it easier to pretty print lists of SExpressions in some cases
{-# OPTIONS -XFlexibleInstances -XUndecidableInstances -XOverlappingInstances -XTypeSynonymInstances -XBangPatterns #-}
module SExp 
   ( SExp ( .. )
   , SExpression ( .. )
   , SParser
   , a_sp
   , stoken
   , simple_parse
   , rec_parse
   , any_stoken
   , sexpp
   , special
   , pretty_print
   , err_pos
   , with_err_bad
   , join_sexps
   , parse_file_sexps
   )
   where

import Symbol
import Compiler ( compile_error )
import DPretty
import CharParser

import Control.Monad.Stream as C

import Data.Maybe
import Data.ByteString.Char8 as B
import Data.List.Stream as L
import qualified Data.Attoparsec.Char8 as A

import qualified Text.ParserCombinators.Poly.State as P


-- | S-Expressions
-- | With the inclusion of some special data: this is so other things ( Haskell, for instance ) can be embedded within Delve 
data SExp =
   SExp {      pos      :: SourcePos 
        ,      list     :: [ SExp ] }
   | Token {   pos      :: SourcePos
           ,   t        :: Symbol }
   | Special { pos      :: SourcePos 
             , st       :: Symbol
             , contents :: String } -- to embed Haskell code within delve

instance SExpression String where
   to_sexp   = SExp a_sp . simple_parse parse_sexps ""
   from_sexp = flip ( pprint 0) ""

instance Show SExp where
   show sexp =
      pprint 0 sexp ""

-- | Join a list of S-Expressions into one sexpression
join_sexps :: [ SExp ] -> SExp
join_sexps = 
   SExp a_sp . L.concatMap from_sexp
   
pretty_print :: [ SExp ] -> String   
pretty_print ss =
   L.foldr ( ( . ) . ( . ( '\n' : ) ) ) id ( L.map ( pprint 0 ) ss ) ""

class SExpression s where
   to_sexp :: s -> SExp
   from_sexp :: SExp -> s

instance SExpression s => Show s where
   show = show . to_sexp

-- | Extract a list of sexpressions from an s-expression
-- | Raise an error if they're not present
instance SExpression [ SExp ] where
   to_sexp = SExp a_sp 
   from_sexp sexp =
      case sexp of
         SExp _ sexps -> sexps
         _            -> compile_error $ "Expected list S-Expression"        

-- an emmiter for S-Expressions
pprint n ( Token _ sym )   =
   (L.++) $ B.unpack sym

pprint n ( SExp _ sexps )  =
  mk_spaces n . (:) '(' . L.foldr (.) id ( L.zipWith (.) ( id : L.repeat ( (:) ' ' ) ) ( L.map ( pprint $ n + 1 ) sexps ) ) . (:) ')'

pprint n ( Special _ t c ) =
   let st = B.unpack t
   in mk_spaces n . (:) '[' . (L.++) st . (:) ']' . (L.++) c . (L.++) "[/" . (L.++) st . (:) ']'

-- a custom sepBy parser
csepBy :: P.Parser s t a -> P.Parser s t sep -> P.Parser s t [ a ]
csepBy = csepBy' [ ]

csepBy' :: [ a ] -> P.Parser s t a -> P.Parser s t sep -> P.Parser s t [ a ]
csepBy' acc ap sepp = P.oneOf [
      ap_first
   ,  only_ap
   ,  nothing
   ]
   where
   ap_first  = do
      a <- ap
      sepp
      csepBy' ( a : acc ) ap sepp
   only_ap   = 
      ap >>= return . L.reverse . flip (:) acc 
   nothing =
      return $ L.reverse acc

sexp :: CharParser ( Maybe SExp )
sexp =  P.oneOf' [
      ( "Comment" , dwspace )
   ,  ( "S-Expression" , fmap Just asexp )
   ,  ( "Special" , fmap Just specialp )
   ,  ( "Token" , fmap Just tokenp )
   ]

with_err_bad msg pa = do
   p <- P.stGet
   P.adjustErrBad pa ( flip (L.++) $ P.indent 3 $ '\n' : err_pos p L.++ '\n' : msg )
   
set_err msg pa = do
   p <- P.stGet
   P.adjustErr pa ( const $ P.indent 3 $ err_pos p L.++ '\n' : msg )


asexp :: CharParser SExp
asexp = do
   p <- P.stGet
   char '('
   with_err_bad "Badly formatted S-Expression" $ asexp_rest p 
   where
   asexp_rest p = do 
      P.optional dwspace
      c <- sexp_contents  -- $ ( flip (L.++) $ P.indent 3 "Badly formatted S-Expression contents" )
      P.optional dwspace
      char ')'
      return $ SExp p c
   sexp_contents = 
      fmap catMaybes $ P.many sexp 
 
-- Whitespace in the delve language
dwspace :: CharParser ( Maybe a )
dwspace = P.oneOf [
      P.many1 scomment >> return Nothing
   ,  P.many1 space >> return Nothing
   ]


scomment :: CharParser ( )
scomment = do
   spaces
   string "//"
   manyTill anyChar ( char '\n' )
   spaces

specialp :: CharParser SExp
specialp = do
   p <- P.stGet
   char '['
   with_err_bad "Badly formatted special data" $ special_rest p 

special_rest p = do
   spaces
   t <- special_sym
   spaces
   char ']'
   s <- manyTill anyChar $ do string "[/"
                              spaces
                              string t
                              spaces
                              char ']'
   return $ Special p ( B.pack t ) s


skipWSpace = A.skipWhile $ A.inClass wchars

wspace = A.satisfy $ A.inClass wchars

wchars = "\n\t \r"

tokenp :: CharParser SExp
tokenp = do
   p <- P.stGet
   t <- P.many1 $ noneOf $ wchars L.++ "()["
   return $ Token p $ B.pack t   

special_sym :: CharParser String
special_sym =
   P.many1 $ noneOf $ wchars L.++ "]"


-- a parser for S-Expressions
parse_sexps :: CharParser [ SExp ]
parse_sexps = fmap catMaybes $ P.many $ do
   P.optional dwspace
   sex <- sexp
   P.optional dwspace
   return sex -- hardeharharhar

-- | The type of parsers which transform S-Expressions to another data type.
type SParser = P.Parser SourcePos SExp

-- | Match a `Special`
special :: SParser SExp
special = set_err "Expected special" $ P.satisfy $ \ s ->
   case s of
      Special _ _ _ -> True
      _             -> False

-- | Match a `Token` which represents the given `Symbol`
stoken :: B.ByteString -> SParser SExp
stoken bs = do
   advance_spos
   set_err ( "Expected token: " L.++ show bs ) $ P.satisfy $ 
      \ sexp ->
         case sexp of
            t@(  Token _ b ) -> 
               if b == bs 
                  then True
                  else False
            _                -> False

-- | Match any sexp `Token`
any_stoken :: SParser SExp
any_stoken = do
   advance_spos
   set_err "Expected any Token" $ P.satisfy $
     \ sexp ->
         case sexp of
            t@(  Token _ _ ) -> True
            _                -> False
 
-- | Match only an `SExp`
sexpp :: SParser SExp
sexpp = do
   advance_spos
   set_err "Expected any S-Expression" $ P.satisfy $ 
      \ sexp ->
         case sexp of
            s@( SExp _ _ ) -> True
            _              -> False

advance_spos = do
   s <- P.next
   P.stUpdate $ const $ pos s
   P.reparse [ s ] 

-- | Recursively parse the elements of a nested `SExp`
-- |
-- | For example, the following will parse correctly
-- |
-- | `rec_parse` $ `stoken` ( pack "hi" ) >> `stoken` ( pack "there" )
-- |
-- | If the remaining input is:
-- |
-- | `SExp` [ Token $ pack "hi" , Token $ pack "there ]
-- |
rec_parse :: SParser a -> SParser a
rec_parse par = do
   SExp _ exps <- sexpp
   rest <- P.many P.next
   P.reparse exps
   a <- par
   P.reparse rest
   return a

a_sp :: SourcePos
a_sp = SourcePos 0 0

-- Parse a list of files with associated file paths
parse_file_sexps :: [ FilePath ] -> [ String ] -> SExp
parse_file_sexps fs dus =
   let sparser = simple_parse parse_sexps
       sexps  = L.concat $ L.zipWith sparser dus fs
   in  SExp a_sp sexps

-- | A simple parser with an associated SourcePos state, triggering an error on failure.
simple_parse :: Show t => P.Parser SourcePos t a -> [ t ] -> String -> a 
simple_parse parser !ts name = 
   let ( eres , sp , rem ) = P.runParser parser a_sp ts 
   in  either ( error . ( ('\n' : name L.++ "\n") L.++) )
              ( \ !res ->
                 if L.null rem
                    then res
                    else error $ ('\n' : name L.++ "\n" ) L.++ err_pos sp L.++
                                 "\nInput not entirely consumed.  Remainder:" L.++
                                 '\n' : show rem )
              eres

err_pos ( SourcePos ln cl ) = "Line " L.++ show ln L.++ ", column " L.++ show cl


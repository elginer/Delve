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

-- A parser over a character string
module CharParser where

import qualified Text.ParserCombinators.Poly.State as P

import Control.Monad.Stream as C

import Data.List as L

-- The CharParser parser is for parsing data from text
type CharParser = P.Parser SourcePos Char

-- The current position of the parser
data SourcePos =
   SourcePos {
      line   :: ! Int
   ,  column :: ! Int
   }
   deriving Show


manyTill :: P.Parser st t a -> P.Parser st t sep -> P.Parser st t [ a ]
manyTill = manyTillRec [ ]

manyTillRec acc p lst =
   ( do lst
        return $ L.reverse acc ) `P.onFail` ( do a <- p
                                                 manyTillRec ( a : acc ) p lst )
  
noneOf l =
   P.satisfy $ not . flip L.elem l  

char :: Char -> CharParser Char
char c = do 
   P.satisfy $ (==) c
   advance_space
   return c

string s =
   C.sequence_ $ L.map char s

space = 
   P.oneOf [
      nl
   ,  tab
   ,  br
   ]

nl =
   P.oneOf [ crlf , cr, lf ]
  
crlf = do
   string "\r\n"
   advance_line

cr = do
   char '\r'
   advance_line

lf = do
   char '\n'
   advance_line

br = do
   char ' '
   advance_space

tab = do
   char '\t'
   advance_space

advance_line = P.stUpdate $ \ ( SourcePos l _ ) -> SourcePos ( l + 1 ) 0

advance_space = P.stUpdate $ \ sp -> sp { column = column sp + 1 }

spaces = P.many space >> return ( )

anyChar :: CharParser Char
anyChar =
   P.satisfy $ const True

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

{-# OPTIONS -XRankNTypes #-}
module UniqueVars 
   ( module Control.Monad.RWS
   , UniqueVarsM
   , SimpleVars
   , new_name
   )
   where

-- | A monad for generating unique variable names.

import Symbol
import Control.Monad.RWS
import Data.List.Stream as L
import Data.ByteString as B
import Data.Word
import Data.Binary.Put
import Data.ByteString.Lazy as Z
import qualified Data.Binary as Bin
import Data.Char

-- | A RWS allowing access to unique symbols
type UniqueVarsM a b = RWS a b Word64

-- | A Simple varient of UniqueVarsM.  It rather disrupts 'from simple to complex'.
type SimpleVars = UniqueVarsM ( ) ( )

-- | Create a new unique symbol
new_name :: Monoid b => UniqueVarsM a b Symbol
new_name = do
   w <- get
   let new_w = w + 1
   put new_w
   return $ mk_unique_sym w

mk_unique_sym :: Word64 -> Symbol
mk_unique_sym = mk_unique_sym' [ ]

mk_unique_sym' :: [ Word8 ] -> Word64 -> Symbol
mk_unique_sym' acc w =
   if w >= 26
      then
         mk_unique_sym' ( w8 : acc ) $ ( w `div` 26 ) - 1
      else B.pack $ [ 95 , 86 , 95 ] L.++ 97 + fromIntegral w : acc
   where
   w8 :: Word8
   w8 =  fromIntegral $ ( w `mod` 26 ) + 97
   

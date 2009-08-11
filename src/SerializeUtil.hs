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

-- A few utilities for serialization
module SerializeUtil where

import Data.Word
import Data.Int

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

import Data.List as L

import Control.Monad.Stream as C

-- It would be cool to use multi-param classes to paramitize this over 
instance Binary a => Binary ( DList a ) where
   get = do
      w <- get
      fmap DList $ C.sequence $ L.genericTake ( w :: Word64 ) $ L.repeat get
   put ( DList c ) = do 
      put $ w64 $ L.length c
      C.mapM_ put c

newtype DList a = DList [ a ]


li_get :: Binary a => Get [ a ]
li_get = do
   DList a <- get
   return a

lput :: Binary a => [ a ] -> PutM ( )
lput = put . DList

i64_get :: Integral i => Get i
i64_get = do
   w <- get :: Get Int64
   return $ fromIntegral w

i64_put :: Integral i => i -> PutM ( )
i64_put = put . i64

w8_put :: Integral i => i -> PutM ( )
w8_put = put . w8
         
w8 :: Integral i => i -> Word8
w8 = fromIntegral

i64 :: Integral i => i -> Int64
i64 = fromIntegral

w64 :: Integral i => i -> Word64
w64 = fromIntegral

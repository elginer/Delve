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

-- Test binary serialization is working ok
{-# OPTIONS -XTypeSynonymInstances #-}
module DCodeTest where

import Data.Int
import Data.ByteString.Char8

import Control.Monad

import Test.QuickCheck

import System.Random

import DMachineState

instance Arbitrary Expr where
   arbitrary = oneof [
         new
      ,  refself
      ,  refobj
      ,  newblock
      ,  reflocal
      ,  newprim
      ,  newsymbol
      ,  rememberobjobj
      ,  rememberobjlocal
      ,  rememberlocalobj
      ,  rememberlocallocal
      ,  pushframeobj
      ,  pushframelocal
      ,  popframe
      ,  pushobjarg
      ,  pushlocalarg
      ,  writearg
      ,  assignlocal
      ,  assignobject
      ,  loadobj
      ,  loadlocal
      ,  popobject
      ,  localtailcallobj
      ,  localtailcalllocal
      ,  frametailcallobj
      ,  callobj
      ,  calllocal
      ,  pushlocal
      ,  poplocal
      ,  matchlocal
      ,  matchobj
      ,  callcc
      ,  jumpobj
      ,  jumplocal
      ]

new = return New

refself = return RefSelf

refobj = arg1 RefObj

newblock = arg1 NewBlock 

reflocal = loc_gen RefLocal

newprim = fmap ( NewPrim . I ) i64

newsymbol = arg1 NewSymbol

rememberobjobj = arg1 RememberObjObj

rememberobjlocal = loc_gen RememberObjLocal

rememberlocalobj = arg1 RememberLocalObj

rememberlocallocal = loc_gen RememberLocalLocal

pushframeobj = arg1 PushFrameObj

pushframelocal = loc_gen PushFrameLocal

popframe = return PopFrame

pushobjarg = liftM2 PushObjArg arbitrary w8

pushlocalarg = do
   w <- w8
   f <- loc_gen PushLocalArg
   return $ f w

writearg = liftM2 WriteArg w8 arbitrary

assignlocal = do 
   s <- arbitrary
   f <- loc_gen AssignLocal
   return $ f s

assignobject = arg2 AssignObject

loadobj = arg1 LoadObj

loadlocal = loc_gen LoadLocal

popobject = return PopObject

localtailcallobj = arg2 LocalTailCallObj

localtailcalllocal = arbitrary >>= loc_gen . LocalTailCallLocal

frametailcalllocal = loc_gen FrameTailCallLocal

frametailcallobj = arg1 FrameTailCallObj

callobj = arg1 CallObj

calllocal = loc_gen CallLocal

pushlocal = return PushLocal

poplocal = return PopLocal

matchlocal = do
   as <- arbitrary
   f <- loc_gen MatchLocal
   return $ f as

matchobj = arg2 MatchObj

callcc = arg1 CallCC

jumplocal = loc_gen JumpLocal

jumpobj = arg1 JumpObj

arg2 :: ( Arbitrary u , Arbitrary v ) => ( u -> v -> w ) -> Gen w
arg2 f = liftM2 f arbitrary arbitrary

loc_gen :: ( Arbitrary a , Random i , Integral i ) => ( i -> a -> b ) -> Gen b
loc_gen f = liftM2 f w8 arbitrary

w8 :: ( Integral i ) => Gen i
w8 =
   fmap fromIntegral $ choose ( 0 :: Int , 255 :: Int )

i64 :: ( Integral i ) => Gen i
i64 =
   fmap fromIntegral $ choose ( fromEnum ( minBound :: Int64 ) , fromEnum ( maxBound :: Int64 ) )

arg1 :: Arbitrary a => ( a -> b ) -> Gen b
arg1 f = fmap f arbitrary

instance Arbitrary Symbol where
   arbitrary = fmap pack arbitrary

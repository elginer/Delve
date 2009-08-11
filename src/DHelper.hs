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

-- Helper functions for writing embedded Delve programs
module DHelper where

import DMachineState
import DelveVM
import Prelude as P
import Control.Monad.Stream as C
import Data.List.Stream as L
import qualified Data.Map as M

import Data.ByteString.Char8 as B

stats = do
   DState astack _ _ args  <- get
   liftIO $ do P.putStrLn "Global information"
               P.putStrLn $ "Size of activation stack: " L.++ show ( L.length astack )
   ActivationFrame os envs <- get_activation_frame
   liftIO $ do P.putStrLn "Activation information"
               P.putStrLn $ "Size of object stack: " L.++ show ( L.length os )
               P.putStrLn $ "Size of environment stack: " L.++ show ( L.length envs )

print_int = do
   i <- get_arg_int 0
   liftIO $ print i

print_sym = do
   i <- get_arg_sym 0
   liftIO $ print i

get_arg_sym i = do
   a <- get_arg i
   let symio = get_symbol a
   liftIO $ readIORef symio

dbg_msg :: String -> DelveM ( )
dbg_msg msg = do
   liftIO $ P.putStrLn msg

{-
 
I got distracted from actually writing the compiler there!
Maybe some time we'll want to print out the whole contents of the VM
It would probably be cool, anyhows.

dump_object = do
   a <- get_arg 0
   liftIO . putStrLn C.=<< flip show_io 0 "" C.=<< liftIO . readIORef C.=<< get_object a
  
-- a better show function
class ShowIO o where
   show_io :: o -> Int -> String -> String

foldrM :: Monad m => ( a -> b -> m b ) -> b -> [ a ] -> m b
foldrM f b as = ( L.foldr (C.<=<) return ) ( L.map f as ) b

instance ShowIO Addr where
   

instance ShowIO Object where
   show_io obj n =
      let add_obj :: IO String -> IOString
          add_obj = fmap ( (L.++) $! mk_spaces n L.++ "(object " ) 
          show_members :: [ String -> IO String ]
          show_members = L.map ( flip show_io $! n + 1 ) $! M.elems members 
      in  add_obj . foldrM (C.<=<) return show_members . (:) ')'

instance ShowIO Symbol where
   show_io sym n =
      return . mk_spaces n . (L.++) "(symbol '" . (L.++) B.unpack sym . (L.++) "')"

instance ShowIO Prim where
   show_io p n =
      case p of
         I i -> return . mk_spaces n . (L.++) "(prim " . (L.++) show i . (:) ")"
-}
get_arg_int :: Word8 -> DelveM Int
get_arg_int i = do
   a <- get_arg i
   let pio = get_prim a
   I x <- liftIO $ readIORef pio
   return x

primative_mult :: DelveM ( )
primative_mult = math_op (*)

primative_div :: DelveM ( )
primative_div = math_op div

math_op :: ( Int -> Int -> Int ) -> DelveM ( )
math_op f = do
   i1 <- get_arg_int 0
   i2 <- get_arg_int 1
   res <- fmap PrimAddr $ liftIO $ newIORef $! I $! i1 `f` i2
   put_result res

logic_op :: ( Int -> Int -> Bool ) -> DelveM ( )
logic_op f = do
   i1 <- get_arg_int 0
   i2 <- get_arg_int 1
   res <- fmap SymbolAddr $ liftIO $ newIORef $! B.pack $! show $! i1 `f` i2
   put_result res

add_ints :: DelveM ( )
add_ints = math_op (+)

primative_lte = logic_op (<=) 

primative_eq = logic_op (==)

primative_gt = logic_op (>)

primative_lt = logic_op (<)

primative_gte = logic_op (>=)

primative_pow = math_op intpow
   where
   intpow :: Int -> Int -> Int
   intpow = (^)

subtract_ints :: DelveM ( )
subtract_ints = math_op (-)

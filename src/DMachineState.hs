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

{-# OPTIONS -XBangPatterns -XTypeSynonymInstances #-}
-- a virtual machine for the Delve programming language
-- this machine uses IORefs to store the state of all the values, and so does not need an explicit heap ( or dump ). 
-- Since it does not have an explicit heap, it can rely on haskell for garbage collection
module DMachineState 
       ( module Symbol
       , DelveM
       , DState          ( .. )
       , Continuation
       , Addr            ( .. )
       , ActivationStack
       , ActivationFrame ( .. )
       , MemoryFrame     ( .. )
       , ObjectStack 
       , Object          ( .. )
       , LocalEnvStack
       , LocalEnv        ( .. )
       , Code
       , Expr            ( .. )
       , Alternatives
       , Alternative
       , Prim            ( .. )
       , ArgGroup
       , CodeBlock       ( .. )
       , Members
       )
       where

import Data.Int
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.IORef
import qualified Data.Map as M
import qualified Data.ByteString as B
import Data.Array.IO
-- import Data.Binary as Bin
import Data.Word
import Data.List.Stream as L

import Control.Monad.State.Strict hiding ( put , get )
import Control.Monad.Stream as C

import SerializeUtil
import Symbol

-- STATE
-- The state of the virtual machine

-- Delve computations take place in the Delve monad; DelveM
type DelveM = StateT DState IO

data DState =
   DState {
      activation_stack :: ! ActivationStack -- the activation stack
      , result         :: ! ( Maybe Addr ) -- The address of the last return value
      , code           :: ! Code -- The code being executed
      , args           :: ! ArgGroup -- The arguments
   }

-- A continuation is just a snapshot of the state of the whole machine
type Continuation =
   DState

-- An address is a value wrapped in a transactional variable
data Addr =
   ObjAddr      ( IORef Object )
   | PrimAddr   ( IORef Prim )
   | CodeAddr   ( IORef CodeBlock )
   | SymbolAddr ( IORef Symbol )
   | ContAddr   ( IORef Continuation )

-- The top element of the activation stack is the current activation frame
type ActivationStack =
   [ ActivationFrame ]

 -- The activation frame contains the local environment and objects in scope
data ActivationFrame =
   ActivationFrame {
      object_stack :: ! ObjectStack  -- The objects in scope
      , local_env  :: ! LocalEnvStack -- The local environment
   }

-- Used by functions to construct the next activation frame
-- If a stack is not present, the old stack is used
data MemoryFrame =
   MemoryFrame {
      mem_object_stack :: ! ( Maybe ( IORef Object ) )  -- The objects in scope
      , mem_local_env  :: ! ( Maybe LocalEnvStack ) -- The local environment
   }

-- The top element of the object stack is the object inside of which code is currently being evaluated
type ObjectStack =
   [ IORef Object ]

-- Each object is a mapping of symbols to addresses
data Object =
   Object {
      members :: ! Members -- The members of the object, its methods and variables
   }

-- The members of an object
type Members = 
   M.Map Symbol Addr

-- The local environment stack is a stack of local variables, the topmost of which is the current local environment
type LocalEnvStack =
   [ IORef LocalEnv ]

-- The local environment is a mapping of local variables to addresses
data LocalEnv =
   LocalEnv {
      env :: ! Members -- The mapping
   }

-- An ArgGroup is a an IOArray of addresses
type ArgGroup =
   IOArray Word8 Addr


-- The primative types available
data Prim =
   I Int
   deriving ( Show , Eq )

-- CODE
-- The code which runs inside the virtual machine

-- Each code block has an associated activation frame
data CodeBlock =
   CodeBlock {
      memory_frame :: ! MemoryFrame -- The frame
      , block      :: ! Code -- The code
   }
-- Code is a list of expressions
type Code = 
   [ Expr ]


-- it would be nice to replace these different calls to w by some type class instances
instance Binary Expr where
   get = do
      w <- get
      get_expr w

   put c =
      case c of
         New                      -> w8_put   0
         RefSelf                  -> w8_put   1
         NewBlock c               -> li_put   2 c
         RefLocal i path          -> loc_put  3 i path
         NewPrim ( I i )          -> w8_put   4 >> i64_put i
         NewSymbol s              -> w8_put   5 >> put s
         RememberObjObj p         -> li_put   6 p
         RememberObjLocal i p     -> loc_put  7 i p
         RememberLocalObj p       -> li_put   8 p
         RememberLocalLocal i p   -> loc_put  9 i p
         PushFrameObj p           -> li_put  10 p
         PushFrameLocal i p       -> loc_put 11 i p
         PopFrame                 -> w8_put  12
         PushObjArg s w           -> li_put  13 s >> put w
         PushLocalArg i s w       -> loc_put 14 i s >> put w
         WriteArg w s             -> w8_put  15 >> put w >> put s
         AssignLocal i p s        -> loc_put 16 i p >> put s
         AssignObject p s         -> li_put  17 p >> put s
         LoadObj p                -> li_put  18 p
         LoadLocal i p            -> loc_put 19 i p
         PopObject                -> w8_put  20
         LocalTailCallObj b p     -> li_put  21 p >> put b
         LocalTailCallLocal b i p -> loc_put 22 i p >> put b
         FrameTailCallLocal i p   -> loc_put 23 i p
         FrameTailCallObj p       -> li_put  24 p
         CallObj p                -> li_put  25 p
         CallLocal i p            -> loc_put 26 i p
         PushLocal                -> w8_put  27
         PopLocal                 -> w8_put  28
         MachineInstruction _     ->           error "Oooops... attempting to compile a Haskell expression into Delve bytecode"
         MatchLocal i p as        -> loc_put 29 i p >> put ( DList $ L.map DAlt as )
         MatchObj p as            -> li_put  30 p >> put ( DList $ L.map DAlt as )
         CallCC c                 -> li_put  31 c
         JumpLocal i p            -> loc_put 32 i p
         JumpObj p                -> li_put  33 p
        
get_expr :: Word8 -> Get Expr
get_expr 0 = return New

get_expr 1 = return RefSelf

get_expr 2 = fmap NewBlock li_get

get_expr 3 = fmap ( uncurry RefLocal ) loc_get
   
get_expr 4 = fmap ( NewPrim . I ) i64_get

get_expr 5 = fmap NewSymbol get

get_expr 6 = fmap RememberObjObj li_get

get_expr 7 = fmap ( uncurry RememberObjLocal ) loc_get

get_expr 8 = fmap RememberLocalObj li_get

get_expr 9 = fmap ( uncurry RememberLocalLocal ) loc_get

get_expr 10 = fmap PushFrameObj li_get

get_expr 11 = fmap ( uncurry PushFrameLocal ) loc_get

get_expr 12 = return PopFrame

get_expr 13 = C.liftM2 PushObjArg li_get get

get_expr 14 = do
   ( i , p ) <- loc_get
   fmap ( PushLocalArg i p ) get

get_expr 15 = C.liftM2 WriteArg get get

get_expr 16 = do
   ( i , p ) <- loc_get
   fmap ( AssignLocal i p ) get

get_expr 17 = C.liftM2 AssignObject li_get get

get_expr 18 = fmap LoadObj li_get

get_expr 19 = fmap ( uncurry LoadLocal ) loc_get 

get_expr 20 = return PopObject

get_expr 21 = C.liftM2 (flip LocalTailCallObj) li_get get

get_expr 22 = do
   ( i , p ) <- loc_get
   b <- get
   return $ LocalTailCallLocal b i p

get_expr 23 = fmap (uncurry FrameTailCallLocal) loc_get

get_expr 24 = fmap FrameTailCallObj li_get

get_expr 25 = fmap CallObj li_get

get_expr 26 = fmap (uncurry CallLocal) loc_get

get_expr 27 = return PushLocal

get_expr 28 = return PopLocal

get_expr 29 = do
   ( i , p ) <- loc_get
   fmap ( MatchLocal i p ) li_get

get_expr 30 = C.liftM2 MatchObj li_get li_get

get_expr 31 = fmap CallCC li_get

get_expr 32 = fmap (uncurry JumpLocal) loc_get

get_expr 33 = fmap JumpObj li_get

get_expr _  = error "Error reading DCode: invalid op!"
-- customize serialization of alternatives 
newtype DAlt = DAlt Alternative

instance Binary DAlt where
   put ( DAlt ( s , c ) ) = put s >> put ( DList c )
   get = do
      s <- get
      DList c <- get
      return $ DAlt ( s , c )
      
loc_get :: ( Binary a , Integral i ) => Get ( i , [ a ] )
loc_get = do
   w <- get :: Get Word8
   l <- li_get
   return ( fromIntegral w , l )

li_put :: Binary a => Word8 -> [ a ] -> PutM ( )
li_put i as = put i >> put ( DList as )

loc_put :: ( Binary a , Integral i ) => Word8 -> i -> [ a ] -> PutM ( )
loc_put c d as = put c >> put ( w8 d ) >> put ( DList as ) 


-- The expressions are as follows
data Expr =
   New                                              -- create a new object
   | RefSelf                                        -- move the current object into the return slot
   | RefObj             [ Symbol ]                  -- set the result address to the object at this path in the current object
   | NewBlock           Code                        -- assign space for some code and put the address in result
   | RefLocal           Int [ Symbol ]              -- set the result address to the object at this path in the local environment
   | NewPrim            Prim                        -- assign space for a new primative value and set the result to that address
   | NewSymbol          Symbol                      -- assign space for a new symbol and set the result to this address
   | RememberObjObj     [ Symbol ]                  -- associate the current object stack with this symbol in the object at the end of the path, starting from the current object
   | RememberObjLocal   Int [ Symbol ]              -- associate the current object stack with the object at the end of the path, starting from  this symbol in the local scope
   | RememberLocalObj   [ Symbol ]                  -- associate the current local environment with this symbol in the object at the end of the path, starting from the current object
   | RememberLocalLocal Int [ Symbol ]              -- associate the current local environment with this symbol in the object at the end of the path, starting from this symbol in the local scope
   | PushFrameObj       [ Symbol ]                  -- find the activation frame associated with this symbol in the current object, and push it to the top of the stack
   | PushFrameLocal     Int [ Symbol ]              -- find the activation frame associated with this symbol in the local env, and push it to the top of the stack
   | PopFrame                                       -- Pop an activation frame from the top of the stack
   | PushObjArg         [ Symbol ] Word8            -- Push the address referred to by the symbol in the current object on to the argument array at this index 
   | PushLocalArg       Int [ Symbol ] Word8        -- Push the address referred to by the symbol in the local environment on to the argument stack at this index
   | WriteArg           Word8 Symbol                -- Set the argument at the index ( which should be counted from the other end, it's more efficient to work backwards ) in the current argument list equal to the symbol in the local environment
   | AssignLocal        Int [ Symbol ] Symbol       -- Read the address in the result slot, and set a mapping between the symbol and this address in the environment given by the stack index and at the end of the chain of object references
   | AssignObject       [ Symbol ] Symbol           -- Read the address in the result slot, and set a mapping between the symbol and this address in the object at the end of the chain of object references
   | LoadObj            [ Symbol ]                  -- Add the object refered to by the symbol in the current object to the top of the object stack ( it becomes the current object )
   | LoadLocal          Int [ Symbol ]              -- Add the object refered to by the symbol in the local environment to the top of the object stack ( it becomes the current object )
   | PopObject -- Pop the top object from the object stack
   | LocalTailCallObj   Bool [ Symbol ]                   -- Tail call the code refered to by the symbol in the current object by concatenating it with the rest of the executing code and popping the local scope before it is executed, if boolean 'pop' is true, then pop the current object
   | LocalTailCallLocal Bool Int [ Symbol ]               -- Call the code referred to by the symbol in the current local environment by concatenating it with the rest of the executing code and popping the local scope before it is executed, if boolean 'pop' is true, then pop the current object
   | FrameTailCallLocal Int [ Symbol ]               -- Call the code referred to by the symbol in the current local environment by concatenating it with the rest of the executing code and popping the activation frame before it is executed
   | FrameTailCallObj   [ Symbol ]                  -- Call the code referred to by the symbol in the current object by concatenating it with the rest of the executing code and popping the activation frame before it is executed
   | CallObj            [ Symbol ]                  -- Call the code refered to by the symbol in the current object by concatenating it with the rest of the executing code
   | CallLocal          Int [ Symbol ]              -- Call the code refred to by the symbol in the current local environment by concatenating it with the rest of the executing code
   | PushLocal                                      -- Add a new empty local scope to the local environment stack
   | PopLocal                                       -- Pop a local scope from the local environment
   | MachineInstruction DelveInstruction             -- An arbitrary machine instruction
   | MatchLocal         Int [ Symbol ] Alternatives -- A symbolic pattern match, comparing the symbol at the address referenced by symbol in the local environment to one of the alternatives
   | MatchObj           [ Symbol ] Alternatives     -- A symbolic pattern match, comparing the symbol at the address referenced by symbol in the current object to one of the alternatives
   | CallCC                Code                     -- Create a new continuation and add it to the argument list
   | JumpObj               [ Symbol ]               -- Jump to the continuation referenced by the symbol in the current object
   | JumpLocal             Int [ Symbol ]           -- Jump to the continuation referenced by the symbol in the local environment
   deriving ( Show , Eq )

instance Eq DelveInstruction where
   _ == _ = error "Cannot compare DelveVM Machine instructions"

type DelveInstruction = 
   DelveM ( )

instance Show DelveInstruction where
   show _ = "<DelveVM Code>"

-- The alternatives are a list of pairs of patterns with code
type Alternatives =
   [ Alternative ] 

type Alternative = ( Symbol , Code )

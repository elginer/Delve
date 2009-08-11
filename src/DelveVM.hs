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

-- The virtual machine for the delve programming language
{-# OPTIONS -XBangPatterns #-}
module DelveVM
       ( module Control.Monad.State.Strict
       , module Data.Maybe
       , module Data.Word
       , eval
       , DState ( .. )
       , ActivationFrame ( .. )
       , Object ( .. )
       , LocalEnv ( .. )
       , Expr ( .. )
       , get_object
       , get_current_object_ref
       , get_arg
       , push_code
       , get_prim
       , get_symbol
       , get_cont
       , get_code_block
       , run_delve 
       , lookup_current_object
       , lookup_local_env
       , get_activation_frame
       , get_current_object
       , get_result
       , irrefutible_lookup
       , put_result 
       , new
       , new_prim
       , call_cc
       , jump_local
       , jump_obj
       , newIORef
       , modifyIORef
       , readIORef
       )
       where

import DMachineState

import Control.Monad.State.Strict
import Control.Applicative
import Control.Exception as E

import Control.Monad.Stream as C
import Data.List.Stream as L

import Data.Word
import Data.IORef 
-- hiding ( newIORef , modifyIORef , readIORef , writeIORef )
--import qualified Data.IORef as I ( newIORef , modifyIORef , readIORef , writeIORef )
import qualified Data.Map as M
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Data.Array.IO

{-
-- SHOULD THESE EVEN BE STRICT!!?!
-- a strict version of 'newIORef'
newIORef :: a -> IO ( IORef a )
newIORef !x = I.newIORef x

-- a strict version of 'modifyIORef'
modifyIORef :: IORef a -> ( a -> a ) -> IO ( )
modifyIORef !ref !f = do
   a <- readIORef
   let new_a = f $! a
   case new_a of
      !forced_a -> writeIORef ref forced_a

writeIORef :: IORef a -> a -> IO ( )
writeIORef !ref !a =
   I.writeIORef ref a
         
   
-- a strict version of 'readIORef'
readIORef :: IORef a -> IO a
readIORef ref = do
   a <- I.readIORef ref
   case a of
      !stuff -> return stuff
-}
-- Run a Delve program
run_delve :: Code -> IO ( )
run_delve code = do 
   new_delve_state code >>= runStateT eval 
   return ( )

-- | Add a new object to the heap and store the address in the result slot
new :: DelveM ( )
new = do
   o <- new_object_addr
   put_result o 

-- | Add a new primative to the heap and store the address in the result slot 
new_prim :: Prim -> DelveM ( )
new_prim prim = do
   prim_addr <- new_prim_addr prim
   put_result prim_addr

{-

This function ( eval_expr ) is plainly ridiculous.  It needs to become a type class!

-}

-- | Evaluate an expression
eval_expr :: Expr -> DelveM ( ) 
-- create a new object
eval_expr New =
   new 
-- add a new block of code to the heap and store the address in the result slot
eval_expr ( NewBlock code ) = do
   c_addr <- new_code_addr code
   put_result c_addr   
-- set the result address to this symbol in current object
eval_expr ( RefObj path ) = do
   a <- lookup_current_object path 
   put_result a
-- set the result address to this symbol in the local environment
eval_expr ( RefLocal depth path ) = do
   e <- get_environment_at depth
   a <- search_environment e path
   put_result a
-- create a new primative
eval_expr ( NewPrim prim ) = 
   new_prim prim
-- associate the current object stack with this symbol in the current object
eval_expr ( RememberObjObj path ) = do
   a <- lookup_current_object path
   remember_objects a
-- associate the current object stack with this symbol in the local scope
eval_expr ( RememberObjLocal depth path ) = do
   le <- get_environment_at depth
   ela <- find_addr_local le path
   either ( const $ local_memory_error )
          remember_objects
          ela
-- associate the current local environment with this symbol in the current object
eval_expr ( RememberLocalObj path ) = do
   a <- lookup_current_object path
   remember_local a
-- associate the current local environment with this symbol in the local scope
eval_expr ( RememberLocalLocal depth path ) = do 
   le <- get_environment_at depth
   ela <- find_addr_local le path
   either ( const $ local_memory_error )
          remember_local
          ela
-- push the activation frame associated with the code block refered to by the symbol in the current object on the top of the activation stack
eval_expr ( PushFrameObj path ) = do
   a <- lookup_current_object path
   fr <- get_address_frame a
   vat <- get_activation_frame
   push_activation_frame vat fr 
-- push the activation frame associated with the code block refered to by the symbol in the local environment to the top of the activation stack
eval_expr ( PushFrameLocal depth path ) = do
   e <- get_environment_at depth
   a <- search_environment e path
   fr <- get_address_frame a
   vat <- get_activation_frame
   push_activation_frame vat fr
-- pop the current activation frame from the stack
eval_expr PopFrame = do
   pop_activation_frame
   return ( )
-- Push the address referred to by the symbol in the current object on to the argument stack
eval_expr ( PushObjArg path w ) = do
   a <- lookup_current_object path
   push_arg a w
-- Push the address referred to by the symbol in the local environment on to the argument stack
eval_expr ( PushLocalArg depth path w ) = do
   e <- get_environment_at depth
   a <- search_environment e path
   push_arg a w
-- Set the argument at the index ( which should be counted from the other end, it's more efficient to work backwards ) in the current argument list equal to the symbol in the local environment
eval_expr ( WriteArg i sym ) = do
   arg <- get_arg i
   le <- get_current_env
   assign_local le [ ] sym arg
-- Read the address in the result slot, and set a mapping between the symbol and this address in the local environment
eval_expr ( AssignLocal depth path sym ) = do
   a <- get_result
   ev <- get_environment_at depth
   assign_local ev path sym a
-- Read the address in the result slot, and set a mapping between the symbol and this address in the object at the end of the chain of object references
eval_expr ( AssignObject path sym ) = do
   a <- get_result
   assign_object path sym a
-- Add the object refered to by the symbol in the current object to the top of the object stack ( it becomes the current object )
eval_expr (LoadObj path ) = do
   o <- get_current_object_ref
   a <- find_addr path $ ObjAddr o
   let o = get_object a
   push_object o
-- Add the object refered to by the symbol in the local environment to the top of the object stack ( it becomes the current object )
eval_expr ( LoadLocal depth path ) = do
   e <- get_environment_at depth
   a <- search_environment e path
   let o = get_object a
   push_object o
-- Pop an object from the top of the current object stack
eval_expr PopObject = 
   pop_object
-- Call the code refered to by the symbol in the current object by concatenating it with the rest of the executing code
eval_expr ( CallObj sym ) = do
--   liftIO $ putStrLn "gonna lookup"
   o <- get_current_object
   a <- lookup_current_object sym
--   liftIO $ do putStrLn "lookup up object"
--               case a of
--                  _ -> putStrLn "got summink"
   let cbio = get_code_block a
   cb <- liftIO $ readIORef cbio
   push_code_block cb
   vat <- get_activation_frame
   push_activation_frame vat $ memory_frame cb
-- Call the code refred to by the symbol in the current local environment by concatenating it with the rest of the executing code
eval_expr ( CallLocal depth path ) = do
   a <- lookup_local_env depth path
   let cbio = get_code_block a
   cb <- liftIO $ readIORef cbio
   push_code_block cb
   vat <- get_activation_frame
   push_activation_frame vat $ memory_frame cb
-- Add a new empty local scope to the local environment stack
eval_expr PushLocal = do
   push_local
-- Pop the last local scope from the environment stack
eval_expr PopLocal = do
   pop_local
-- Perform an arbitrary machine instruction
eval_expr ( MachineInstruction command ) =
   command
-- A symbolic pattern match, comparing the symbol at the address referenced by symbol in the current object to one of the alternatives
eval_expr ( MatchObj path alts ) = do
   o <- get_current_object_ref
   a <- find_addr path $ ObjAddr o
   sym <- liftIO $ readIORef $ get_symbol a
   match sym alts
-- A symbolic pattern match, comparing the symbol at the address referenced by symbol in the local environment to one of the alternatives
eval_expr ( MatchLocal depth path alts ) = do
   e <- get_environment_at depth
   a <- search_environment e path
   sym <- liftIO $ readIORef $ get_symbol a
   match sym alts
-- move the current object into the return slot
eval_expr RefSelf = do
   o <- get_current_object_ref
   o2 <- get_current_object
   put_result $ ObjAddr o
-- Tail call the code refered to by the symbol in the current object by concatenating it with the rest of the executing code and popping the local scope before it is executed
eval_expr ( LocalTailCallObj pop sym ) = do
   a <- lookup_current_object sym
   let cbio = get_code_block a
   cb <- liftIO $ readIORef cbio
   fr <- get_address_frame a
   vat <- get_activation_frame
   pop_local
   C.when pop pop_object
   push_activation_frame vat fr
   push_code_block cb
-- Call the code refered to by the symbol in the current local environment by concatenating it with the rest of the executing code and popping the local scope before it is executed -- pop the current object if the flag is set
-- I think there's a bit of in consistancy here...
eval_expr ( LocalTailCallLocal pop depth sym ) = do
   a <- lookup_local_env depth sym
   let cbio = get_code_block a
   cb <- liftIO $ readIORef cbio
   fr <- get_address_frame a
   vat <- get_activation_frame
   pop_local
   C.when pop pop_object
   push_activation_frame vat fr
   push_code_block cb
-- Call the code referred to by the symbol in the current local environment by concatenating it with the rest of the executing code and popping the activation frame before it is executed
eval_expr ( FrameTailCallLocal depth sym ) = do
   a <- lookup_local_env depth sym
   let cbio = get_code_block a
   cb <- liftIO $ readIORef cbio
   fr <- get_address_frame a
   vat <- pop_activation_frame
   push_activation_frame vat fr
   push_code_block cb
-- Call the code referred to by the symbol in the current object by concatenating it with the rest of the executing code and popping the activation frame before it is executed
eval_expr ( FrameTailCallObj sym ) = do
   a <- lookup_current_object sym
   let cbio = get_code_block a
   cb <- liftIO $ readIORef cbio
   fr <- get_address_frame a
   vat <- pop_activation_frame
   push_activation_frame vat fr
   push_code_block cb
-- assign space for a new symbol and set the result to this address
eval_expr ( NewSymbol sym ) = do
   sym_addr <- fmap SymbolAddr $ liftIO $ newIORef sym
   put_result sym_addr
-- call-with-current-continuation: 
eval_expr ( CallCC code ) = 
   call_cc code
-- Jump to the continuation referenced by the symbol in the current object
eval_expr ( JumpObj path ) =
   jump_obj path
-- Jump to the continuation referenced by the symbol in the local environment
eval_expr ( JumpLocal depth path ) = 
   jump_local depth path

-- Jump to the continuation referenced by the symbol in the local environment
jump_local :: Int -> [ Symbol ] -> DelveM ( )
jump_local depth path = do
   e <- get_environment_at depth
   cio <- fmap get_cont $ search_environment e path
   c <- liftIO $ readIORef cio
   r <- get_result
   put c
   put_result r

-- | call-with-current-continuation: create a new continuation and add it to a new arg list
call_cc :: Code -> DelveM ( )
call_cc code = do
   st <- get
   ca <- fmap ContAddr $ liftIO $ newIORef st
   push_arg ca 0
   push_code code

-- | Jump to the continuation referenced by the symbol in the current object
jump_obj :: [ Symbol ] -> DelveM ( )
jump_obj path = do
   o <- get_current_object_ref
   cio <- fmap get_cont $ find_addr path $ ObjAddr o
   c <- liftIO $ readIORef cio
   r <- get_result
   put c
   put_result r


-- | Initialize the state of the virtual machine
new_delve_state :: Code -> IO DState
new_delve_state c = do
   act_stack <- new_activation_stack
   arg_group <- new_arg_group 255
   return $ DState {
               activation_stack = act_stack
            ,  result = Nothing
            ,  code = c
            ,  args = arg_group
            }

-- | Create a new activation stack
new_activation_stack :: IO ActivationStack
new_activation_stack = do
   os <- new_object_stack
   le <- new_local_environment_stack
   let act_ref = ActivationFrame os le
   return [ act_ref ]

-- | Create a new object stack
new_object_stack :: IO ObjectStack
new_object_stack = do
   o_ref <- new_object
   return [ o_ref ]

-- | create a new object
new_object :: IO ( IORef Object )
new_object = do
   let new_o = Object M.empty
   newIORef new_o

-- | create a new local environment
new_local_environment :: IO ( IORef LocalEnv )
new_local_environment = do 
   let new_env = LocalEnv M.empty
   newIORef new_env


-- | create a new local environment stack
new_local_environment_stack :: IO LocalEnvStack
new_local_environment_stack = do
   env_ref <- new_local_environment
   return [ env_ref ]


-- | Evaluate Delve
eval :: DelveM ( )
eval = do
   me <- pop_expression
   maybe ( return ( ) )
         ( \ e -> do
      --      liftIO $ putStrLn $ "expr: " L.++ show e
            eval_expr e
      --      liftIO $ putStrLn "evaluated"
            eval
         ) 
         me

-- | Pop an expression from the code
pop_expression :: DelveM ( Maybe Expr )
pop_expression = do
   rst <- get
   case code rst of
      [ ] -> return Nothing
      ( c : rest ) -> do
         put $ rst { code = rest }
         return $ Just c

-- | The address of a new object
new_object_addr :: DelveM Addr
new_object_addr =
   fmap ObjAddr $ liftIO new_object

-- | Get the address referenced by the symbol in the current object
-- | trigger a fatal error otherwise ( Delve is well-typed, so this should never happen unless you're hacking the VM )
lookup_current_object :: [ Symbol ] -> DelveM Addr
lookup_current_object path@( _ : _ ) = do
   o <- get_current_object_ref
--   liftIO $ putStrLn $ "Looking up: " L.++ show path
   find_addr path $ ObjAddr o
lookup_current_object _ =
   delve_error "Tried to look up current object with no path"

-- | Find the symbol in the object, looking up the superclass if it is not in the subclass
find_obj_sym :: Object -> Symbol -> DelveM Addr
find_obj_sym o s = do
 --  liftIO $ putStrLn $ "find_obj_sym: " L.++ show s
 --  liftIO $ putStrLn $ "members of object: " L.++ show ( M.keys $ members o )
--   let a = irrefutible_lookup s $ members o
--   case a of
--      ObjAddr _  -> liftIO $ putStrLn "Aaaargh!"
--      CodeAddr _ -> liftIO $ putStrLn "yay"
   maybe ( do let clsio = get_object $ irrefutible_lookup class_sym $ members o
              cls <- liftIO $ readIORef clsio
              search_classes cls s
         )
         return
         $ M.lookup s ( members o )

-- The symbol for \"class\"
class_sym :: Symbol 
class_sym = B.pack "class"

-- | An irrefutible symbol lookup in a map
irrefutible_lookup :: Symbol -> Members -> Addr
irrefutible_lookup s m =
   fromMaybe ( symbol_error s )
             $ M.lookup s m

-- | Search a class and its superclasses for a symbol
search_classes :: Object -> Symbol -> DelveM Addr
search_classes c s = do
 --  liftIO $ putStrLn $ "members of class: " L.++ show ( M.keys $ members c )
   let insio = get_object $ irrefutible_lookup methods_sym $ members c
   ins <- liftIO $ readIORef insio
   maybe find_in_parent
         return
         $ M.lookup s $ members ins
   where 
   methods_sym = B.pack "instance_methods"
   super_sym = B.pack "super"
   find_in_parent = do
      let superio = get_object $ irrefutible_lookup super_sym $ members c
      super <- liftIO $ readIORef superio
      search_classes super s 

-- | Display an error message
delve_error msg =
   error $ "The possible happened!\n" L.++ msg L.++ "\nUnless you were hacking on the Delve Virtual Machine, please submit the code that caused this as a bug report to <spoon@killersmurf.com>"

-- | Symbol error
symbol_error :: Symbol -> a
symbol_error sym = 
   delve_error $ "Delve tried to look up '" L.++ B.unpack sym L.++ "', which wasn't there." 

-- | Get the local environment stack
get_local_env :: DelveM LocalEnvStack
get_local_env = do
   ActivationFrame _ local_env_stack <- get_activation_frame 
   return local_env_stack   

-- | Get the current activation frame
get_activation_frame :: DelveM ActivationFrame
get_activation_frame = do
   rst <- get
   return $ L.head $ activation_stack rst

-- | Get the current object
get_current_object :: DelveM Object
get_current_object = do
   cobject_ref <- get_current_object_ref
   liftIO $ readIORef cobject_ref

-- | Get a reference to the current object
get_current_object_ref :: DelveM ( IORef Object )
get_current_object_ref = do   
   ActivationFrame object_stack _ <- get_activation_frame
   return $ L.head object_stack

-- create a new code block wrapped in a reference
new_code_block :: Code -> DelveM ( IORef CodeBlock )
new_code_block =
   liftIO . newIORef . CodeBlock ( MemoryFrame Nothing Nothing ) 

-- create an address for a block of code
new_code_addr :: Code -> DelveM Addr
new_code_addr =
   fmap CodeAddr . new_code_block

-- create an address for a new primative value
new_prim_addr :: Prim -> DelveM Addr
new_prim_addr =
   fmap PrimAddr . liftIO . newIORef  
 
-- get the referece to a code block from an address
get_code_block :: Addr -> IORef CodeBlock
get_code_block a =
   case a of
      CodeAddr cb -> cb
      _ ->           delve_error "Could not find code at address"

-- get the current object stack
get_objects :: DelveM ObjectStack
get_objects = do
   ActivationFrame os _ <- get_activation_frame
   return os

-- add a local environment to a memory frame
add_local_frame :: LocalEnvStack -> CodeBlock -> CodeBlock
add_local_frame !les !( CodeBlock ( MemoryFrame os _ ) code ) =
   CodeBlock ( MemoryFrame os $ Just les ) code

-- add an object stack to a memory frame
add_object_frame :: IORef Object -> CodeBlock -> CodeBlock 
add_object_frame !os !( CodeBlock ( MemoryFrame _ les ) code ) =
   CodeBlock ( MemoryFrame ( Just os ) les ) code

-- set the activation frame on an address to remember the current objects
remember_objects :: Addr -> DelveM ( )
remember_objects a = do
   let f = get_code_block a
   obj <- fmap L.head get_objects
   liftIO $ modifyIORef f ( add_object_frame obj )

-- get the local environment stack
get_local_stack :: DelveM LocalEnvStack
get_local_stack = do
   ActivationFrame _ les <- get_activation_frame
   return les

-- set the activation frame on an address to remember the current local scope
remember_local :: Addr -> DelveM ( )
remember_local a = do
   let f = get_code_block a
   frame <- get_local_stack
   liftIO $ modifyIORef f $ add_local_frame frame

-- get the memory frame associated with an address
get_address_frame :: Addr -> DelveM MemoryFrame
get_address_frame a = do
   let iof = get_code_block a
   CodeBlock fr _ <- liftIO $ readIORef iof
   return fr

-- index a list safely
safe_index l i =
   if L.length l <= i
      then Nothing
      else Just $ l L.!! i 

-- push a new activation frame to the top of the activation frame stack
-- use the given activation frame there when trying to remember stuff the memory frame has not noted
push_activation_frame :: ActivationFrame -> MemoryFrame -> DelveM ( )
push_activation_frame (ActivationFrame last_os last_les ) ( MemoryFrame mos mles )  = do
   let mfr = ( do os <- mos
                  les <- mles
                  Just $ ActivationFrame [ os ] les ) <|> ( do os <- mos
                                                               Just $ ActivationFrame [ os ] last_les ) <|> ( do les <- mles
                                                                                                                 Just $ ActivationFrame [ L.head $ last_os ] les )
       f = fromMaybe id $ mfr >>= ( \ fr -> Just ( \ stack -> fr : stack ) )
       
   modify $ \ dst -> dst { activation_stack = f $ activation_stack dst }

-- push an address onto the argument stack at the given index
push_arg :: Addr -> Word8 -> DelveM ( )
push_arg !a !w = do
   arr <- fmap args get
   liftIO $ writeArray arr w a 

-- assign a symbol in the local environment to refer to an address
assign_local :: IORef LocalEnv -> [ Symbol ] -> Symbol -> Addr -> DelveM ( )
assign_local leio path sym addr = do
   eenva <- find_addr_local leio path
 {-  liftIO $ either ( const $ return ( ) )
                   ( \ a -> case a of
                      ObjAddr _  -> putStrLn "It's an obj"
                      CodeAddr _ -> putStrLn "It's code"
                      _          -> putStrLn "Some other shit"
                   ) eenva -}
           
   liftIO $ either ( \ ! envio -> modifyIORef envio $ \ ! le ->  LocalEnv $ M.insert sym addr $ env le )
                   ( ( \ ! obio -> modifyIORef obio $ \ ! o -> Object $ M.insert sym addr $ members o ) . get_object )
                   eenva

-- find an object from  a path of object references, beginning in the local scope, or returning the local scope if there is no path
find_addr_local :: IORef LocalEnv -> [ Symbol ] -> DelveM ( Either ( IORef LocalEnv ) Addr )
find_addr_local leio [ ] = do
   --liftIO $ B.putStrLn $ B.pack "lookup up symbol: " `B.append` sym
   return $ Left leio 
find_addr_local leio ( s : ss ) = do
   le <- liftIO $ readIORef leio
   let ob = irrefutible_lookup s $ env le
   if L.null ss
      then return $ Right ob
      else do
         r <- find_addr ss ob
         return $ Right r


-- assign a symbol to refer to an address in the object refered to by the path
assign_object :: [ Symbol ] -> Symbol -> Addr -> DelveM ( )
assign_object path sym addr = do
   ActivationFrame ( oio : _ ) _ <- get_activation_frame
   obio <- fmap get_object $ find_addr path $ ObjAddr oio
   liftIO $ modifyIORef obio $ ( \ ! o -> Object $ M.insert sym addr $ members o )

-- find the object at the end of the path of object references
find_addr :: [ Symbol ] -> Addr -> DelveM Addr
find_addr ( s : ss ) addr = do
   o <- liftIO $ readIORef $ get_object addr
--   liftIO $ putStrLn "Finding addr"
   new_addr <- find_obj_sym o s
   find_addr ss new_addr
find_addr _ addr = do
--   liftIO $ putStrLn "Not bothering"
   return addr

-- get the last result, triggering an error if it has not been set
get_result :: DelveM Addr
get_result = do
   rst <- get
   case result rst of
      !mr -> return $ fromMaybe ( delve_error "Result was not set" )
                                mr

-- get an object from an address
get_object :: Addr -> IORef Object
get_object a =
   case a of
      ObjAddr o -> o
      _         -> delve_error "Address did not point to an object"

-- get a primative from an address
get_prim :: Addr -> IORef Prim
get_prim a =
    case a of
       PrimAddr p -> p
       _          -> delve_error "Address did not point to a primative"

-- push an object onto the object stack so it becomes the current object
push_object :: IORef Object -> DelveM ( )
push_object o =
   modify_frame $ \ fr -> fr { object_stack = o : object_stack fr }

-- change the current top activation frame
modify_frame :: ( ActivationFrame -> ActivationFrame ) -> DelveM ( )
modify_frame !f = do
   dst <- get
   let stack = activation_stack dst
   put $ dst { activation_stack = ( f $ L.head stack ) : L.tail stack }

-- pop an object from the top of the object stack
pop_object :: DelveM ( )
pop_object = do
   modify_frame $ \ fr -> fr { object_stack = L.tail $ object_stack fr }

-- add a piece of code from a code block into the machine
push_code_block :: CodeBlock -> DelveM ( )
push_code_block ( CodeBlock _ new_code ) =
   push_code new_code

-- add a piece of code into the machine
push_code :: Code -> DelveM ( )
push_code new_code = do
   dst <- get
   put $ dst { code = new_code L.++ code dst }

-- push an empty local environment into the local environment stack
push_local :: DelveM ( )
push_local = do
   new_env <- liftIO $ new_local_environment
   modify_frame $ \ ! fr -> fr { local_env = new_env : local_env fr }

-- pop a local environment from the local environment stack
pop_local :: DelveM ( )
pop_local =
   modify_frame $ \ ! fr -> fr { local_env = L.tail $ local_env fr }

-- Match the symbol given to one of the alternatives, and push the matched code onto the code stack
match :: Symbol -> Alternatives -> DelveM ( )
match !sym !alts = do
   let !m_match = L.find ( ( == ) sym . fst ) alts
       default_match = L.find ( (==) ( B.pack "default" ) . fst ) alts
       new_code = fromMaybe ( delve_error "No case alternative matches." )
                            $ ( m_match >>= Just . snd ) <|> ( default_match >>= Just . snd )
   push_code new_code
   
-- Get the symbol reference from an address
get_symbol :: Addr -> IORef Symbol
get_symbol a =
   case a of
      SymbolAddr s -> s
      _            -> delve_error "Address did not point to a symbol"

-- set the result to an address
put_result :: Addr -> DelveM ( )
put_result a = do
   dst <- get
   put $ dst { result = Just a }

-- get the argument at the index
get_arg :: Word8 -> DelveM Addr
get_arg i = do
   arg_group <- fmap args get
   liftIO $ E.catch ( readArray arg_group i ) 
                    argument_exception
   where
   argument_exception :: ArrayException -> IO Addr
   argument_exception = const $ delve_error "Function had less arguments than expected"

get_current_env :: DelveM ( IORef LocalEnv )
get_current_env =
   fmap ( L.head . local_env . L.head . activation_stack ) get
 
-- Error when a path is not provided, when attempting to remember local data
local_memory_error =
   delve_error "Local environment could not be saved as the target object was non-existant"

-- pop the current activation frame from the stack
pop_activation_frame :: DelveM ActivationFrame
pop_activation_frame = do
   rst <- get
   put $ rst { activation_stack = L.tail $ activation_stack rst }
   return $ L.head $ activation_stack rst

-- a new empty argument group
new_arg_group :: Word8 -> IO ArgGroup
new_arg_group size =
   newArray_ ( 0 , size - 1 )

-- get the continuation from an address
get_cont :: Addr -> IORef Continuation
get_cont a =
   case a of
      ContAddr c -> c
      _          -> delve_error "Address did not point to a continuation"

-- find a variable in the environment at this depth
lookup_local_env :: Int -> [ Symbol ] -> DelveM Addr
lookup_local_env depth ( sym : path ) = do
  envio <- get_environment_at depth
  envi <- liftIO $ readIORef envio
  let target = irrefutible_lookup sym $ env envi
  find_addr path target
lookup_local_env _ _ =
   delve_error "Tried to perform lookup in local environment with non-existant path"

-- get the environment at the given depth on the environment stack
get_environment_at :: Int -> DelveM ( IORef LocalEnv )
get_environment_at depth = do
   les <- get_local_env
   let mev = safe_index les depth
       ev = fromMaybe ( delve_error "Tried to look up variable in non-existant local environment" ) mev
   return ev

-- search the environment for the variable described by the path, fail if it is not found
search_environment :: IORef LocalEnv -> [ Symbol ] -> DelveM Addr
search_environment e path = do
   ela <- find_addr_local e path
   either ( const $ symbol_error $ render_path path )
          return
          $ ela



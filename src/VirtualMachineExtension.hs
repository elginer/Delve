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

module VirtualMachineExtension where
import Data.ByteString.Char8
import DMachineState
import DHelper
import DelveVM
import Data.Map as M
import DHelper
import DHelper
import Foreign.Marshal.Alloc
import Foreign.Ptr
import DHelper
import DelveVM
import Data.ByteString.Char8 as B
import qualified Data.Map as M
bytecode
  = [NewBlock [MachineInstruction _V_n],
     AssignLocal 0 [] (pack "stats"),
     NewBlock [MachineInstruction _V_o],
     AssignLocal 0 [] (pack "current"),
     NewBlock [MachineInstruction _V_p],
     AssignLocal 0 [] (pack "print_obj"),
     NewBlock [MachineInstruction _V_q],
     AssignLocal 0 [] (pack "mk_obj"), CallLocal 0 [(pack "mk_obj")],
     AssignLocal 0 [] (pack "Object"), RefLocal 0 [(pack "mk_obj")],
     AssignLocal 0 [(pack "Object")] (pack "new"),
     LoadLocal 0 [(pack "Object")], CallObj [(pack "new")], PopObject,
     AssignLocal 0 [] (pack "Symbol"), LoadLocal 0 [(pack "Object")],
     CallObj [(pack "new")], PopObject,
     AssignLocal 0 [(pack "Symbol")] (pack "instance_methods"),
     NewBlock
       [PushLocal, WriteArg 0 (pack "s"), RefLocal 0 [(pack "s")],
        AssignObject [] (pack "prim"), PopFrame],
     AssignLocal 0 [] (pack "_V_ac"),
     RememberLocalLocal 0 [(pack "_V_ac")],
     AssignLocal
       0 [(pack "Symbol"), (pack "instance_methods")] (pack "initialize"),
     NewBlock
       [PushLocal, NewBlock [MachineInstruction _V_r],
        AssignLocal 0 [] (pack "_V_m"), PushObjArg [(pack "prim")] 0,
        FrameTailCallLocal 0 [(pack "_V_m")]],
     AssignLocal 0 [] (pack "_V_ad"),
     RememberLocalLocal 0 [(pack "_V_ad")],
     AssignLocal
       0 [(pack "Symbol"), (pack "instance_methods")] (pack "print"),
     NewBlock
       [PushLocal, WriteArg 0 (pack "x"), LoadLocal 1 [(pack "Object")],
        CallObj [(pack "new")], PopObject,
        AssignLocal 0 [] (pack "instance"), RefSelf,
        AssignLocal 0 [(pack "instance")] (pack "class"),
        PushLocalArg 0 [(pack "x")] 0, LoadLocal 0 [(pack "instance")],
        CallObj [(pack "initialize")], PopObject,
        RefLocal 0 [(pack "instance")], PopFrame],
     AssignLocal 0 [] (pack "_V_ae"),
     RememberLocalLocal 0 [(pack "_V_ae")],
     AssignLocal 0 [(pack "Symbol")] (pack "new"),
     LoadLocal 0 [(pack "Object")], CallObj [(pack "new")], PopObject,
     AssignLocal 0 [] (pack "Bool"), LoadLocal 0 [(pack "Object")],
     CallObj [(pack "new")], PopObject,
     AssignLocal 0 [(pack "Bool")] (pack "instance_methods"),
     NewBlock
       [PushLocal, WriteArg 0 (pack "sym"), LoadLocal 1 [(pack "Object")],
        CallObj [(pack "new")], PopObject,
        AssignLocal 0 [] (pack "instance"), RefSelf,
        AssignLocal 0 [(pack "instance")] (pack "class"),
        PushLocalArg 0 [(pack "sym")] 0, LoadLocal 1 [(pack "Symbol")],
        CallObj [(pack "new")], PopObject, AssignLocal 0 [] (pack "_V_l"),
        PushLocalArg 0 [(pack "_V_l")] 0, LoadLocal 0 [(pack "instance")],
        CallObj [(pack "initialize")], PopObject,
        RefLocal 0 [(pack "instance")], PopFrame],
     AssignLocal 0 [] (pack "_V_af"),
     RememberLocalLocal 0 [(pack "_V_af")],
     AssignLocal 0 [(pack "Bool")] (pack "new"),
     NewBlock
       [PushLocal, WriteArg 0 (pack "sym"), RefLocal 0 [(pack "sym")],
        AssignObject [] (pack "sym"), PopFrame],
     AssignLocal 0 [] (pack "_V_ag"),
     RememberLocalLocal 0 [(pack "_V_ag")],
     AssignLocal
       0 [(pack "Bool"), (pack "instance_methods")] (pack "initialize"),
     NewSymbol (pack "False"), AssignLocal 0 [] (pack "_V_k"),
     PushLocalArg 0 [(pack "_V_k")] 0, LoadLocal 0 [(pack "Bool")],
     CallObj [(pack "new")], PopObject, AssignLocal 0 [] (pack "False"),
     NewSymbol (pack "True"), AssignLocal 0 [] (pack "_V_j"),
     PushLocalArg 0 [(pack "_V_j")] 0, LoadLocal 0 [(pack "Bool")],
     CallObj [(pack "new")], PopObject, AssignLocal 0 [] (pack "True"),
     NewBlock
       [PushLocal,
        MatchObj [(pack "sym"), (pack "prim")]
          [((pack "True"),
            [PushLocal, RefLocal 2 [(pack "False")], PopFrame]),
           ((pack "False"),
            [PushLocal, RefLocal 2 [(pack "True")], PopFrame])]],
     AssignLocal 0 [] (pack "_V_ah"),
     RememberLocalLocal 0 [(pack "_V_ah")],
     AssignLocal
       0 [(pack "Bool"), (pack "instance_methods")] (pack "not"),
     NewBlock
       [PushLocal, WriteArg 0 (pack "other"),
        MatchObj [(pack "sym"), (pack "prim")]
          [((pack "True"),
            [PushLocal,
             MatchLocal 1 [(pack "other"), (pack "sym"), (pack "prim")]
               [((pack "True"),
                 [PushLocal, RefLocal 3 [(pack "False")], PopFrame]),
                ((pack "False"),
                 [PushLocal, RefLocal 3 [(pack "True")], PopFrame])]]),
           ((pack "False"),
            [PushLocal, RefLocal 2 [(pack "True")], PopFrame])]],
     AssignLocal 0 [] (pack "_V_ai"),
     RememberLocalLocal 0 [(pack "_V_ai")],
     AssignLocal
       0 [(pack "Bool"), (pack "instance_methods")] (pack "nand"),
     NewBlock
       [PushLocal, WriteArg 0 (pack "other"),
        PushLocalArg 0 [(pack "other")] 0, CallObj [(pack "nand")],
        AssignLocal 0 [] (pack "n"), LoadLocal 0 [(pack "n")],
        FrameTailCallObj [(pack "not")]],
     AssignLocal 0 [] (pack "_V_aj"),
     RememberLocalLocal 0 [(pack "_V_aj")],
     AssignLocal
       0 [(pack "Bool"), (pack "instance_methods")] (pack "and"),
     NewBlock
       [PushLocal, WriteArg 0 (pack "q"),
        PushLocalArg 1 [(pack "True")] 0, CallObj [(pack "nand")],
        AssignLocal 0 [] (pack "ntp"), PushLocalArg 1 [(pack "True")] 0,
        LoadLocal 0 [(pack "q")], CallObj [(pack "nand")], PopObject,
        AssignLocal 0 [] (pack "ntq"), PushLocalArg 0 [(pack "ntq")] 0,
        LoadLocal 0 [(pack "ntp")], FrameTailCallObj [(pack "nand")]],
     AssignLocal 0 [] (pack "_V_ak"),
     RememberLocalLocal 0 [(pack "_V_ak")],
     AssignLocal
       0 [(pack "Bool"), (pack "instance_methods")] (pack "or"),
     NewBlock
       [PushLocal, LoadObj [(pack "sym")],
        FrameTailCallObj [(pack "print")]],
     AssignLocal 0 [] (pack "_V_al"),
     RememberLocalLocal 0 [(pack "_V_al")],
     AssignLocal
       0 [(pack "Bool"), (pack "instance_methods")] (pack "print"),
     LoadLocal 0 [(pack "Object")], CallObj [(pack "new")], PopObject,
     AssignLocal 0 [] (pack "Int"), LoadLocal 0 [(pack "Object")],
     CallObj [(pack "new")], PopObject,
     AssignLocal 0 [(pack "Int")] (pack "instance_methods"),
     NewBlock
       [PushLocal, WriteArg 0 (pack "prim"), RefLocal 0 [(pack "prim")],
        AssignObject [] (pack "prim"), PopFrame],
     AssignLocal 0 [] (pack "_V_am"),
     RememberLocalLocal 0 [(pack "_V_am")],
     AssignLocal
       0 [(pack "Int"), (pack "instance_methods")] (pack "initialize"),
     NewBlock
       [PushLocal, WriteArg 0 (pack "other"),
        NewBlock [MachineInstruction _V_s],
        AssignLocal 0 [] (pack "machine_addition"),
        PushObjArg [(pack "prim")] 0,
        PushLocalArg 0 [(pack "other"), (pack "prim")] 1,
        CallLocal 0 [(pack "machine_addition")],
        AssignLocal 0 [] (pack "p"), PushLocalArg 0 [(pack "p")] 0,
        LoadLocal 1 [(pack "Int")], FrameTailCallObj [(pack "new")]],
     AssignLocal 0 [] (pack "_V_an"),
     RememberLocalLocal 0 [(pack "_V_an")],
     AssignLocal 0 [(pack "Int"), (pack "instance_methods")] (pack "+"),
     NewBlock
       [PushLocal, WriteArg 0 (pack "other"),
        NewBlock [MachineInstruction _V_t],
        AssignLocal 0 [] (pack "machine_subtraction"),
        PushObjArg [(pack "prim")] 0,
        PushLocalArg 0 [(pack "other"), (pack "prim")] 1,
        CallLocal 0 [(pack "machine_subtraction")],
        AssignLocal 0 [] (pack "_V_i"), PushLocalArg 0 [(pack "_V_i")] 0,
        LoadLocal 1 [(pack "Int")], FrameTailCallObj [(pack "new")]],
     AssignLocal 0 [] (pack "_V_ao"),
     RememberLocalLocal 0 [(pack "_V_ao")],
     AssignLocal 0 [(pack "Int"), (pack "instance_methods")] (pack "-"),
     NewBlock
       [PushLocal, WriteArg 0 (pack "other"),
        NewBlock [MachineInstruction _V_u],
        AssignLocal 0 [] (pack "machine_lte"),
        PushObjArg [(pack "prim")] 0,
        PushLocalArg 0 [(pack "other"), (pack "prim")] 1,
        CallLocal 0 [(pack "machine_lte")], AssignLocal 0 [] (pack "_V_h"),
        PushLocalArg 0 [(pack "_V_h")] 0, LoadLocal 1 [(pack "Bool")],
        FrameTailCallObj [(pack "new")]],
     AssignLocal 0 [] (pack "_V_ap"),
     RememberLocalLocal 0 [(pack "_V_ap")],
     AssignLocal
       0 [(pack "Int"), (pack "instance_methods")] (pack "<="),
     NewBlock
       [PushLocal, WriteArg 0 (pack "other"),
        NewBlock [MachineInstruction _V_v],
        AssignLocal 0 [] (pack "machine_eq"), PushObjArg [(pack "prim")] 0,
        PushLocalArg 0 [(pack "other"), (pack "prim")] 1,
        CallLocal 0 [(pack "machine_eq")], AssignLocal 0 [] (pack "_V_g"),
        PushLocalArg 0 [(pack "_V_g")] 0, LoadLocal 1 [(pack "Bool")],
        FrameTailCallObj [(pack "new")]],
     AssignLocal 0 [] (pack "_V_aq"),
     RememberLocalLocal 0 [(pack "_V_aq")],
     AssignLocal 0 [(pack "Int"), (pack "instance_methods")] (pack "="),
     NewBlock
       [PushLocal, WriteArg 0 (pack "other"),
        PushLocalArg 0 [(pack "other")] 0, CallObj [(pack "<=")],
        AssignLocal 0 [] (pack "lte"), LoadLocal 0 [(pack "lte")],
        FrameTailCallObj [(pack "not")]],
     AssignLocal 0 [] (pack "_V_ar"),
     RememberLocalLocal 0 [(pack "_V_ar")],
     AssignLocal 0 [(pack "Int"), (pack "instance_methods")] (pack ">"),
     NewBlock
       [PushLocal, WriteArg 0 (pack "other"),
        PushLocalArg 0 [(pack "other")] 0, CallObj [(pack ">")],
        AssignLocal 0 [] (pack "gt"), PushLocalArg 0 [(pack "other")] 0,
        CallObj [(pack "=")], AssignLocal 0 [] (pack "_V_f"),
        PushLocalArg 0 [(pack "_V_f")] 0, LoadLocal 0 [(pack "gt")],
        FrameTailCallObj [(pack "or")]],
     AssignLocal 0 [] (pack "_V_as"),
     RememberLocalLocal 0 [(pack "_V_as")],
     AssignLocal
       0 [(pack "Int"), (pack "instance_methods")] (pack ">="),
     NewBlock
       [PushLocal, WriteArg 0 (pack "other"),
        PushLocalArg 0 [(pack "other")] 0, CallObj [(pack "<=")],
        AssignLocal 0 [] (pack "lte"), PushLocalArg 0 [(pack "other")] 0,
        CallObj [(pack "=")], AssignLocal 0 [] (pack "eq"),
        LoadLocal 0 [(pack "eq")], CallObj [(pack "not")], PopObject,
        AssignLocal 0 [] (pack "_V_e"), PushLocalArg 0 [(pack "_V_e")] 0,
        LoadLocal 0 [(pack "lte")], FrameTailCallObj [(pack "and")]],
     AssignLocal 0 [] (pack "_V_at"),
     RememberLocalLocal 0 [(pack "_V_at")],
     AssignLocal 0 [(pack "Int"), (pack "instance_methods")] (pack "<"),
     NewBlock
       [PushLocal, WriteArg 0 (pack "other"),
        NewBlock [MachineInstruction _V_w], AssignLocal 0 [] (pack "_V_d"),
        PushObjArg [(pack "prim")] 0,
        PushLocalArg 0 [(pack "other"), (pack "prim")] 1,
        FrameTailCallLocal 0 [(pack "_V_d")]],
     AssignLocal 0 [] (pack "_V_au"),
     RememberLocalLocal 0 [(pack "_V_au")],
     AssignLocal 0 [(pack "Int"), (pack "instance_methods")] (pack "*"),
     NewBlock
       [PushLocal, WriteArg 0 (pack "other"),
        NewBlock [MachineInstruction _V_x], AssignLocal 0 [] (pack "_V_c"),
        PushObjArg [(pack "prim")] 0,
        PushLocalArg 0 [(pack "other"), (pack "prim")] 1,
        FrameTailCallLocal 0 [(pack "_V_c")]],
     AssignLocal 0 [] (pack "_V_av"),
     RememberLocalLocal 0 [(pack "_V_av")],
     AssignLocal 0 [(pack "Int"), (pack "instance_methods")] (pack "/"),
     NewBlock
       [PushLocal, WriteArg 0 (pack "other"),
        NewBlock [MachineInstruction _V_y], AssignLocal 0 [] (pack "_V_b"),
        PushObjArg [(pack "prim")] 0,
        PushLocalArg 0 [(pack "other"), (pack "prim")] 1,
        FrameTailCallLocal 0 [(pack "_V_b")]],
     AssignLocal 0 [] (pack "_V_aw"),
     RememberLocalLocal 0 [(pack "_V_aw")],
     AssignLocal 0 [(pack "Int"), (pack "instance_methods")] (pack "^"),
     NewBlock
       [PushLocal, NewBlock [MachineInstruction _V_z],
        AssignLocal 0 [] (pack "machine_print"),
        PushObjArg [(pack "prim")] 0,
        FrameTailCallLocal 0 [(pack "machine_print")]],
     AssignLocal 0 [] (pack "_V_ax"),
     RememberLocalLocal 0 [(pack "_V_ax")],
     AssignLocal
       0 [(pack "Int"), (pack "instance_methods")] (pack "print"),
     NewBlock
       [PushLocal, WriteArg 0 (pack "prim"),
        LoadLocal 1 [(pack "Object")], CallObj [(pack "new")], PopObject,
        AssignLocal 0 [] (pack "instance"), RefSelf,
        AssignLocal 0 [(pack "instance")] (pack "class"),
        PushLocalArg 0 [(pack "prim")] 0, LoadLocal 0 [(pack "instance")],
        CallObj [(pack "initialize")], PopObject,
        RefLocal 0 [(pack "instance")], PopFrame],
     AssignLocal 0 [] (pack "_V_ay"),
     RememberLocalLocal 0 [(pack "_V_ay")],
     AssignLocal 0 [(pack "Int")] (pack "new"),
     NewBlock
       [PushLocal, WriteArg 0 (pack "exec"),
        NewBlock [MachineInstruction _V_aa],
        AssignLocal 0 [] (pack "machine_call/cc"),
        NewBlock [MachineInstruction _V_ab],
        AssignLocal 0 [] (pack "_V_a"), CallLocal 0 [(pack "_V_a")],
        PushLocalArg 0 [(pack "exec")] 0,
        FrameTailCallLocal 0 [(pack "machine_call/cc")]],
     AssignLocal 0 [] (pack "_V_az"),
     RememberLocalLocal 0 [(pack "_V_az")],
     RememberObjLocal 0 [(pack "_V_az")],
     AssignLocal 0 [] (pack "call/cc")]
_V_n = stats
_V_o
  = do o <- get_current_object
       liftIO $ Prelude.putStrLn "Current object:"
       liftIO $ Prelude.putStrLn $ show $ M.keys $ members o
_V_p
  = do a <- get_arg 0
       liftIO $
         do o <- readIORef $ get_object a
            Prelude.putStrLn "An Object:"
            Prelude.putStrLn $ show $ M.keys $ members o
_V_q = new
_V_r = print_sym
_V_s = add_ints
_V_t = subtract_ints
_V_u = primative_lte
_V_v = primative_eq
_V_w = primative_mult
_V_x = primative_div
_V_y = primative_pow
_V_z = print_int
_V_aa
  = let jumper = B.singleton 'j'
        cont = B.singleton 'c'
        result = B.singleton 'r'
        exec = B.singleton 'e'
        call
          = [PushLocal, WriteArg 0 exec,
             CallCC
               [PushLocal, WriteArg 0 cont,
                NewBlock
                  [WriteArg 0 result, RefLocal 0 [result], JumpLocal 0 [cont]],
                AssignLocal 0 [] jumper, RememberLocalLocal 0 [jumper],
                PushLocalArg 0 [jumper] 0, LocalTailCallLocal False 1 [exec]],
             PopLocal]
      in push_code $! call
_V_ab = stats
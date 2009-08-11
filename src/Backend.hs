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

{-# OPTIONS
    -XEmptyDataDecls
    -XMultiParamTypeClasses
    -XFlexibleInstances
    -XFlexibleContexts
#-}
-- Things shared between backends
-- There is still a bit of boiler-plate duplication as far as the different back-ends go
-- Maybe it would be better to generalize the whole process.
-- This should probably be done, but isn't high priority.
module Backend where

import LocalCore
import Util
import Symbol

import Data.Monoid
import Data.List.Stream as L

import Control.Monad.Stream as C

import Control.Applicative

-- The optimization class
-- Each backend handles tail call optimization individually, but here we can specify the correct popping mechanism taking into account the optimization
class Optimization o t where
   oppop :: ( Alternative a , Monoid ( a t ) ) => o -> a t

-- A tail call optimization which works on activation frames
data Frame
-- A tail call optimization which works on the local scpe
data Local
-- Non-optimized code - this code is not tail-called
data None

class ( Optimization o t , Monad m ) => Backend o s m t where
   backend :: o -> s -> m [ t ]

-- easy aliasing of various optimization strategies
frame :: Frame
frame = undefined

local :: Local
local = undefined

none :: None
none = undefined

-- The first function is applied to a local variable, the second to an object variable 
on_var :: LVar -> ( Int -> [ Symbol ] -> a ) -> ( [ Symbol ] -> a ) -> a
on_var v lf vof =
   case v of
      LLocalVar i p -> lf i p
      LObjVar p     -> vof p

-- analyze a block of code to optimize the tail call
-- Worried about this being very slow
tail_analyze_block :: ( Functor m , Backend None s m t , Backend o s m t ) => o -> [ s ] -> m [ t ]
tail_analyze_block o lcore = do 
   lst_code <- backend o $ L.last lcore 
   fmap ( flip (L.++) lst_code . L.concat ) ( C.mapM ( backend none ) $ drop_lst lcore )

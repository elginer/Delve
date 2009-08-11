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

-- Reading and writing .dcode and .dui files
module DelveFormats 
   ( module SerializeUtil
   , module DMachineState
   , to_dcode
   , from_dcode
   , to_dui
   , from_dui
   ) 
   where

import SerializeUtil
import DMachineState

import Data.ByteString.Lazy as Z
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

-- Encode a dui file
to_dui :: [ Symbol ] -> Word64 -> Z.ByteString
to_dui syms w =
    runPut $ do
        put w
        put $ DList syms 

-- Decode a dui file
from_dui :: Z.ByteString -> ( [ Symbol ] , Word64 )
from_dui = do
   runGet $ do
      w <- get
      DList syms <- get
      return $ ( syms , w )

-- Decode a dcode file
from_dcode :: Z.ByteString -> Code
from_dcode = runGet li_get

-- Encode a dcode file
to_dcode :: Code -> Z.ByteString
to_dcode c = runPut $ lput c

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.CodeGen.Stencil.DIM2
-- Copyright   : [2014] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.CodeGen.Stencil.DIM2
  where

-- accelerate
import Data.Array.Accelerate.AST                                ( Stencil )
import Data.Array.Accelerate.Array.Sugar                        ( Array, Elt, DIM2 )
import Data.Array.Accelerate.Type                               ( Boundary(..) )

import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.CodeGen.Monad

import Data.Array.Accelerate.LLVM.Native.CodeGen.Base
import Data.Array.Accelerate.LLVM.Native.CodeGen.Loop



-- Map a stencil over an array. In contrast to 'map', the domain of a stencil
-- function is an entire /neighbourhood/ of each array element. Neighbourhoods
-- are sub-arrays centred around a focal point. They are not necessarily
-- rectangular, but they are symmetric and have an extent of at least three in
-- each dimensions. Due to this symmetry requirement, the extent is necessarily
-- odd. The focal point is the array position that determines the single output
-- element for each application of the stencil.
--
-- For those array positions where the neighbourhood extends past the boundaries
-- of the source array, a boundary condition determines the contents of the
-- out-of-bounds neighbourhood positions.
--
-- stencil :: (Shape sh, Elt a, Elt b, Stencil sh a stencil)
--         => (stencil -> Exp b)                 -- stencil function
--         -> Boundary a                         -- boundary condition
--         -> Acc (Array sh a)                   -- source array
--         -> Acc (Array sh b)                   -- destination array
--
mkStencil
    :: forall native aenv stencil a b. (Stencil DIM2 a stencil, Elt b)
    => Gamma aenv
    -> IRFun1    aenv (stencil -> b)
    -> IRDelayed aenv (Array DIM2 a)
    -> Boundary (IRExp aenv a)
    -> CodeGen [Kernel native aenv (Array DIM2 b)]
mkStencil aenv apply IRDelayed{..} boundary =
  let
      (start, end, paramGang)   = gangParam
      arrOut                    = arrayData  (undefined::Array DIM2 b) "out"
      paramOut                  = arrayParam (undefined::Array DIM2 b) "out"
      paramEnv                  = envParam aenv
  in
  makeKernel "stencil" (paramGang ++ paramOut ++ paramEnv) $ do
    return ()


-- Utilities
-- ---------


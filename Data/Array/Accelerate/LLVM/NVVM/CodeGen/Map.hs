{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.NVVM.CodeGen.Map
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.NVVM.CodeGen.Map
  where

-- accelerate
import Data.Array.Accelerate.Array.Sugar                        ( Array, Elt )
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Type

import Data.Array.Accelerate.LLVM.NVVM.Target                   ( NVVM )
import Data.Array.Accelerate.LLVM.NVVM.CodeGen.Base


-- Apply a unary function to each element of an array. Each thread processes
-- multiple elements, striding the array by the grid size.
--
mkMap :: forall t aenv sh a b. Elt b
      => NVVM
      -> Gamma aenv
      -> IRFun1    aenv (a -> b)
      -> IRDelayed aenv (Array sh a)
      -> CodeGen [Kernel t aenv (Array sh b)]
mkMap _nvvm aenv apply IRDelayed{..} =
  let
      arrOut      = arrayData  (undefined::Array sh b) "out"
      paramOut    = arrayParam (undefined::Array sh b) "out"
      paramEnv    = envParam aenv
  in
  makeKernel "map" (paramOut ++ paramEnv) $ do

    loop      <- newBlock "loop.top"
    exit      <- newBlock "loop.exit"

    -- setup main loop
    -- ---------------
    --
    -- Threads process multiple elements, striding by array by the grid size.
    -- This gives optimal memory coalescing (at least for the case when
    -- delayedLinearIndex is direct indexing) and amortises the cost of
    -- launching the thread block.
    --
    n           <- shapeSize (undefined::Array sh b) "out"
    step        <- gridSize

    c           <- lt int32 threadIdx n
    top         <- cbr c loop exit

    -- main loop
    -- ---------
    setBlock loop
    indv        <- freshName
    let i        = local indv

    xs          <- delayedLinearIndex [i]
    ys          <- apply xs
    writeArray arrOut i ys

    i'          <- add int32 i step
    c'          <- lt int32 i' n
    bot         <- cbr c' loop exit
    _           <- phi loop indv (typeOf (int32 :: IntegralType Int32)) [(i',bot), (threadIdx,top)]

    setBlock exit
    return_


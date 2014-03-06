{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.NVVM.CodeGen.Transform
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.NVVM.CodeGen.Transform
  where

-- accelerate
import Data.Array.Accelerate.Array.Sugar                        ( Array, Shape, Elt )
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

-- standard library
import Prelude                                                  hiding ( fromIntegral )


-- A combination map/backpermute, where the index and value transformations have
-- been separated
--
mkTransform
    :: forall t aenv sh sh' a b. (Shape sh, Shape sh', Elt a, Elt b)
    => NVVM
    -> Gamma aenv
    -> IRFun1    aenv (sh' -> sh)
    -> IRFun1    aenv (a -> b)
    -> IRDelayed aenv (Array sh a)
    -> CodeGen [Kernel t aenv (Array sh' b)]
mkTransform _dev aenv permute apply IRDelayed{..} =
  let
      arrOut                      = arrayData  (undefined::Array sh' b) "out"
      shOut                       = arrayShape (undefined::Array sh' b) "out"
      paramOut                    = arrayParam (undefined::Array sh' b) "out"
      paramEnv                    = envParam aenv
  in
  makeKernel "transform" (paramOut ++ paramEnv) $ do

    loop        <- newBlock "loop.top"
    exit        <- newBlock "loop.exit"

    -- loop header
    -- -----
    n           <- shapeSize (undefined::Array sh b) "out"
    step        <- gridSize
    tid         <- threadIdx
    c           <- lt int32 tid n
    top         <- cbr c loop exit

    -- Main loop
    -- ---------
    setBlock loop
    indv        <- freshName
    let i       =  local indv
    ii          <- fromIntegral int32 int i             -- loop counter is i32, calculation is in Int
    ix          <- indexOfInt (map local shOut) ii      -- convert to multidimensional index
    ix'         <- permute ix                           -- apply backwards index permutation
    xs          <- delayedIndex ix'                     -- get element
    ys          <- apply xs                             -- apply function from input array
    writeArray arrOut i ys

    i'          <- add int32 i step
    c'          <- lt int32 i' n
    bot         <- cbr c' loop exit
    _           <- phi loop indv (typeOf (int32 :: IntegralType Int32)) [(i',bot), (tid,top)]

    setBlock exit
    return_



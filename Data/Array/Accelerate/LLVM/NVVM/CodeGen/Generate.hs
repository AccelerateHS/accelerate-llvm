{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.NVVM.CodeGen.Generate
-- Copyright   : [2013] Trevor L. McDonell, Sean Lee, Vinod Grover
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.NVVM.CodeGen.Generate
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

import Data.Array.Accelerate.LLVM.NVVM.Target
import Data.Array.Accelerate.LLVM.NVVM.CodeGen.Base


-- Construct a new array by applying a function to each index. Each thread
-- processes multiple adjacent elements.
--
mkGenerate
    :: forall arch aenv sh e. (Shape sh, Elt e)
    => NVVM
    -> Gamma aenv
    -> IRFun1 aenv (sh -> e)
    -> CodeGen [Kernel arch aenv (Array sh e)]
mkGenerate _dev aenv apply =
  let
      arrOut                      = arrayData  (undefined::Array sh e) "out"
      shOut                       = arrayShape (undefined::Array sh e) "out"
      paramOut                    = arrayParam (undefined::Array sh e) "out"
      paramEnv                    = envParam aenv
  in
  makeKernel "generate" (paramOut ++ paramEnv) $ do

    loop        <- newBlock "loop.top"
    exit        <- newBlock "loop.exit"

    -- loop header
    -- -----------
    n           <- shapeSize (undefined::Array sh b) "out"
    step        <- gridSize
    c           <- lt int32 threadIdx n
    top         <- cbr c loop exit

    -- main loop
    -- ---------
    setBlock loop
    indv        <- freshName                            -- induction variable
    let i        = local indv
    ix          <- indexOfInt (map local shOut) i       -- convert to multidimensional index
    r           <- apply ix                             -- apply generator function
    writeArray arrOut i r                               -- write result

    i'          <- add int32 i step
    c'          <- lt int32 i' n
    bot         <- cbr c' loop exit
    _           <- phi loop indv (typeOf (int32 :: IntegralType Int32)) [(i',bot), (threadIdx,top)]

    setBlock exit
    return_


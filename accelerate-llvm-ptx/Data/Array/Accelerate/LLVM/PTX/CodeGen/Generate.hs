{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.CodeGen.Generate
-- Copyright   : [2014..2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.CodeGen.Generate
  where

import Prelude                                                  hiding ( fromIntegral )

-- accelerate
import Data.Array.Accelerate.Array.Sugar                        ( Array, Shape, Elt )
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
import Data.Array.Accelerate.LLVM.CodeGen.Array
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar

import Data.Array.Accelerate.LLVM.PTX.CodeGen.Base
import Data.Array.Accelerate.LLVM.PTX.CodeGen.Loop
import Data.Array.Accelerate.LLVM.PTX.Target                    ( PTX )


-- Construct a new array by applying a function to each index. Each thread
-- processes multiple adjacent elements.
--
mkGenerate
    :: forall aenv sh e. (Shape sh, Elt e)
    => PTX
    -> Gamma aenv
    -> IRFun1 PTX aenv (sh -> e)
    -> CodeGen (IROpenAcc PTX aenv (Array sh e))
mkGenerate ptx aenv apply =
  let
      (start, end, paramGang)   = gangParam
      (arrOut, paramOut)        = mutableArray ("out" :: Name (Array sh e))
      paramEnv                  = envParam aenv
  in
  makeOpenAcc ptx "generate" (paramGang ++ paramOut ++ paramEnv) $ do

    imapFromTo start end $ \i -> do
      i' <- fromIntegral integralType numType i         -- loop counter is Int32
      ix <- indexOfInt (irArrayShape arrOut) i'         -- convert to multidimensional index
      r  <- app1 apply ix                               -- apply generator function
      writeArray arrOut i' r                            -- store result

    return_



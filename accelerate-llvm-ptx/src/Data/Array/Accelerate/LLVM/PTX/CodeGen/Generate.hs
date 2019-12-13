{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.CodeGen.Generate
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.CodeGen.Generate
  where

import Prelude                                                  hiding ( fromIntegral )

-- accelerate
import Data.Array.Accelerate.Array.Sugar                        ( Array, Shape, Elt )

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
    => Gamma aenv
    -> IRFun1  PTX aenv (sh -> e)
    -> CodeGen PTX      (IROpenAcc PTX aenv (Array sh e))
mkGenerate aenv apply =
  let
      (arrOut, paramOut)  = mutableArray ("out" :: Name (Array sh e))
      paramEnv            = envParam aenv
  in
  makeOpenAcc "generate" (paramOut ++ paramEnv) $ do

    start <- return (lift 0)
    end   <- shapeSize (irArrayShape arrOut)

    imapFromTo start end $ \i -> do
      ix <- indexOfInt (irArrayShape arrOut) i          -- convert to multidimensional index
      r  <- app1 apply ix                               -- apply generator function
      writeArray arrOut i r                             -- store result

    return_


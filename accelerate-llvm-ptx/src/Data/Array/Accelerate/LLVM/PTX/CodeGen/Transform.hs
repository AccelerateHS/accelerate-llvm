{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.PTX.CodeGen.Transform
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.PTX.CodeGen.Transform
  where

-- accelerate
import Data.Array.Accelerate.Representation.Array
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


-- Apply a unary function to each element of an array. Each thread processes
-- multiple elements, striding the array by the grid size.
--
mkTransform
    :: Gamma       aenv
    -> ArrayR (Array sh  a)
    -> ArrayR (Array sh' b)
    -> IRFun1  PTX aenv (sh' -> sh)
    -> IRFun1  PTX aenv (a -> b)
    -> CodeGen PTX      (IROpenAcc PTX aenv (Array sh' b))
mkTransform aenv repr@(ArrayR shr _) repr'@(ArrayR shr' _) p f =
  let
      (arrOut, paramOut)  = mutableArray repr' "out"
      (arrIn,  paramIn)   = mutableArray repr  "in"
      paramEnv            = envParam aenv
  in
  makeOpenAcc "transform" (paramOut ++ paramIn ++ paramEnv) $ do

    let start = liftInt 0
    end   <- shapeSize shr' (irArrayShape arrOut)

    imapFromTo start end $ \i' -> do
      ix' <- indexOfInt shr' (irArrayShape arrOut) i'
      ix  <- app1 p ix'
      i   <- intOfIndex shr  (irArrayShape arrIn) ix
      a   <- readArray TypeInt arrIn i
      b   <- app1 f a
      writeArray TypeInt arrOut i' b

    return_


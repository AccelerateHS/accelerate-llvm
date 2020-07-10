{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.CodeGen.Transform
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.CodeGen.Transform
  where

-- accelerate
import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Array
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.Compile.Cache

import Data.Array.Accelerate.LLVM.Native.Target                     ( Native )
import Data.Array.Accelerate.LLVM.Native.CodeGen.Base
import Data.Array.Accelerate.LLVM.Native.CodeGen.Loop


-- Hybrid map/backpermute operation
--
mkTransform
    :: UID
    -> Gamma aenv
    -> ArrayR (Array sh  a)
    -> ArrayR (Array sh' b)
    -> IRFun1  Native aenv (sh' -> sh)
    -> IRFun1  Native aenv (a -> b)
    -> CodeGen Native      (IROpenAcc Native aenv (Array sh' b))
mkTransform uid aenv reprIn reprOut p f =
  let
      (start, end, paramGang)   = gangParam (arrayRshape reprOut)
      (arrIn,  paramIn)         = mutableArray reprIn  "in"
      (arrOut, paramOut)        = mutableArray reprOut "out"
      paramEnv                  = envParam aenv
      shIn                      = irArrayShape arrIn
      shOut                     = irArrayShape arrOut
  in
  makeOpenAcc uid "transform" (paramGang ++ paramOut ++ paramIn ++ paramEnv) $ do

    imapNestFromTo (arrayRshape reprOut) start end shOut $ \ix' i' -> do
      ix  <- app1 p ix'
      i   <- intOfIndex (arrayRshape reprIn) shIn ix
      a   <- readArray TypeInt arrIn i
      b   <- app1 f a
      writeArray TypeInt arrOut i' b

    return_


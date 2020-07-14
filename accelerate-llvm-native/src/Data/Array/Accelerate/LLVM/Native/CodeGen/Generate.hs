{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.CodeGen.Generate
-- Copyright   : [2014..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.CodeGen.Generate
  where

import Data.Array.Accelerate.Representation.Array
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Array
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import Data.Array.Accelerate.LLVM.Compile.Cache

import Data.Array.Accelerate.LLVM.Native.Target                 ( Native )
import Data.Array.Accelerate.LLVM.Native.CodeGen.Base
import Data.Array.Accelerate.LLVM.Native.CodeGen.Loop


-- Construct a new array by applying a function to each index. Each thread
-- processes multiple adjacent elements.
--
mkGenerate
    :: UID
    -> Gamma aenv
    -> ArrayR (Array sh e)
    -> IRFun1  Native aenv (sh -> e)
    -> CodeGen Native      (IROpenAcc Native aenv (Array sh e))
mkGenerate uid aenv repr apply =
  let
      (start, end, paramGang)   = gangParam (arrayRshape repr)
      (arrOut, paramOut)        = mutableArray repr "out"
      paramEnv                  = envParam aenv
      shOut                     = irArrayShape arrOut
  in
  makeOpenAcc uid "generate" (paramGang ++ paramOut ++ paramEnv) $ do

    imapNestFromTo (arrayRshape repr) start end shOut $ \ix i -> do
      r <- app1 apply ix                        -- apply generator function
      writeArray TypeInt arrOut i r             -- store result

    return_


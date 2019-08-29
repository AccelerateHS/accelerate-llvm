{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Native.CodeGen.Generate
-- Copyright   : [2014..2019] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Native.CodeGen.Generate
  where

-- accelerate
import Data.Array.Accelerate.Array.Sugar                        ( Array, Shape, Elt )

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
    :: forall aenv sh e. (Shape sh, Elt e)
    => UID
    -> Gamma aenv
    -> IRFun1  Native aenv (sh -> e)
    -> CodeGen Native      (IROpenAcc Native aenv (Array sh e))
mkGenerate uid aenv apply =
  let
      (start, end, paramGang)   = gangParam    @sh
      (arrOut, paramOut)        = mutableArray @sh "out"
      paramEnv                  = envParam aenv
      shOut                     = irArrayShape arrOut
  in
  makeOpenAcc uid "generate" (paramGang ++ paramOut ++ paramEnv) $ do

    imapNestFromTo start end shOut $ \ix i -> do
      r <- app1 apply ix                        -- apply generator function
      writeArray arrOut i r                     -- store result

    return_


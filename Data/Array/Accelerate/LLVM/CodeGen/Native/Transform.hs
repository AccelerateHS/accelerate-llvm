{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Native.Transform
-- Copyright   :
-- License     :
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@nvidia.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Native.Transform
  where

-- llvm-general
import LLVM.General.AST
import LLVM.General.AST.Global

-- accelerate
import Data.Array.Accelerate.Array.Sugar                        ( Array, Shape, Elt )
import Data.Array.Accelerate.Type

import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic
import Data.Array.Accelerate.LLVM.CodeGen.Base
import Data.Array.Accelerate.LLVM.CodeGen.Constant
import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.Exp
import Data.Array.Accelerate.LLVM.CodeGen.Module
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.LLVM.CodeGen.Type


-- A combination map/backpermute, where the index and value transformations have
-- been separated
--
mkTransform
    :: forall t aenv sh sh' a b. (Shape sh, Shape sh', Elt a, Elt b)
    => Gamma aenv
    -> IRFun1    aenv (sh' -> sh)
    -> IRFun1    aenv (a -> b)
    -> IRDelayed aenv (Array sh a)
    -> CodeGen [Kernel t aenv (Array sh' b)]
mkTransform aenv permute apply IRDelayed{..} = do
  code  <- runBody
  return [ Kernel $ functionDefaults
             { returnType  = VoidType
             , name        = "transform"
             , parameters  = (paramGang ++ paramOut ++ paramEnv, False)
             , basicBlocks = code
             } ]
  where
    arrOut                      = arrayData  (undefined::Array sh' b) "out"
    shOut                       = arrayShape (undefined::Array sh' b) "out"
    paramOut                    = arrayParam (undefined::Array sh' b) "out"
    paramEnv                    = envParam aenv
    (start, end, paramGang)     = gangParam

    runBody :: CodeGen [BasicBlock]
    runBody = do
      loop      <- newBlock "loop.top"
      exit      <- newBlock "loop.exit"

      -- Entry
      -- -----
      c         <- lt int start end
      top       <- cbr c loop exit

      -- Main loop
      -- ---------
      setBlock loop
      indv <- freshName
      let i = local indv
      ix        <- indexOfInt (map local shOut) i       -- convert to multidimensional index
      ix'       <- permute ix                           -- apply backwards index permutation
      xs        <- delayedIndex ix'                     -- get element
      ys        <- apply xs                             -- apply function from input array
      writeArray arrOut i ys

      i'        <- add int i (constOp $ num int 1)
      c'        <- eq int i' end
      bot       <- cbr c' exit loop
      _         <- phi loop indv (typeOf (int :: IntegralType Int)) [(i', bot), (start,top)]

      setBlock exit
      return_
      --
      >> createBlocks


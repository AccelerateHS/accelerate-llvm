{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_HADDOCK hide #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.CodeGen.Sugar
-- Copyright   : [2015..2020] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <trevor.mcdonell@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.CodeGen.Sugar (

  IRExp, MIRExp, IRFun1, IRFun2,
  IROpenExp, IROpenFun1(..), IROpenFun2(..),
  IROpenAcc(..),
  IRDelayed(..), MIRDelayed(..),

  IRArray(..),

) where

import LLVM.AST.Type.AddrSpace
import LLVM.AST.Type.Instruction.Volatile

import Data.Array.Accelerate.Representation.Array

import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.CodeGen.Module
import {-# SOURCE #-} Data.Array.Accelerate.LLVM.CodeGen.Monad


-- Scalar expressions
-- ------------------

-- | LLVM IR is in single static assignment, so we need to be able to generate
-- fresh names for each application of a scalar function or expression.
--
type IRExp     arch     aenv t = IROpenExp arch () aenv t
type MIRExp    arch     aenv t = Maybe (IRExp arch aenv t)
type IROpenExp arch env aenv t = CodeGen arch (Operands t)

type IRFun1 arch aenv t = IROpenFun1 arch () aenv t
type IRFun2 arch aenv t = IROpenFun2 arch () aenv t

data IROpenFun1 arch env aenv t where
  IRFun1 :: { app1 :: Operands a -> IROpenExp arch (env,a) aenv b }
         -> IROpenFun1 arch env aenv (a -> b)

data IROpenFun2 arch env aenv t where
  IRFun2 :: { app2 :: Operands a -> Operands b -> IROpenExp arch ((env,a),b) aenv c }
         -> IROpenFun2 arch env aenv (a -> b -> c)


-- Arrays
-- ------

data IROpenAcc arch aenv arrs where
  IROpenAcc :: [Kernel arch aenv arrs]
            -> IROpenAcc arch aenv arrs

data MIRDelayed arch aenv a
  = IRDelayedJust (IRDelayed arch aenv a)
  | IRDelayedNothing (ArrayR a)

data IRDelayed arch aenv a where
  IRDelayed :: { delayedRepr        :: ArrayR (Array sh e)
               , delayedExtent      :: IRExp  arch aenv sh
               , delayedIndex       :: IRFun1 arch aenv (sh -> e)
               , delayedLinearIndex :: IRFun1 arch aenv (Int -> e)
               }
            -> IRDelayed arch aenv (Array sh e)

data IRArray a where
  IRArray :: { irArrayRepr        :: ArrayR (Array sh e)
             , irArrayShape       :: Operands sh        -- Array extent
             , irArrayData        :: Operands e         -- Array payloads (should really be 'Ptr e')
             , irArrayAddrSpace   :: AddrSpace
             , irArrayVolatility  :: Volatility
             }
          -> IRArray (Array sh e)

